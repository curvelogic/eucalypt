//! Lazy producer infrastructure
//!
//! Provides `LazyProducer` trait and a global handle table for
//! registering producers at import time and accessing them at runtime
//! via the `PRODUCER_NEXT` intrinsic.

use std::cell::{Cell, RefCell};
use std::collections::HashMap;
use std::rc::Rc;

use crate::common::sourcemap::Smid;
use crate::eval::error::ExecutionError;

use super::syntax::StgSyn;

/// A lazy producer of STG values, accessed via a handle in ProducerTable.
///
/// Each call to `next()` advances the underlying source (file cursor,
/// CSV parser, etc.) and returns the next value as pre-compiled STG
/// syntax, `None` when the source is exhausted, or `Some(Err(_))` when
/// a read or parse error occurs.
pub trait LazyProducer {
    /// Produce the next value as STG syntax.
    ///
    /// - `Some(Ok(v))` — the next value
    /// - `None` — producer exhausted
    /// - `Some(Err(e))` — an IO or parse error; the producer should be
    ///   considered failed and not advanced further
    fn next(&mut self) -> Option<Result<Rc<StgSyn>, ExecutionError>>;

    /// Whether this producer is pure — advancing it has no effect outside
    /// this process's own memory (same state → same output).
    ///
    /// An **impure** producer owns external state, typically an open file
    /// descriptor whose offset is shared with any forked child. Advancing one
    /// inside a process-parallelism worker would silently steal input from the
    /// parent, so [`producer_next`] refuses to do it (see
    /// [`enter_parallel_worker`]).
    ///
    /// Deliberately has no default: every producer must state its purity, so
    /// that a new one cannot silently inherit the wrong answer.
    fn is_pure(&self) -> bool;
}

/// A reference-counted, interiorly-mutable producer handle.
pub type ProducerHandle = Rc<RefCell<Box<dyn LazyProducer>>>;

/// Table mapping numeric handles to lazy producers.
///
/// Producers are registered at import time and looked up at runtime
/// by the `PRODUCER_NEXT` intrinsic.
pub struct ProducerTable {
    handles: HashMap<u32, ProducerHandle>,
    next_id: u32,
}

impl Default for ProducerTable {
    fn default() -> Self {
        ProducerTable {
            handles: HashMap::new(),
            next_id: 1,
        }
    }
}

impl ProducerTable {
    /// Register a producer and return its handle ID.
    pub fn register(&mut self, producer: Box<dyn LazyProducer>) -> u32 {
        let id = self.next_id;
        self.next_id += 1;
        self.handles.insert(id, Rc::new(RefCell::new(producer)));
        id
    }

    /// Look up a producer by handle ID.
    pub fn get(&self, handle: u32) -> Option<&ProducerHandle> {
        self.handles.get(&handle)
    }
}

thread_local! {
    /// Global producer table, accessible from both import and runtime code.
    static PRODUCER_TABLE: RefCell<ProducerTable> = RefCell::new(ProducerTable::default());

    /// Set only inside a forked process-parallelism worker, for that worker's
    /// lifetime. See [`enter_parallel_worker`].
    static IN_PARALLEL_WORKER: Cell<bool> = const { Cell::new(false) };
}

/// Register a lazy producer in the global table and return its handle ID.
pub fn register_producer(producer: Box<dyn LazyProducer>) -> u32 {
    PRODUCER_TABLE.with(|table| table.borrow_mut().register(producer))
}

/// Mark this process as a forked process-parallelism worker (spike R2).
///
/// A worker inherits the parent's open file descriptors, and an impure
/// producer's fd offset is **shared** with the parent — so a worker that
/// advanced one would consume input the parent still expects to read. Rather
/// than predict that statically (registration is not use: a stream imported but
/// never touched is no hazard at all), we detect it at the point of harm: while
/// this flag is set, [`producer_next`] refuses to advance an impure producer and
/// fails the worker. The PP driver treats any worker failure as a signal to
/// re-run **sequentially in the parent**, where the producer is consumed
/// correctly — so the user sees the ordinary `map` semantics, not an error.
///
/// The flag is only ever set in a child that leaves via `_exit`, so it cannot
/// leak into any later evaluation in the parent — unlike process-lifetime
/// state, this is safe in long-lived hosts (the LSP server, the WASM API) that
/// evaluate many units per process.
pub fn enter_parallel_worker() {
    IN_PARALLEL_WORKER.with(|f| f.set(true));
}

/// Whether this process is a forked process-parallelism worker.
pub fn in_parallel_worker() -> bool {
    IN_PARALLEL_WORKER.with(|f| f.get())
}

/// The error a worker fails with when it tries to advance an impure producer.
/// It is diagnostic only: the parent discards it and re-runs sequentially.
fn impure_producer_in_worker() -> ExecutionError {
    ExecutionError::Panic(
        Smid::default(),
        "a parallel worker may not advance an impure streaming producer".to_string(),
    )
}

/// Drain all remaining values from a producer.
///
/// Returns a vector of all STG syntax values, consuming the
/// producer to exhaustion. Stops and returns an error if the
/// producer yields `Some(Err(_))`.
pub fn producer_drain(handle: u32) -> Result<Vec<Rc<StgSyn>>, ExecutionError> {
    PRODUCER_TABLE.with(|table| {
        let table = table.borrow();
        match table.get(handle) {
            Some(producer) => {
                let mut values = Vec::new();
                let mut producer = producer.borrow_mut();
                if in_parallel_worker() && !producer.is_pure() {
                    return Err(impure_producer_in_worker());
                }
                loop {
                    match producer.next() {
                        Some(Ok(v)) => values.push(v),
                        Some(Err(e)) => return Err(e),
                        None => break,
                    }
                }
                Ok(values)
            }
            None => Ok(Vec::new()),
        }
    })
}

/// Advance a producer by a single step.
///
/// Returns:
/// - `Some(Ok(value))` if the producer yielded an element
/// - `Some(Err(e))` if the producer encountered an error
/// - `None` if the producer is exhausted or the handle is invalid
pub fn producer_next(handle: u32) -> Option<Result<Rc<StgSyn>, ExecutionError>> {
    PRODUCER_TABLE.with(|table| {
        let table = table.borrow();
        match table.get(handle) {
            Some(producer) => {
                let mut producer = producer.borrow_mut();
                if in_parallel_worker() && !producer.is_pure() {
                    return Some(Err(impure_producer_in_worker()));
                }
                producer.next()
            }
            None => None,
        }
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    /// A finite producer yielding `remaining` values then exhausting. Its
    /// purity is configurable so we can exercise both boundary behaviours.
    struct CountingProducer {
        remaining: usize,
        pure: bool,
    }

    impl LazyProducer for CountingProducer {
        fn next(&mut self) -> Option<Result<Rc<StgSyn>, ExecutionError>> {
            if self.remaining == 0 {
                None
            } else {
                self.remaining -= 1;
                use crate::eval::stg::syntax::dsl;
                Some(Ok(dsl::atom(dsl::num(0))))
            }
        }

        fn is_pure(&self) -> bool {
            self.pure
        }
    }

    /// Restore the worker flag whatever the test does, so one test cannot
    /// poison the rest of the (thread-shared) suite.
    struct WorkerFlagGuard;

    impl WorkerFlagGuard {
        fn enter() -> Self {
            enter_parallel_worker();
            WorkerFlagGuard
        }
    }

    impl Drop for WorkerFlagGuard {
        fn drop(&mut self) {
            IN_PARALLEL_WORKER.with(|f| f.set(false));
        }
    }

    #[test]
    fn outside_a_worker_an_impure_producer_advances_normally() {
        let handle = register_producer(Box::new(CountingProducer {
            remaining: 2,
            pure: false,
        }));
        assert!(!in_parallel_worker());
        assert!(matches!(producer_next(handle), Some(Ok(_))));
        assert!(matches!(producer_next(handle), Some(Ok(_))));
        assert!(producer_next(handle).is_none());
    }

    #[test]
    fn a_worker_refuses_to_advance_an_impure_producer() {
        let handle = register_producer(Box::new(CountingProducer {
            remaining: 2,
            pure: false,
        }));
        let _guard = WorkerFlagGuard::enter();
        assert!(
            matches!(producer_next(handle), Some(Err(_))),
            "a worker must refuse an impure producer rather than steal the parent's fd offset"
        );
        assert!(producer_drain(handle).is_err());
    }

    #[test]
    fn a_worker_may_advance_a_pure_producer() {
        let handle = register_producer(Box::new(CountingProducer {
            remaining: 2,
            pure: true,
        }));
        let _guard = WorkerFlagGuard::enter();
        assert!(matches!(producer_next(handle), Some(Ok(_))));
    }

    #[test]
    fn refusal_does_not_advance_the_producer() {
        let handle = register_producer(Box::new(CountingProducer {
            remaining: 1,
            pure: false,
        }));
        {
            let _guard = WorkerFlagGuard::enter();
            assert!(matches!(producer_next(handle), Some(Err(_))));
        }
        // The parent's sequential re-run still sees the untouched element.
        assert!(matches!(producer_next(handle), Some(Ok(_))));
        assert!(producer_next(handle).is_none());
    }
}
