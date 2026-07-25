//! Lazy producer infrastructure
//!
//! Provides `LazyProducer` trait and a global handle table for
//! registering producers at import time and accessing them at runtime
//! via the `PRODUCER_NEXT` intrinsic.

use std::cell::RefCell;
use std::collections::{HashMap, HashSet};
use std::rc::Rc;

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

    /// Whether this producer is pure (same state → same output).
    /// Pure producers can safely be forked/shared in future.
    /// Import and IO producers are not pure.
    fn is_pure(&self) -> bool {
        false
    }
}

/// A reference-counted, interiorly-mutable producer handle.
pub type ProducerHandle = Rc<RefCell<Box<dyn LazyProducer>>>;

/// Table mapping numeric handles to lazy producers.
///
/// Producers are registered at import time and looked up at runtime
/// by the `PRODUCER_NEXT` intrinsic.
pub struct ProducerTable {
    handles: HashMap<u32, ProducerHandle>,
    /// Handles of registered *impure* producers not yet observed exhausted.
    /// The process-parallelism boundary refuses to fork while any of these is
    /// live (a forked worker would share the underlying fd offset, spike R2).
    live_impure: HashSet<u32>,
    next_id: u32,
}

impl Default for ProducerTable {
    fn default() -> Self {
        ProducerTable {
            handles: HashMap::new(),
            live_impure: HashSet::new(),
            next_id: 1,
        }
    }
}

impl ProducerTable {
    /// Register a producer and return its handle ID.
    pub fn register(&mut self, producer: Box<dyn LazyProducer>) -> u32 {
        let id = self.next_id;
        self.next_id += 1;
        if !producer.is_pure() {
            self.live_impure.insert(id);
        }
        self.handles.insert(id, Rc::new(RefCell::new(producer)));
        id
    }

    /// Look up a producer by handle ID.
    pub fn get(&self, handle: u32) -> Option<&ProducerHandle> {
        self.handles.get(&handle)
    }

    /// Mark a producer exhausted — no longer a hazard at the parallel boundary.
    fn mark_exhausted(&mut self, handle: u32) {
        self.live_impure.remove(&handle);
    }

    /// Whether any registered impure producer is still live (unexhausted).
    pub fn has_live_impure(&self) -> bool {
        !self.live_impure.is_empty()
    }
}

thread_local! {
    /// Global producer table, accessible from both import and runtime code.
    static PRODUCER_TABLE: RefCell<ProducerTable> = RefCell::new(ProducerTable::default());
}

/// Register a lazy producer in the global table and return its handle ID.
pub fn register_producer(producer: Box<dyn LazyProducer>) -> u32 {
    PRODUCER_TABLE.with(|table| table.borrow_mut().register(producer))
}

/// Whether any registered impure producer is still live (unexhausted).
///
/// The process-parallelism combinators consult this at the `par-*` boundary:
/// a live impure producer shares an fd offset across forked workers (spike
/// R2), so forking is refused while one is in flight.
pub fn any_live_impure_producer() -> bool {
    PRODUCER_TABLE.with(|table| table.borrow().has_live_impure())
}

/// Drain all remaining values from a producer.
///
/// Returns a vector of all STG syntax values, consuming the
/// producer to exhaustion. Stops and returns an error if the
/// producer yields `Some(Err(_))`.
pub fn producer_drain(handle: u32) -> Result<Vec<Rc<StgSyn>>, ExecutionError> {
    // Clone the handle out so the table borrow is released while we drive the
    // producer (which may itself touch the table), then re-borrow to mark the
    // now-exhausted producer.
    let producer = PRODUCER_TABLE.with(|table| table.borrow().get(handle).cloned());
    let producer = match producer {
        Some(p) => p,
        None => return Ok(Vec::new()),
    };
    let mut values = Vec::new();
    {
        let mut producer = producer.borrow_mut();
        loop {
            match producer.next() {
                Some(Ok(v)) => values.push(v),
                Some(Err(e)) => return Err(e),
                None => break,
            }
        }
    }
    PRODUCER_TABLE.with(|table| table.borrow_mut().mark_exhausted(handle));
    Ok(values)
}

/// Advance a producer by a single step.
///
/// Returns:
/// - `Some(Ok(value))` if the producer yielded an element
/// - `Some(Err(e))` if the producer encountered an error
/// - `None` if the producer is exhausted or the handle is invalid
pub fn producer_next(handle: u32) -> Option<Result<Rc<StgSyn>, ExecutionError>> {
    let producer = PRODUCER_TABLE.with(|table| table.borrow().get(handle).cloned())?;
    let result = producer.borrow_mut().next();
    if result.is_none() {
        // Exhausted: no longer a parallel-boundary hazard.
        PRODUCER_TABLE.with(|table| table.borrow_mut().mark_exhausted(handle));
    }
    result
}

#[cfg(test)]
mod tests {
    use super::*;

    /// A finite producer that yields `count` unit values then exhausts. Its
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

    #[test]
    fn impure_producer_flips_live_flag_until_drained() {
        let handle = register_producer(Box::new(CountingProducer {
            remaining: 2,
            pure: false,
        }));
        assert!(
            any_live_impure_producer(),
            "an impure producer must register as live"
        );
        // one step — still live (not yet exhausted)
        assert!(producer_next(handle).is_some());
        assert!(any_live_impure_producer(), "still live before exhaustion");
        // drain the rest — the exhausting `None` clears the live flag
        assert!(producer_next(handle).is_some());
        assert!(producer_next(handle).is_none());
        assert!(
            !any_live_impure_producer(),
            "an exhausted impure producer is no longer live"
        );
    }

    #[test]
    fn pure_producer_never_flips_live_flag() {
        let _handle = register_producer(Box::new(CountingProducer {
            remaining: 3,
            pure: true,
        }));
        assert!(
            !any_live_impure_producer(),
            "a pure producer is never a boundary hazard"
        );
    }
}
