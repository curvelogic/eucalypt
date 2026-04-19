//! Lazy producer infrastructure
//!
//! Provides `LazyProducer` trait and a global handle table for
//! registering producers at import time and accessing them at runtime
//! via the `PRODUCER_NEXT` intrinsic.

use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use super::syntax::StgSyn;

/// A lazy producer of STG values, accessed via a handle in ProducerTable.
///
/// Each call to `next()` advances the underlying source (file cursor,
/// CSV parser, etc.) and returns the next value as pre-compiled STG
/// syntax, or `None` when the source is exhausted.
pub trait LazyProducer {
    /// Produce the next value as STG syntax, or `None` if exhausted.
    fn next(&mut self) -> Option<Rc<StgSyn>>;

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
}

/// Register a lazy producer in the global table and return its handle ID.
pub fn register_producer(producer: Box<dyn LazyProducer>) -> u32 {
    PRODUCER_TABLE.with(|table| table.borrow_mut().register(producer))
}

/// Drain all remaining values from a producer.
///
/// Returns a vector of all STG syntax values, consuming the
/// producer to exhaustion.
pub fn producer_drain(handle: u32) -> Vec<Rc<StgSyn>> {
    PRODUCER_TABLE.with(|table| {
        let table = table.borrow();
        match table.get(handle) {
            Some(producer) => {
                let mut values = Vec::new();
                let mut producer = producer.borrow_mut();
                while let Some(v) = producer.next() {
                    values.push(v);
                }
                values
            }
            None => Vec::new(),
        }
    })
}

/// Advance a producer by a single step.
///
/// Returns `Some(value)` if the producer yielded an element, or
/// `None` if the producer is exhausted or the handle is invalid.
pub fn producer_next(handle: u32) -> Option<Rc<StgSyn>> {
    PRODUCER_TABLE.with(|table| {
        let table = table.borrow();
        match table.get(handle) {
            Some(producer) => producer.borrow_mut().next(),
            None => None,
        }
    })
}
