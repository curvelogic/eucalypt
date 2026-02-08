//! Streaming file import infrastructure
//!
//! Provides `StreamProducer` trait and a global handle table for
//! registering producers at import time and accessing them at runtime
//! via the `__STREAM_NEXT` intrinsic.

use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use super::syntax::StgSyn;

/// A producer that yields values lazily from an IO source.
///
/// Each call to `next()` advances the underlying source (file cursor,
/// CSV parser, etc.) and returns the next value as pre-compiled STG
/// syntax, or `None` when the source is exhausted.
pub trait StreamProducer {
    /// Produce the next value as STG syntax, or `None` if exhausted.
    fn next(&mut self) -> Option<Rc<StgSyn>>;
}

/// A reference-counted, interiorly-mutable stream producer handle.
pub type StreamHandle = Rc<RefCell<Box<dyn StreamProducer>>>;

/// Table mapping numeric handles to stream producers.
///
/// Producers are registered at import time and looked up at runtime
/// by the `__STREAM_NEXT` intrinsic.
pub struct StreamTable {
    handles: HashMap<u32, StreamHandle>,
    next_id: u32,
}

impl Default for StreamTable {
    fn default() -> Self {
        StreamTable {
            handles: HashMap::new(),
            next_id: 1,
        }
    }
}

impl StreamTable {
    /// Register a producer and return its handle ID.
    pub fn register(&mut self, producer: Box<dyn StreamProducer>) -> u32 {
        let id = self.next_id;
        self.next_id += 1;
        self.handles.insert(id, Rc::new(RefCell::new(producer)));
        id
    }

    /// Look up a producer by handle ID.
    pub fn get(&self, handle: u32) -> Option<&StreamHandle> {
        self.handles.get(&handle)
    }
}

thread_local! {
    /// Global stream table, accessible from both import and runtime code.
    static STREAM_TABLE: RefCell<StreamTable> = RefCell::new(StreamTable::default());
}

/// Register a stream producer in the global table and return its handle ID.
pub fn register_stream(producer: Box<dyn StreamProducer>) -> u32 {
    STREAM_TABLE.with(|table| table.borrow_mut().register(producer))
}

/// Drain all remaining values from a stream producer.
///
/// Returns a vector of all STG syntax values, consuming the
/// producer to exhaustion.
pub fn stream_drain(handle: u32) -> Vec<Rc<StgSyn>> {
    STREAM_TABLE.with(|table| {
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
