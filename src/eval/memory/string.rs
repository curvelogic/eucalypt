//! Immutable heap string storage

use std::slice::from_raw_parts;

use super::{
    alloc::{ScopedAllocator, StgObject},
    array::RawArray,
};

/// UTF-8 string data stored in the heap
pub struct HeapString {
    length: usize,
    data: RawArray<u8>,
}

impl StgObject for HeapString {}

impl HeapString {
    pub fn from_str<'guard, A: ScopedAllocator<'guard>>(mem: &A, source: &str) -> Self {
        HeapString {
            length: source.len(),
            data: RawArray::with_data(mem, source.as_bytes())
                .expect("with_data: alloc failure (string)"),
        }
    }

    pub fn as_slice(&self) -> &[u8] {
        if let Some(ptr) = self.data.as_ptr() {
            unsafe { from_raw_parts(ptr, self.length) }
        } else {
            &[]
        }
    }

    pub fn as_str(&self) -> &str {
        std::str::from_utf8(self.as_slice()).expect("bad str data")
    }
}
