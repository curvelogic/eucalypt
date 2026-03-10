//! Immutable heap string storage

use std::slice::from_raw_parts;

use super::{
    alloc::{ScopedAllocator, StgObject},
    array::RawArray,
    collect::{CollectorHeapView, CollectorScope, GcScannable, OpaqueHeapBytes, ScanPtr},
};

/// UTF-8 string data stored in the heap
pub struct HeapString {
    length: usize,
    data: RawArray<u8>,
}

impl StgObject for HeapString {}

/// `HeapString` holds a separate heap allocation for the backing byte
/// array (`RawArray<u8>`).  Without scanning, that allocation is
/// invisible to the GC: the `HeapString` struct is marked but its
/// backing bytes are never traced, so the sweep can reclaim them while
/// the `HeapString` still holds a dangling pointer.
///
/// This `GcScannable` implementation fixes the bug by:
/// - `scan`: marking the backing byte allocation and pushing it as a
///   heap object so the evacuating collector can move it.
/// - `scan_and_update`: rewriting the backing pointer when the byte
///   array has been evacuated to a new location.
impl GcScannable for HeapString {
    fn scan<'a>(
        &'a self,
        scope: &'a dyn CollectorScope,
        marker: &mut CollectorHeapView<'a>,
        out: &mut Vec<ScanPtr<'a>>,
    ) {
        if let Some(ptr) = self.data.allocated_data() {
            if marker.mark_raw_bytes(ptr) {
                out.push(ScanPtr::from_non_null(scope, ptr.cast::<OpaqueHeapBytes>()));
            }
        }
    }

    fn scan_and_update(&mut self, heap: &CollectorHeapView<'_>) {
        if let Some(old_ptr) = self.data.allocated_data() {
            if let Some(new_ptr) = heap.forwarded_to(old_ptr) {
                // SAFETY: new_ptr is a valid evacuated copy of the same
                // backing byte allocation.
                unsafe { self.data.set_backing_ptr(new_ptr.cast()) };
            }
        }
    }
}

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
            // SAFETY: The slice construction is valid because:
            // - `ptr` points to valid heap-allocated memory (from RawArray::with_data)
            // - `self.length` was set from the source string length at construction
            // - The data was copied from valid UTF-8 bytes (source.as_bytes())
            // - HeapString owns this memory via RawArray for the slice lifetime
            unsafe { from_raw_parts(ptr, self.length) }
        } else {
            &[]
        }
    }

    pub fn as_str(&self) -> &str {
        std::str::from_utf8(self.as_slice()).expect("bad str data")
    }
}
