//! A simple vector-like array in our allocator
//!
//! Environments, closures and STG syntax items all require
//! collections allocated in the heap. `Array` is a very crude vector
//! with storage allocated in the STG heap. It panics on allocation
//! failure (as Vec does...)

use std::{cmp::min, mem::size_of, slice::from_raw_parts};

use crate::eval::error::ExecutionError;

use super::{
    alloc::{MutatorScope, ScopedAllocator},
    syntax::RefPtr,
};

/// Simple growable backing array
///
/// Provides memory for [`Array<T>`]
#[derive(Debug)]
pub struct RawArray<T: Sized> {
    capacity: usize,
    ptr: Option<RefPtr<T>>,
}

impl<T: Sized> Clone for RawArray<T> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<T: Sized> Copy for RawArray<T> {}

impl<T: Sized> Default for RawArray<T> {
    fn default() -> Self {
        RawArray::new()
    }
}

impl<T: Sized> RawArray<T> {
    /// New empty backing array
    pub fn new() -> Self {
        RawArray {
            capacity: 0,
            ptr: None,
        }
    }

    /// Construct with capacity
    pub fn with_capacity<'guard, A: ScopedAllocator<'guard>>(
        mem: &A,
        capacity: usize,
    ) -> Result<RawArray<T>, ExecutionError> {
        Ok(RawArray {
            capacity,
            ptr: Self::alloc(mem, capacity)?,
        })
    }

    /// Initialise by copying from a slice
    pub fn with_data<'guard, A: ScopedAllocator<'guard>>(
        mem: &A,
        data: &[T],
    ) -> Result<RawArray<T>, ExecutionError> {
        let new_ptr = Self::alloc(mem, data.len())?;
        if let Some(new) = new_ptr {
            // SAFETY: The copy is valid because:
            // - `data.as_ptr()` points to valid, initialised memory
            // - `new.as_ptr()` points to freshly allocated heap memory
            // - Both regions have size `data.len() * size_of::<T>()` bytes
            // - The regions do not overlap (new is freshly allocated)
            // - T is Sized, so element size is well-defined
            unsafe {
                std::ptr::copy_nonoverlapping(data.as_ptr(), new.as_ptr(), data.len());
            }
        }

        Ok(RawArray {
            capacity: data.len(),
            ptr: new_ptr,
        })
    }

    /// Resize to new capacity, copying data if required
    pub fn resize<'guard, A: ScopedAllocator<'guard>>(
        &mut self,
        mem: &A,
        new_capacity: usize,
    ) -> Result<(), ExecutionError> {
        let new_ptr = Self::alloc(mem, new_capacity)?;

        if let Some(old) = self.ptr {
            if let Some(new) = new_ptr {
                // SAFETY: The copy is valid because:
                // - `old.as_ptr()` points to previously allocated heap memory
                // - `new.as_ptr()` points to freshly allocated heap memory
                // - Copy count is min(old_capacity, new_capacity) elements
                // - The regions do not overlap (new is freshly allocated)
                // - Old memory remains valid (GC-managed, not freed here)
                unsafe {
                    std::ptr::copy_nonoverlapping(
                        old.as_ptr(),
                        new.as_ptr(),
                        min(self.capacity, new_capacity),
                    );
                }
            }
        }

        self.ptr = new_ptr;
        self.capacity = new_capacity;
        Ok(())
    }

    /// Current capacity
    pub fn capacity(&self) -> usize {
        self.capacity
    }

    fn alloc<'guard, A: ScopedAllocator<'guard>>(
        mem: &A,
        capacity: usize,
    ) -> Result<Option<RefPtr<T>>, ExecutionError> {
        if capacity == 0 {
            Ok(None)
        } else {
            let capacity_bytes = capacity
                .checked_mul(size_of::<T>())
                .ok_or(ExecutionError::AllocationError)?;
            Ok(RefPtr::new(
                mem.alloc_bytes(capacity_bytes)?.as_ptr() as *mut T
            ))
        }
    }

    /// Pointer to the backing array
    pub fn as_ptr(&self) -> Option<*const T> {
        match self.ptr {
            Some(ptr) => Some(ptr.as_ptr()),
            None => None,
        }
    }
}

/// Array
///
/// A simple vector-like array with very limited functionality.
/// Panics on allocation failure.
#[derive(Clone, Debug)]
pub struct Array<T: Sized + Clone> {
    /// Number of items in the array
    length: usize,
    /// Underlying data
    data: RawArray<T>,
}

impl<T: Sized + Clone> Default for Array<T> {
    /// Construct empty array
    fn default() -> Self {
        Array {
            length: 0,
            data: RawArray::new(),
        }
    }
}

impl<T: Sized + Clone> Array<T> {
    /// Construct an Array with a known capacity
    pub fn with_capacity<'guard, A: ScopedAllocator<'guard>>(mem: &A, capacity: usize) -> Self {
        Array {
            length: 0,
            data: RawArray::with_capacity(mem, capacity)
                .expect("with_capacity: allocation failure"),
        }
    }

    /// Construct an Array with a known capacity
    pub fn from_slice<'guard, A: ScopedAllocator<'guard>>(mem: &A, slice: &[T]) -> Self {
        Array {
            length: slice.len(),
            data: RawArray::with_data(mem, slice).expect("from_slice: allocation failure"),
        }
    }

    /// Length of the array
    pub fn len(&self) -> usize {
        self.length
    }

    /// True if the array is empty
    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    /// Add an item at the end
    pub fn push<'guard, A: ScopedAllocator<'guard>>(&mut self, mem: &A, item: T) {
        if self.length == self.data.capacity {
            self.data
                .resize(mem, Self::default_array_growth(self.data.capacity))
                .expect("allocation failure");
        }

        self.length += 1;
        self.write(self.length - 1, item);
    }

    /// Remove and return the final item (if any)
    pub fn pop(&mut self) -> Option<T> {
        if self.length > 0 {
            let len = self.length - 1;
            let ret = self.read(len);
            self.length = len;
            Some(ret)
        } else {
            None
        }
    }

    pub fn pop_n<G, V, F>(&mut self, _scope: &G, _n: usize, _consumer: F) -> V
    where
        F: Fn(&[T]) -> V,
        G: MutatorScope,
    {
        todo!();
    }

    /// Return the final item
    pub fn top(&self) -> Option<T> {
        if self.length > 0 {
            Some(self.read(self.length - 1))
        } else {
            None
        }
    }

    /// Return item at index
    pub fn get(&self, index: usize) -> Option<T> {
        if self.length > index {
            Some(self.read(index))
        } else {
            None
        }
    }

    /// Set item at index
    pub fn set(&mut self, index: usize, item: T) -> Result<(), ExecutionError> {
        if index >= self.length {
            return Err(ExecutionError::ArrayBoundsError {
                index,
                length: self.length,
            });
        }
        self.write(index, item);
        Ok(())
    }

    /// Set item at index without bounds checking
    ///
    /// # Safety
    ///
    /// Caller must ensure index < self.length to avoid undefined behavior.
    /// This method is intended for performance-critical paths where bounds
    /// are guaranteed by construction (e.g., pre-allocated arrays).
    pub(crate) unsafe fn set_unchecked(&mut self, index: usize, item: T) {
        debug_assert!(
            index < self.length,
            "Array bounds violation: index {} >= length {}",
            index,
            self.length
        );
        self.write(index, item);
    }

    /// As immutable slice
    pub fn as_slice(&self) -> &[T] {
        if let Some(ptr) = self.data.as_ptr() {
            // SAFETY: The slice construction is valid because:
            // - `ptr` points to heap-allocated memory (from RawArray)
            // - `self.length` elements have been initialised (via push/from_slice)
            // - The Array owns this memory for the lifetime of the borrow
            // - T is Sized and properly aligned in the allocation
            unsafe { from_raw_parts(ptr, self.length) }
        } else {
            &[]
        }
    }

    /// As mutable slice
    pub fn as_mut_slice(&mut self) -> &mut [T] {
        if let Some(ptr) = self.data.as_ptr() {
            // SAFETY: Same as as_slice(), plus we have &mut self so
            // exclusive access is guaranteed.
            unsafe { std::slice::from_raw_parts_mut(ptr as *mut T, self.length) }
        } else {
            &mut []
        }
    }

    /// Read only iterator
    pub fn iter(&self) -> std::slice::Iter<'_, T> {
        debug_assert_ne!(self.length, usize::MAX);
        debug_assert!(self.length < u32::MAX as usize);
        self.as_slice().iter()
    }

    /// Mutable iterator
    pub fn iter_mut(&mut self) -> std::slice::IterMut<'_, T> {
        debug_assert_ne!(self.length, usize::MAX);
        debug_assert!(self.length < u32::MAX as usize);
        self.as_mut_slice().iter_mut()
    }

    /// Return pointer for index
    fn get_offset(&self, index: usize) -> Option<*mut T> {
        if index < self.length {
            self.data
                .as_ptr()
                // SAFETY: Pointer arithmetic is valid because:
                // - `index < self.length` is checked above
                // - `self.length <= self.data.capacity` (Array invariant)
                // - The underlying allocation has capacity for `capacity` elements
                // - T is Sized, so offset calculation is well-defined
                .map(|p| unsafe { p.add(index) as *mut T })
        } else {
            None
        }
    }

    /// Determine size to grow to
    fn default_array_growth(existing_capacity: usize) -> usize {
        if existing_capacity == 0 {
            8
        } else {
            existing_capacity
                .checked_add(existing_capacity / 2)
                .expect("cannot grow array")
        }
    }

    fn write(&mut self, index: usize, item: T) -> &T {
        // SAFETY: The write is valid because:
        // - `get_offset` returns None if index >= length (panic path)
        // - `get_offset` ensures the pointer is within allocated capacity
        // - The memory at dest is valid for writing T
        // - After write, the reference is valid for the borrow lifetime
        unsafe {
            let dest = self.get_offset(index).expect("write: bounds error");
            std::ptr::write(dest, item);
            &*dest as &T
        }
    }

    fn read(&self, index: usize) -> T {
        // SAFETY: The read is valid because:
        // - `get_offset` returns None if index >= length (panic path)
        // - Elements at indices < length have been initialised
        // - T is Clone, so reading produces a valid copy
        // - The original value remains valid (no double-free risk)
        unsafe {
            let dest = self.get_offset(index).expect("bounds error");
            std::ptr::read(dest)
        }
    }

    // Return pointer to allocated data for navigating to header (and
    // marking during GC)
    pub fn allocated_data(&self) -> Option<RefPtr<u8>> {
        self.data.ptr.map(|p| p.cast())
    }
}

#[cfg(test)]
pub mod tests {

    use crate::eval::memory::{heap::Heap, mutator::MutatorHeapView};

    use super::*;

    #[test]
    pub fn test_simple_array_ops() {
        let heap = Heap::new();
        let view = MutatorHeapView::new(&heap);
        let mut arr = Array::default();
        for i in 0..128 {
            arr.push(&view, i);
        }
        assert_eq!(arr.len(), 128);

        for _i in 0..64 {
            arr.pop();
        }
        assert_eq!(arr.top(), Some(63));
    }

    #[test]
    pub fn test_array_bounds_checking() {
        let heap = Heap::new();
        let view = MutatorHeapView::new(&heap);
        let mut arr = Array::default();

        // Push some items
        for i in 0..5 {
            arr.push(&view, i);
        }
        assert_eq!(arr.len(), 5);

        // Test valid set operations
        assert!(arr.set(0, 100).is_ok());
        assert!(arr.set(4, 400).is_ok());
        assert_eq!(arr.get(0), Some(100));
        assert_eq!(arr.get(4), Some(400));

        // Test bounds checking for set
        assert!(matches!(
            arr.set(5, 500).unwrap_err(),
            ExecutionError::ArrayBoundsError {
                index: 5,
                length: 5
            }
        ));
        assert!(matches!(
            arr.set(100, 1000).unwrap_err(),
            ExecutionError::ArrayBoundsError {
                index: 100,
                length: 5
            }
        ));

        // Test valid get operations
        assert_eq!(arr.get(0), Some(100));
        assert_eq!(arr.get(4), Some(400));
        assert_eq!(arr.get(5), None);
        assert_eq!(arr.get(100), None);

        // Test pop boundary
        assert_eq!(arr.len(), 5);
        assert!(arr.pop().is_some());
        assert_eq!(arr.len(), 4);

        // Pop all remaining items
        while !arr.is_empty() {
            arr.pop();
        }
        assert_eq!(arr.len(), 0);
        assert_eq!(arr.pop(), None);
    }

    #[test]
    pub fn test_array_slice_safety() {
        let heap = Heap::new();
        let view = MutatorHeapView::new(&heap);
        let mut arr = Array::default();

        // Empty array should return empty slice
        let slice = arr.as_slice();
        assert_eq!(slice.len(), 0);

        // Add items and check slice
        for i in 0..10 {
            arr.push(&view, i);
        }
        let slice = arr.as_slice();
        assert_eq!(slice.len(), 10);
        assert_eq!(slice[0], 0);
        assert_eq!(slice[9], 9);

        // Iterator should work correctly
        let collected: Vec<_> = arr.iter().cloned().collect();
        assert_eq!(collected, (0..10).collect::<Vec<_>>());
    }

    #[test]
    pub fn test_unchecked_set_performance_path() {
        let heap = Heap::new();
        let view = MutatorHeapView::new(&heap);
        let mut arr = Array::with_capacity(&view, 5);

        // Pre-populate array to establish length
        for i in 0..5 {
            arr.push(&view, i * 10);
        }
        assert_eq!(arr.len(), 5);

        // Use unchecked set for performance-critical controlled updates
        unsafe {
            arr.set_unchecked(0, 999);
            arr.set_unchecked(4, 888);
        }

        // Verify updates worked
        assert_eq!(arr.get(0), Some(999));
        assert_eq!(arr.get(4), Some(888));
        assert_eq!(arr.get(1), Some(10)); // Unchanged
        assert_eq!(arr.get(3), Some(30)); // Unchanged
    }
}
