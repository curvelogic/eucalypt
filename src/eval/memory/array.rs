//! A simple vector-like array in our allocator
//!
//! Environments, closures and STG syntax items all require
//! collections allocated in the heap. `Array` is a very crude vector
//! with storage allocated in the STG heap. It panics on allocation
//! failure (as Vec does...)

use std::{cmp::min, mem::size_of, slice::from_raw_parts};

use crate::eval::error::ExecutionError;

use super::{alloc::ScopedAllocator, syntax::RefPtr};

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
        RawArray {
            capacity: self.capacity,
            ptr: self.ptr,
        }
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
                .checked_mul(size_of::<T>() as usize)
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
    pub fn set(&mut self, index: usize, item: T) {
        self.write(index, item);
    }

    /// As immutable slice
    pub fn as_slice(&self) -> &[T] {
        if let Some(ptr) = self.data.as_ptr() {
            unsafe { from_raw_parts(ptr, self.length) }
        } else {
            &[]
        }
    }

    /// Read only iterator
    pub fn iter(&self) -> std::slice::Iter<T> {
        self.as_slice().iter()
    }

    /// Return pointer for index
    fn get_offset(&self, index: usize) -> Option<*mut T> {
        if index < self.length {
            self.data
                .as_ptr()
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
        unsafe {
            let dest = self.get_offset(index).expect("write: bounds error");
            std::ptr::write(dest, item);
            &*dest as &T
        }
    }

    fn read(&self, index: usize) -> T {
        unsafe {
            let dest = self.get_offset(index).expect("bounds error");
            std::ptr::read(dest)
        }
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
}
