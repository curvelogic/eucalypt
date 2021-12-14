//! The STG heap implementation

use std::{cell::UnsafeCell, mem::size_of, ptr::NonNull};
use std::{ptr::write, slice::from_raw_parts_mut};

use super::alloc::MutatorScope;
use super::lob::LargeObjectBlock;
use super::{
    alloc::{AllocHeader, Allocator},
    bump::{self, AllocError, BumpBlock},
};

#[derive(Debug)]
pub struct HeapStats {
    /// Number of standard blocks allocated
    pub blocks_allocated: usize,
    /// Number of large objects allocated
    pub lobs_allocated: usize,
}

/// Object size class.
#[repr(u8)]
#[derive(Copy, Clone, Debug, PartialEq)]
pub enum SizeClass {
    /// Small objects fit inside a line
    Small,
    /// Medium objects span lines inside a block
    Medium,
    /// Large objects are larger than a normal block
    Large,
}

impl SizeClass {
    pub fn for_size(object_size: usize) -> SizeClass {
        if object_size < bump::LINE_SIZE {
            SizeClass::Small
        } else if object_size < bump::BLOCK_SIZE {
            SizeClass::Medium
        } else {
            SizeClass::Large
        }
    }
}

/// Simple heap of bump blocks with no reclaim for now
pub struct HeapState {
    /// For allocating small objects
    head: Option<BumpBlock>,
    /// For allocating medium objects
    overflow: Option<BumpBlock>,
    /// Part used - not yet reclaimed
    rest: Vec<BumpBlock>,
    /// Large object blocks - each contains single object
    lobs: Vec<LargeObjectBlock>,
}

impl Default for HeapState {
    fn default() -> Self {
        HeapState::new()
    }
}

impl HeapState {
    pub fn new() -> Self {
        HeapState {
            head: None,
            overflow: None,
            rest: vec![],
            lobs: vec![],
        }
    }

    /// Get head block, creating if necessary
    pub fn head(&mut self) -> &mut BumpBlock {
        if self.head.is_none() {
            self.head = Some(BumpBlock::new());
        }

        self.head.as_mut().unwrap()
    }

    /// Get overflow block, creating if necessary
    pub fn overflow(&mut self) -> &mut BumpBlock {
        if self.overflow.is_none() {
            self.overflow = Some(BumpBlock::new());
        }

        self.overflow.as_mut().unwrap()
    }

    pub fn replace_head(&mut self) -> &mut BumpBlock {
        self.head.replace(BumpBlock::new()).and_then(|old| {
            self.rest.push(old);
            None as Option<BumpBlock>
        });
        self.head.as_mut().unwrap()
    }

    pub fn replace_overflow(&mut self) -> &mut BumpBlock {
        self.overflow.replace(BumpBlock::new()).and_then(|old| {
            self.rest.push(old);
            None as Option<BumpBlock>
        });
        self.overflow.as_mut().unwrap()
    }

    /// Create and return a new large object block able to store data
    /// of the specified size
    pub fn lob(&mut self, size: usize) -> &mut LargeObjectBlock {
        self.lobs.push(LargeObjectBlock::new(size));
        self.lobs.last_mut().unwrap()
    }

    /// Statistics
    pub fn stats(&self) -> HeapStats {
        HeapStats {
            blocks_allocated: self.rest.len()
                + self.head.iter().count()
                + self.overflow.iter().count(),
            lobs_allocated: self.lobs.len(),
        }
    }
}

/// A heap (with interior mutability)
pub struct Heap {
    state: UnsafeCell<HeapState>,
}

impl MutatorScope for Heap {}

impl Default for Heap {
    fn default() -> Self {
        Heap::new()
    }
}

impl Heap {
    pub fn new() -> Self {
        Heap {
            state: UnsafeCell::new(HeapState::new()),
        }
    }

    pub fn stats(&self) -> HeapStats {
        unsafe { (*self.state.get()).stats() }
    }
}

impl Allocator for Heap {
    /// Allocate an object into the backing bump blocks
    fn alloc<T>(&self, object: T) -> Result<std::ptr::NonNull<T>, super::bump::AllocError>
    where
        T: super::alloc::StgObject,
    {
        let header_size = size_of::<AllocHeader>();
        let object_size = size_of::<T>();
        let alloc_size = Self::alloc_size_of(header_size + object_size);

        let space = self.find_space(alloc_size)?;

        let header = AllocHeader {};

        unsafe {
            write(space as *mut AllocHeader, header);
            let object_space = space.add(header_size);
            write(object_space as *mut T, object);
            Ok(NonNull::new_unchecked(object_space as *mut T))
        }
    }

    /// Allocate a region of bytes (for array / vector implementations)
    fn alloc_bytes(
        &self,
        size_bytes: usize,
    ) -> Result<std::ptr::NonNull<u8>, super::bump::AllocError> {
        let header_size = size_of::<AllocHeader>();
        let alloc_size = Self::alloc_size_of(header_size + size_bytes);

        let space = self.find_space(alloc_size)?;

        let header = AllocHeader {};

        unsafe {
            write(space as *mut AllocHeader, header);
            let array_space = space.add(header_size);
            let array = from_raw_parts_mut(array_space as *mut u8, size_bytes);
            for byte in array {
                *byte = 0
            }
            Ok(NonNull::new_unchecked(array_space as *mut u8))
        }
    }

    /// Get header from object pointer
    fn get_header<T>(
        &self,
        _object: std::ptr::NonNull<T>,
    ) -> std::ptr::NonNull<super::alloc::AllocHeader> {
        todo!()
    }

    /// Get object from header pointer
    fn get_object(
        &self,
        _header: std::ptr::NonNull<super::alloc::AllocHeader>,
    ) -> std::ptr::NonNull<()> {
        todo!()
    }
}

impl Heap {
    /// Allocate space
    fn find_space(&self, size_bytes: usize) -> Result<*const u8, AllocError> {
        let heap_state = unsafe { &mut *self.state.get() };
        let head = heap_state.head();

        let space = match SizeClass::for_size(size_bytes) {
            SizeClass::Large => {
                let lob = heap_state.lob(size_bytes);
                lob.space()
            }
            SizeClass::Medium if size_bytes > head.current_hole_size() => heap_state
                .overflow()
                .bump(size_bytes)
                .or_else(|| heap_state.replace_overflow().bump(size_bytes))
                .expect("aargh"),
            _ => head
                .bump(size_bytes)
                .or_else(|| heap_state.replace_head().bump(size_bytes))
                .expect("aarrgh"),
        };

        Ok(space)
    }

    /// Return the allocated size of an object as it's size_of::<T>() value rounded
    /// up to a double-word boundary
    ///
    /// TODO this isn't correctly implemented, as aligning the object to a double-word
    /// boundary while considering header size (which is not known to this libarary
    /// until compile time) means touching numerous bump-allocation code points with
    /// some math and bitwise ops I haven't worked out yet
    pub fn alloc_size_of(object_size: usize) -> usize {
        let align = size_of::<usize>(); // * 2;
        (object_size + (align - 1)) & !(align - 1)
    }
}

#[cfg(test)]
pub mod tests {
    use std::iter::repeat_with;

    use crate::{
        common::sourcemap::Smid,
        eval::memory::{
            mutator::MutatorHeapView,
            syntax::{LambdaForm, Ref, StgBuilder},
        },
    };

    use super::*;

    #[test]
    pub fn test_simple_allocations() {
        let heap = Heap::new();

        let ptr = heap.alloc(Ref::num(99)).unwrap();
        unsafe { assert_eq!(*ptr.as_ref(), Ref::num(99)) };

        // TODO: heap.get_header(ptr);
    }

    #[test]
    pub fn test_several_blocks() {
        let heap = Heap::new();

        for i in 0..32000 {
            let ptr = heap.alloc(Ref::num(i)).unwrap();
            unsafe { assert_eq!(*ptr.as_ref(), Ref::num(i)) };
        }
    }

    #[test]
    pub fn test_large_object_block() {
        let heap = Heap::new();
        let view = MutatorHeapView::new(&heap);

        let ids = repeat_with(|| -> LambdaForm {
            LambdaForm::new(1, view.atom(Ref::L(0)).unwrap().as_ptr(), Smid::default())
        })
        .take(32000)
        .collect::<Vec<_>>();
        let idarray = view.array(ids.as_slice());

        view.let_(
            idarray,
            view.app(Ref::L(0), view.singleton(view.sym_ref("foo").unwrap()))
                .unwrap(),
        )
        .unwrap();

        assert_eq!(heap.stats().lobs_allocated, 1);
    }
}
