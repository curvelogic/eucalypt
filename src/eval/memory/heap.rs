//! The STG heap implementation

use std::collections::LinkedList;
use std::fmt::Debug;
use std::ptr::NonNull;
use std::{cell::UnsafeCell, mem::size_of};
use std::{ptr::write, slice::from_raw_parts_mut};

use super::bump::BLOCK_SIZE_BYTES;
use super::{
    alloc::{Allocator, MutatorScope},
    bump::{self, AllocError, BumpBlock},
    header::AllocHeader,
    lob::LargeObjectBlock,
};

#[derive(Debug)]
pub struct HeapStats {
    /// Number of standard blocks allocated
    pub blocks_allocated: usize,
    /// Number of large objects allocated
    pub lobs_allocated: usize,
    /// Number of blocks used and not reclaimed
    pub used: usize,
    /// Number of blocks used and recycled
    pub recycled: usize,
}

/// Object size class.
#[repr(u8)]
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
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
        if object_size < bump::LINE_SIZE_BYTES {
            SizeClass::Small
        } else if object_size < bump::BLOCK_SIZE_BYTES {
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
    /// Recycled - part used but reclaimed
    recycled: LinkedList<BumpBlock>,
    /// Part used - not yet reclaimed
    rest: LinkedList<BumpBlock>,
    /// Large object blocks - each contains single object
    lobs: Vec<LargeObjectBlock>,
}

impl Default for HeapState {
    fn default() -> Self {
        HeapState::new()
    }
}

impl std::fmt::Debug for HeapState {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for block in &self.rest {
            writeln!(f, "(XX) {:?}", block)?;
        }

        for block in &self.recycled {
            writeln!(f, "(Cy) {:?}", block)?;
        }

        if let Some(head) = &self.head {
            writeln!(f, "(Hd) {:?}", head)?;
        }

        if let Some(of) = &self.overflow {
            writeln!(f, "(Ov) Overflow:\n\n{:?}", of)?;
        }

        for lob in &self.lobs {
            writeln!(f, "{:?}", lob)?;
        }

        writeln!(f, "\n")
    }
}

impl HeapState {
    pub fn new() -> Self {
        HeapState {
            head: None,
            overflow: None,
            recycled: LinkedList::default(),
            rest: LinkedList::default(),
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
        let replacement = self.recycled.pop_front().unwrap_or_default();

        self.head.replace(replacement).and_then(|old| {
            self.rest.push_back(old);
            None as Option<BumpBlock>
        });
        self.head.as_mut().unwrap()
    }

    pub fn replace_overflow(&mut self) -> &mut BumpBlock {
        self.overflow.replace(BumpBlock::new()).and_then(|old| {
            self.rest.push_back(old);
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

    /// Look for reclaimable blocks and move to recycled list
    pub fn sweep(&mut self) {
        let mut unusable: LinkedList<BumpBlock> = LinkedList::default();

        while let Some(mut block) = self.rest.pop_front() {
            if block.recycle() {
                self.recycled.push_back(block);
            } else {
                unusable.push_back(block);
            }
        }

        self.rest.append(&mut unusable);
    }

    /// Statistics
    pub fn stats(&self) -> HeapStats {
        HeapStats {
            blocks_allocated: self.rest.len()
                + self.recycled.len()
                + self.head.iter().count()
                + self.overflow.iter().count(),
            lobs_allocated: self.lobs.len(),
            used: self.rest.len(),
            recycled: self.recycled.len(),
        }
    }
}

/// A heap (with interior mutability)
pub struct Heap {
    state: UnsafeCell<HeapState>,
    limit: Option<usize>,
}

impl MutatorScope for Heap {}

impl Debug for Heap {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        unsafe { (*self.state.get()).fmt(f) }
    }
}

impl Default for Heap {
    fn default() -> Self {
        Heap::new()
    }
}

impl Heap {
    pub fn new() -> Self {
        Heap {
            state: UnsafeCell::new(HeapState::new()),
            limit: None,
        }
    }

    pub fn with_limit(limit_mib: usize) -> Self {
        let block_limit = (limit_mib * 1_048_576) / BLOCK_SIZE_BYTES;
        println!("block limit {:?}", block_limit);
        Heap {
            state: UnsafeCell::new(HeapState::new()),
            limit: Some(block_limit),
        }
    }

    pub fn stats(&self) -> HeapStats {
        unsafe { (*self.state.get()).stats() }
    }

    pub fn policy_requires_collection(&self) -> bool {
        if let Some(limit) = self.limit {
            let stats = self.stats();
            stats.blocks_allocated >= limit
                && (stats.recycled as f32 / stats.blocks_allocated as f32) < 0.25
        } else {
            false
        }
    }
}

impl Allocator for Heap {
    /// Allocate an object into the backing bump blocks
    fn alloc<T>(&self, object: T) -> Result<NonNull<T>, super::bump::AllocError>
    where
        T: super::alloc::StgObject,
    {
        let header_size = size_of::<AllocHeader>();
        let object_size = size_of::<T>();
        let alloc_size = Self::alloc_size_of(header_size + object_size);

        let space = self.find_space(alloc_size)?;

        let header = AllocHeader::default();

        unsafe {
            write(space as *mut AllocHeader, header);
            let object_space = space.add(header_size);
            write(object_space as *mut T, object);
            Ok(NonNull::new_unchecked(object_space as *mut T))
        }
    }

    /// Allocate a region of bytes (for array / vector implementations)
    fn alloc_bytes(&self, size_bytes: usize) -> Result<NonNull<u8>, super::bump::AllocError> {
        if size_bytes > u32::MAX as usize {
            return Err(AllocError::BadRequest);
        }

        let header_size = size_of::<AllocHeader>();
        let alloc_size = Self::alloc_size_of(header_size + size_bytes);

        let space = self.find_space(alloc_size)?;

        let header = AllocHeader::new(alloc_size as u32);

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
    fn get_header<T>(&self, object: NonNull<T>) -> NonNull<AllocHeader> {
        let header_ptr =
            unsafe { NonNull::new_unchecked(object.cast::<AllocHeader>().as_ptr().offset(-1)) };
        debug_assert!(header_ptr.as_ptr() as usize > 0);
        header_ptr
    }

    /// Get object from header pointer
    fn get_object(&self, header: NonNull<AllocHeader>) -> NonNull<()> {
        unsafe { NonNull::new_unchecked(header.as_ptr().offset(1)).cast::<()>() }
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

    // GC functions

    /// Reset all region marks ready for a fresh GC
    pub fn reset_region_marks(&self) {
        let heap_state = unsafe { &mut *self.state.get() };

        if let Some(head) = &mut heap_state.head {
            head.reset_region_marks();
        }

        for block in &mut heap_state.rest {
            block.reset_region_marks();
        }
    }

    /// Check wither an object is marked
    pub fn is_marked<T>(&self, ptr: NonNull<T>) -> bool {
        let header: NonNull<AllocHeader> = self.get_header(ptr);
        debug_assert!(header.as_ptr() as usize > 0);
        unsafe { (*header.as_ptr()).is_marked() }
    }

    /// Mark an object as live
    pub fn mark_object<T>(&self, ptr: NonNull<T>) {
        debug_assert!(ptr != NonNull::dangling() && ptr.as_ptr() as usize != 0xffffffffffffffff);
        let header: NonNull<AllocHeader> = self.get_header(ptr);
        unsafe {
            (*header.as_ptr()).mark();
        }
    }

    /// Unmark object
    pub fn unmark_object<T>(&self, ptr: NonNull<T>) {
        debug_assert!(ptr != NonNull::dangling() && ptr.as_ptr() as usize != 0xffffffffffffffff);
        let header = self.get_header(ptr);
        unsafe { (*header.as_ptr()).unmark() }
    }

    /// Mark lines for and object (array) that uses untyped backing
    /// store of bytes.
    ///
    /// We need to consult the header for size.
    pub fn mark_lines_for_bytes(&mut self, ptr: NonNull<u8>) {
        let header = unsafe { self.get_header(ptr).as_mut() };
        let heap_state = unsafe { &mut *self.state.get() };

        let bytes = header.length() as usize;
        // TODO: fix
        if let Some(head) = &mut heap_state.head {
            head.mark_region(ptr, bytes);
        }

        for block in &mut heap_state.rest {
            block.mark_region(ptr, bytes);
        }
    }

    /// Mark the line in the appropriate block map
    pub fn mark_line<T>(&mut self, ptr: NonNull<T>) {
        // depending on size of object + header, mark line or lines
        let heap_state = unsafe { &mut *self.state.get() };

        let size = size_of::<T>();
        // TODO: fix this - go directly to the right block!
        if SizeClass::for_size(size) == SizeClass::Medium {
            if let Some(head) = &mut heap_state.head {
                head.mark_region(ptr.cast(), size);
            }

            for block in &mut heap_state.rest {
                block.mark_region(ptr.cast(), size);
            }
        } else {
            if let Some(head) = &mut heap_state.head {
                head.mark_line(ptr);
            }

            for block in &mut heap_state.rest {
                block.mark_line(ptr);
            }
        }
    }

    /// Unmark the line in the appropriate block map
    pub fn unmark_line<T>(&mut self, ptr: NonNull<T>) {
        // depending on size of object + header, unmark line or lines
        let _header = self.get_header(ptr);
        todo!();
    }

    pub fn sweep(&mut self) {
        self.state.get_mut().sweep()
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

        let header_ptr = heap.get_header(ptr);
        let difference = ptr.as_ptr() as usize - header_ptr.as_ptr() as usize;
        assert_eq!(difference, size_of::<AllocHeader>());

        unsafe {
            assert!(!(*header_ptr.as_ptr()).is_marked());
        }
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
