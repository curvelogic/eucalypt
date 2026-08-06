//! Support mutator access to heap and machine

use std::marker::PhantomData;

use crate::eval::{
    error::ExecutionError,
    memory::{
        self,
        alloc::{Allocator, MutatorScope, ScopedAllocator, ScopedPtr, StgObject},
        array::Array,
        heap::Heap,
        syntax::{Ref, RefPtr, StgBuilder},
    },
};

use super::{string::HeapString, syntax::Native};

/// RAII guard that keeps a heap block pinned (non-evacuatable).
pub struct PinGuard {
    base_address: usize,
    heap: *const super::heap::Heap,
}

impl Drop for PinGuard {
    fn drop(&mut self) {
        let heap = unsafe { &*self.heap };
        heap.unpin_block(self.base_address);
    }
}

impl std::fmt::Debug for PinGuard {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "PinGuard(block @ {:#x})", self.base_address)
    }
}

/// A view onto the heap for code that needs mutator access (as
/// opposed to collector access)
///
/// MutatorHeapView provides a scope for dereferencing heap pointers,
/// the means of allocation and convenience constructors for
/// allocating in-heap syntax.
///
/// Internally stores a raw pointer rather than a reference so that a
/// view can be constructed alongside a `&mut` borrow of the same
/// `MachineCore` struct (which also contains the `Heap`).  The
/// phantom lifetime `'guard` upholds the borrow-checker invariant
/// that the heap lives at least as long as the view.  This is safe
/// because `Heap` is entirely `UnsafeCell`/`Cell`-based and all heap
/// mutations go through interior mutability.
#[derive(Copy, Clone)]
pub struct MutatorHeapView<'guard> {
    heap: *const Heap,
    _phantom: PhantomData<&'guard Heap>,
}

impl<'guard> MutatorHeapView<'guard> {
    pub fn new(heap: &'guard Heap) -> Self {
        MutatorHeapView {
            heap: heap as *const _,
            _phantom: PhantomData,
        }
    }

    /// Construct a view from a raw heap pointer.
    ///
    /// # Safety
    ///
    /// `heap` must be a valid, aligned pointer to a `Heap` that remains live
    /// for the lifetime `'guard`.  The caller must ensure no conflicting
    /// exclusive Rust references to the `Heap` (or fields that alias it) exist
    /// for the duration of `'guard`.
    pub unsafe fn from_raw_heap(heap: *const Heap) -> Self {
        MutatorHeapView {
            heap,
            _phantom: PhantomData,
        }
    }

    #[inline(always)]
    fn heap_ref(&self) -> &'guard Heap {
        // SAFETY: the phantom lifetime 'guard guarantees the heap is alive.
        unsafe { &*self.heap }
    }

    /// Obtain a scoped pointer from a RefPtr for dereferencing
    pub fn scoped<T: Sized>(self, ptr: RefPtr<T>) -> ScopedPtr<'guard, T> {
        ScopedPtr::from_non_null(self.heap_ref(), ptr)
    }

    /// Allocate an array, copying from a slice
    pub fn array<T: Sized + Clone>(self, data: &[T]) -> Array<T> {
        Array::from_slice(&self, data)
    }

    /// Allocate a singleton array
    pub fn singleton<T: Sized + Clone>(self, object: T) -> Array<T> {
        let mut array = Array::with_capacity(&self, 1);
        array.push(&self, object);
        array
    }

    /// Pin the block containing `ptr`, preventing evacuation.
    pub fn pin<T>(&self, ptr: std::ptr::NonNull<T>) -> PinGuard {
        self.heap_ref().pin_block(ptr);
        PinGuard {
            base_address: super::bump::block_base_of(ptr),
            heap: self.heap,
        }
    }
}

/// Allow allocation in a mutator scope
impl<'guard> ScopedAllocator<'guard> for MutatorHeapView<'guard> {
    /// Allocate and return scoped pointer
    fn alloc<T>(&'guard self, object: T) -> Result<ScopedPtr<'guard, T>, ExecutionError>
    where
        T: StgObject,
    {
        self.heap_ref()
            .alloc(object)
            .map(|p| self.scoped(p))
            .map_err(Into::into)
    }

    /// Allocate and return region of bytes
    fn alloc_bytes(&self, size_bytes: usize) -> Result<std::ptr::NonNull<u8>, ExecutionError> {
        self.heap_ref().alloc_bytes(size_bytes).map_err(Into::into)
    }

    /// Allocate a region of bytes without zero-initialisation
    fn alloc_bytes_uninit(
        &self,
        size_bytes: usize,
    ) -> Result<std::ptr::NonNull<u8>, ExecutionError> {
        self.heap_ref()
            .alloc_bytes_uninit(size_bytes)
            .map_err(Into::into)
    }
}

/// Allocate native values that need no runtime code: an interned symbol, or
/// a string on the heap.
impl<'guard> StgBuilder<'guard> for MutatorHeapView<'guard> {
    fn sym_ref<T: AsRef<str>>(
        &'guard self,
        pool: &mut memory::symbol::SymbolPool,
        s: T,
    ) -> Result<Ref, ExecutionError> {
        let id = pool.intern(s.as_ref());
        Ok(Ref::V(Native::Sym(id)))
    }

    fn str<T: AsRef<str>>(
        &'guard self,
        s: T,
    ) -> Result<ScopedPtr<'guard, memory::string::HeapString>, ExecutionError> {
        self.alloc(HeapString::from_str(self, s.as_ref()))
    }

    fn str_ref<T: AsRef<str>>(&'guard self, s: T) -> Result<Ref, ExecutionError> {
        Ok(Ref::V(Native::Str(self.str(s)?.as_ptr())))
    }
}

impl MutatorScope for MutatorHeapView<'_> {}

/// Implement mutator to get access to the heap as a mutator
pub trait Mutator: Sized {
    type Input;
    type Output;

    fn run(
        &self,
        view: &MutatorHeapView,
        input: Self::Input,
    ) -> Result<Self::Output, ExecutionError>;
}
