//! The machine allocator
//!
//! Based on "Writing interpreters in Rust" but simpler.

use crate::eval::error::ExecutionError;

use super::bump::AllocError;
use std::ptr::NonNull;
use std::{cell::RefCell, ops::Deref};

pub trait StgObject {}

impl<T> StgObject for RefCell<T> {}
impl<T> StgObject for NonNull<T> {}

pub struct AllocHeader {}

/// Allocator for STG
pub trait Allocator {
    /// Allocate a T
    fn alloc<T>(&self, object: T) -> Result<NonNull<T>, AllocError>
    where
        T: StgObject;

    /// Allocate a region of bytes
    fn alloc_bytes(&self, size_bytes: usize) -> Result<NonNull<u8>, AllocError>;

    /// Get header from object
    fn get_header<T>(&self, object: NonNull<T>) -> NonNull<AllocHeader>;

    /// Get object from header
    fn get_object(&self, header: NonNull<AllocHeader>) -> NonNull<()>;
}

/// Anything that can be used as a mutator scope
pub trait MutatorScope {}

/// A scoped pointer is a pointer valid only during a lifetime
pub struct ScopedPtr<'guard, T: Sized> {
    value: &'guard T,
}

impl<'guard, T: Sized> MutatorScope for ScopedPtr<'guard, T> {}

impl<'guard, T: Sized> ScopedPtr<'guard, T> {
    pub fn new(_guard: &'guard dyn MutatorScope, value: &'guard T) -> ScopedPtr<'guard, T> {
        ScopedPtr { value }
    }

    pub fn from_non_null(guard: &'guard dyn MutatorScope, ptr: NonNull<T>) -> ScopedPtr<'guard, T> {
        ScopedPtr::new(guard, unsafe { &*ptr.as_ptr() })
    }

    pub fn as_ptr(&self) -> NonNull<T> {
        unsafe { NonNull::new_unchecked(&*self.value as *const T as *mut T) }
    }
}

impl<'guard, T: Sized> Deref for ScopedPtr<'guard, T> {
    type Target = T;

    fn deref(&self) -> &T {
        self.value
    }
}

/// Scoped Allocator requires guard and returns scoped pointers
pub trait ScopedAllocator<'guard> {
    /// Allocate and return a scoped pointer
    fn alloc<T>(&'guard self, object: T) -> Result<ScopedPtr<'guard, T>, ExecutionError>
    where
        T: StgObject;

    /// Allocate a region of bytes
    fn alloc_bytes(&self, size_bytes: usize) -> Result<NonNull<u8>, ExecutionError>;
}
