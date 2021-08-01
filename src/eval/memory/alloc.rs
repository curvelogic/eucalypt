//! The machine allocator
//!
//! Based on "Writing interpreters in Rust" but simpler.

use super::bump::AllocError;
use std::ops::Deref;
use std::ptr::NonNull;

pub trait StgObject {}

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

pub struct ScopedPtr<'guard, T: Sized> {
    value: &'guard T,
}

impl<'guard, T: Sized> ScopedPtr<'guard, T> {
    pub fn new(_guard: &'guard dyn MutatorScope, value: &'guard T) -> ScopedPtr<'guard, T> {
        ScopedPtr { value }
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
