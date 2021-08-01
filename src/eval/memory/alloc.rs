//! The machine allocator
//!
//! Based on "Writing interpreters in Rust" but simpler.

use std::ptr::NonNull;

use crate::eval::stg::{
    env::{Closure, EnvFrame},
    machine::Continuation,
    syntax::{Native, Ref},
};

use super::bump::AllocError;

pub trait StgObject {}

impl StgObject for Ref {}
impl StgObject for Native {}
impl StgObject for EnvFrame {}
impl StgObject for Closure {}
impl StgObject for Continuation {}

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

pub trait ScopedRef<T> {
    fn scoped_ref<'scope>(&self, guard: &'scope dyn MutatorScope) -> &'scope T;
}

pub struct ScopedPtr<'guard, T: Sized> {
    value: &'guard T,
}

impl<'guard, T: Sized> ScopedPtr<'guard, T> {
    pub fn new(_guard: &'guard dyn MutatorScope, value: &'guard T) -> ScopedPtr<'guard, T> {
        ScopedPtr { value }
    }
}
