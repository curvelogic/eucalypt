//! The machine allocator
//!
//! Based on "Writing interpreters in Rust" but simpler.

use std::ptr::NonNull;

use super::bump::AllocError;

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
    fn get_header<T>(object: NonNull<T>) -> NonNull<AllocHeader>;

    /// Get object from header
    fn get_object(header: NonNull<AllocHeader>) -> NonNull<()>;
}
