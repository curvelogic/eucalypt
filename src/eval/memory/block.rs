//! Blocks of memory acquired from the OS
//!
//! Heavily based on https://rust-hosted-langs.github.io/

use std::alloc::{alloc, dealloc, Layout};
use std::ptr::NonNull;

use super::bump::BLOCK_SIZE_BYTES;

/// A block of memory allocated by the OS / upstream allocator
#[derive(Debug, PartialEq, Eq)]
pub struct Block {
    /// Pointer to memory
    ptr: NonNull<u8>,
    /// Size of block
    size: usize,
}

#[derive(Debug, PartialEq, Eq)]
pub enum BlockError {
    BadSize,
    OOM,
}

impl Block {
    /// Defer to global allocatore to create new block of given size
    pub fn new(size: usize) -> Result<Self, BlockError> {
        if !size.is_power_of_two() {
            Err(BlockError::BadSize)
        } else {
            Ok(Block {
                ptr: Self::alloc_block(size)?,
                size,
            })
        }
    }

    pub fn size(&self) -> usize {
        self.size
    }

    pub fn as_ptr(&self) -> *const u8 {
        self.ptr.as_ptr()
    }

    fn alloc_block(size: usize) -> Result<NonNull<u8>, BlockError> {
        unsafe {
            let ptr = alloc(Layout::from_size_align_unchecked(size, size));
            if ptr.is_null() {
                Err(BlockError::OOM)
            } else {
                if cfg!(debug_assertions) {
                    // fill memory with 0xff to aid debugging
                    let mem = std::slice::from_raw_parts_mut(ptr, size);
                    mem.fill(0xff);
                }
                Ok(NonNull::new_unchecked(ptr))
            }
        }
    }

    /// Fill areas that are meant to be dead with 0xff to aid debugging
    pub fn fill(&self, offset_bytes: usize, size_bytes: usize) {
        #[cfg(debug_assertions)]
        unsafe {
            let start = self.ptr.as_ptr().add(offset_bytes);
            let mem = std::slice::from_raw_parts_mut(start, size_bytes);
            mem.fill(0xff);
        }
    }

    fn dealloc_block(ptr: NonNull<u8>, size: usize) {
        unsafe { dealloc(ptr.as_ptr(), Layout::from_size_align_unchecked(size, size)) }
    }

    pub fn byte_offset_of<T>(&self, ptr: NonNull<T>) -> Option<usize> {
        // TODO: efficiency
        if ptr.cast() > self.ptr {
            let offset = (ptr.as_ptr() as usize).abs_diff(self.ptr.as_ptr() as usize);
            if offset < BLOCK_SIZE_BYTES {
                return Some(offset);
            }
        }
        None
    }
}

impl Drop for Block {
    fn drop(&mut self) {
        Self::dealloc_block(self.ptr, self.size);
    }
}

#[cfg(test)]
mod tests {

    use super::*;

    #[test]
    fn test_bad_sizealign() {
        assert_eq!(Block::new(999), Err(BlockError::BadSize))
    }

    #[test]
    fn test_32k() {
        let block = Block::new(0x8000).unwrap();

        let mask = 0x7fff;
        let loc = block.ptr.as_ptr() as usize;
        assert!((loc & mask) ^ mask == mask);

        drop(block);
    }
}
