//! A large object allocation
//!
//! A memory region that contains a single object and header

use std::process::abort;

use super::block::Block;

/// A memory allocation containing a single large object with its
/// header, this differs from Block in that it needn't be a power of
/// two.
pub struct LargeObjectBlock {
    /// Block
    block: Block,
}

impl LargeObjectBlock {
    /// Create a new LargeObjectBlock of size sufficient to contain
    /// `required_size` bytes (but potentially much larger). The size
    /// of any object headers is assumed to be already included.
    pub fn new(required_size: usize) -> Self {
        // TODO: extraordinarily wasteful!
        let size = required_size.next_power_of_two();
        LargeObjectBlock {
            block: Block::new(size).unwrap_or_else(|_| abort()),
        }
    }

    /// Pointer to the writeable memory area
    pub fn space(&self) -> *const u8 {
        self.block.as_ptr()
    }
}
