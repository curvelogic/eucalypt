//! Basic (downward) bump allocation and line map
//!

use std::{fmt::Debug, process::abort, ptr::NonNull};

use bitmaps::Bitmap;

use super::block::{Block, BlockError};

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum AllocError {
    BadRequest,
    OOM,
}

impl From<BlockError> for AllocError {
    fn from(e: BlockError) -> Self {
        match e {
            BlockError::BadSize => AllocError::BadRequest,
            BlockError::OOM => AllocError::OOM,
        }
    }
}

/// 32K Block
pub const BLOCK_SIZE_BITS: usize = 15;
/// 32K Block
pub const BLOCK_SIZE_BYTES: usize = 1 << BLOCK_SIZE_BITS;

/// 128 bit line
pub const LINE_SIZE_BITS: usize = 7;
/// 128 bit line
pub const LINE_SIZE_BYTES: usize = 1 << LINE_SIZE_BITS;
/// Lines in a block
pub const LINE_COUNT: usize = BLOCK_SIZE_BYTES / LINE_SIZE_BYTES;

/// Maximum allocation size
pub const MAX_ALLOC_SIZE: usize = std::u32::MAX as usize;

/// LineMap contains mark flags for each line
#[derive(Default)]
pub struct LineMap(Bitmap<LINE_COUNT>);

impl LineMap {
    /// Mark the specified line
    pub fn mark(&mut self, index: usize) {
        debug_assert!(index < LINE_COUNT);
        self.0.set(index, true);
    }

    /// Clear the specified line
    pub fn clear(&mut self, index: usize) {
        debug_assert!(index < LINE_COUNT);
        self.0.set(index, false);
    }

    /// Check whether the specified line is marked
    pub fn marked(&self, index: usize) -> bool {
        debug_assert!(index < LINE_COUNT);
        self.0.get(index)
    }

    /// Clear the specified line
    pub fn reset(&mut self) {
        self.0.clone_from(&Bitmap::new());
    }

    /// Find next hole
    ///
    /// Work downward through the line map. Like immix we use
    /// "conservative marking" that means we need two clear lines to
    /// recognise a gap.
    ///
    /// Returns memory offsets (within the block) of the next hole.
    pub fn find_hole(&self, below_offset: usize) -> Option<(usize, usize)> {
        let limit_line = below_offset / LINE_SIZE_BYTES;
        let mut count = 0;
        let mut upper: Option<usize> = None;
        let mut lower = below_offset;

        for line in (0..limit_line).rev() {
            let marked = self.marked(line);

            if !marked {
                count += 1;

                if upper.is_none() {
                    upper = Some(line + 1);
                }

                lower = line;
            }

            if count > 1 && (marked || lower == 0) {
                if let Some(upper) = upper {
                    if lower > 0 {
                        lower += 1; // conservative mark
                    }
                    return Some((lower * LINE_SIZE_BYTES, upper * LINE_SIZE_BYTES));
                }
            }

            if marked {
                count = 0;
                upper = None;
            }
        }

        None
    }
}

impl Debug for LineMap {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let dwords: [u128; 2] = self.0.into();
        for dword in dwords {
            let lo = dword as u64;
            let hi = (dword >> 64) as u64;
            writeln!(f, "{:#018x} {:#018x}", lo, hi)?;
        }
        Ok(())
    }
}

/// A memory block with downward bump allocation machinery and a mark
/// map
pub struct BumpBlock {
    /// Block
    block: Block,
    /// Lower limit offset
    lower: usize,
    /// Pointer to move as regions are allocated
    cursor: usize,
    /// Block map to store mark flags
    line_map: LineMap,
}

impl Default for BumpBlock {
    fn default() -> Self {
        BumpBlock::new()
    }
}

impl Debug for BumpBlock {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(
            f,
            "{:p} {}:{}:{}",
            self.block.as_ptr(),
            self.block.size(),
            self.cursor,
            self.lower
        )?;
        writeln!(f, "{:?}", self.line_map)
    }
}

impl BumpBlock {
    /// Initialise a fresh new block ready to start bumping downwards
    pub fn new() -> Self {
        BumpBlock {
            block: Block::new(BLOCK_SIZE_BYTES).unwrap_or_else(|_| abort()),
            cursor: BLOCK_SIZE_BYTES,
            lower: 0,
            line_map: LineMap::default(),
        }
    }

    /// Bump cursor back by size
    pub fn bump(&mut self, size: usize) -> Option<*const u8> {
        if size > self.cursor {
            None
        } else {
            let next = self.cursor - size;
            if next < self.lower {
                // find next hole
                if let Some((lower, cursor)) = self.line_map.find_hole(self.cursor) {
                    self.lower = lower;
                    self.cursor = cursor;
                    self.bump(size)
                } else {
                    None
                }
            } else {
                self.cursor = next;
                unsafe { Some(self.block.as_ptr().add(next) as *const u8) }
            }
        }
    }

    /// Size in bytes of the hole we're currently allocating into
    pub fn current_hole_size(&self) -> usize {
        self.cursor - self.lower
    }

    /// Reset line maps ready for GC trace
    pub fn reset_region_marks(&mut self) {
        self.line_map.reset();
    }

    /// If ptr is within the block, mark line containing the ptr.
    ///
    /// Live object is assumed to be Small and therefore following
    /// line is treated as implicitly marked
    pub fn mark_line<T>(&mut self, ptr: NonNull<T>) {
        if let Some(offset) = self.block.byte_offset_of(ptr) {
            self.line_map.mark(offset / LINE_SIZE_BYTES);
        }
    }

    /// If ptr is within the block, mark lines contain the region
    /// starting at ptr and extending for bytes.
    pub fn mark_region(&mut self, ptr: NonNull<u8>, bytes: usize) {
        // n.b. div_ceil is in nightly
        let lines = bytes / LINE_SIZE_BYTES + 1;
        if let Some(offset) = self.block.byte_offset_of(ptr) {
            let first_line = offset / LINE_SIZE_BYTES;
            for line in first_line..(first_line + lines) {
                self.line_map.mark(line);
            }
        }
    }
}

#[cfg(test)]
pub mod tests {

    use super::*;

    #[test]
    pub fn test_find_hole() {
        let mut map = LineMap::default();
        map.mark(0);
        map.mark(10);
        assert_eq!(
            map.find_hole(10 * LINE_SIZE_BYTES),
            Some((2 * LINE_SIZE_BYTES, 10 * LINE_SIZE_BYTES))
        );
    }
}
