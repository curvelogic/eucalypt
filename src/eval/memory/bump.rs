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
    /// Returns memory byte offsets (within the block) of the next hole.
    pub fn find_hole(&self, below_offset: usize) -> Option<(usize, usize)> {
        let limit_line = below_offset / LINE_SIZE_BYTES;
        let mut count = 0;
        let mut upper: Option<usize> = None;
        let mut lower = below_offset;

        for line in (0..limit_line).rev() {
            let marked = self.marked(line);

            if !marked {
                count += 1;
                lower = line;

                if upper.is_none() {
                    // Set upper boundary to the line above the current unmarked sequence
                    upper = Some(line + count);
                }
            } else {
                // Hit a marked line - check if we have a sufficient hole
                if count > 1 && upper.is_some() {
                    let hole_upper = upper.unwrap();
                    let hole_lower = lower;

                    // Apply conservative marking: exclude one line from upper end only
                    let conservative_upper = if hole_upper > hole_lower + 1 {
                        hole_upper - 1
                    } else {
                        hole_upper
                    };

                    if conservative_upper > hole_lower {
                        let lower_bytes = hole_lower * LINE_SIZE_BYTES;
                        let upper_bytes = conservative_upper * LINE_SIZE_BYTES;

                        debug_assert!(upper_bytes <= BLOCK_SIZE_BYTES);

                        return Some((lower_bytes, upper_bytes));
                    }
                }

                // Reset for next potential hole
                count = 0;
                upper = None;
            }
        }

        // Check for hole at the beginning of the block
        if count > 1 && upper.is_some() {
            let hole_upper = upper.unwrap();
            let hole_lower = lower;

            // Apply conservative marking: exclude one line from upper end only
            let conservative_upper = if hole_upper > hole_lower + 1 {
                hole_upper - 1
            } else {
                hole_upper
            };

            if conservative_upper > hole_lower {
                let lower_bytes = hole_lower * LINE_SIZE_BYTES;
                let upper_bytes = conservative_upper * LINE_SIZE_BYTES;

                debug_assert!(upper_bytes <= BLOCK_SIZE_BYTES);

                return Some((lower_bytes, upper_bytes));
            }
        }

        None
    }

    /// Calculate stats
    pub fn stats(&self) -> (usize, usize, usize) {
        let mut count_holes = 0;
        let mut free = 0;
        let mut marked = 0;
        let mut last = true;
        for i in 0..LINE_COUNT {
            let current = self.marked(i);
            if current {
                marked += 1
            } else {
                free += 1
            }
            if last && !current {
                count_holes += 1
            }
            last = current;
        }
        (count_holes, free, marked)
    }
}

impl Debug for LineMap {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let dwords: [u128; 2] = self.0.into();
        for dword in dwords {
            let lo = (dword as u64).reverse_bits();
            let hi = ((dword >> 64) as u64).reverse_bits();
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

impl PartialEq for BumpBlock {
    fn eq(&self, other: &Self) -> bool {
        self.block.as_ptr() == other.block.as_ptr()
    }
}

impl Eq for BumpBlock {}

impl PartialOrd for BumpBlock {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.block.as_ptr().partial_cmp(&other.block.as_ptr())
    }
}

impl Ord for BumpBlock {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.block.as_ptr().cmp(&other.block.as_ptr())
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

        // decorate line map with cursor range
        let upper_char = (self.cursor / LINE_SIZE_BYTES) / 4;
        let lower_char = (self.lower / LINE_SIZE_BYTES) / 4;
        let length = (self.block.size() / LINE_SIZE_BYTES) / 4;
        let cursor_map: Vec<bool> = (0..length)
            .map(|i| i >= lower_char && i <= upper_char)
            .collect();

        let above: String = cursor_map[0..(length / 2)]
            .iter()
            .map(|b| if *b { '_' } else { ' ' })
            .collect();
        let below: String = cursor_map[(length / 2)..]
            .iter()
            .map(|b| if *b { '‾' } else { ' ' })
            .collect();

        let halfway = below.char_indices().take(1 + (length / 4)).last().unwrap();

        let (q1, q2) = above.split_at(length / 4);
        let (q3, q4) = below.split_at(halfway.0);

        writeln!(f, "  {}   {}", q1, q2)?;
        write!(f, "{:?}", self.line_map)?;
        writeln!(f, "  {}   {}", q3, q4)
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
                    if cfg!(debug_assertions) {
                        self.block.fill(lower, cursor - lower);
                    }

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

    /// Recycle partially used block; reset cursor etc.
    pub fn recycle(&mut self) -> bool {
        if let Some((lower, cursor)) = self.line_map.find_hole(BLOCK_SIZE_BYTES) {
            self.cursor = cursor;
            self.lower = lower;
            true
        } else {
            self.cursor = 0;
            self.lower = 0;
            false
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

    /// Returns (count holes, count free, count marked)
    pub fn stats(&self) -> (usize, usize, usize) {
        self.line_map.stats()
    }
}

#[cfg(test)]
pub mod tests {

    use regex::Regex;

    use super::*;

    #[test]
    pub fn test_find_hole() {
        let mut map = LineMap::default();
        map.mark(0);
        map.mark(10);
        assert_eq!(
            map.find_hole(10 * LINE_SIZE_BYTES),
            Some((LINE_SIZE_BYTES, 9 * LINE_SIZE_BYTES))
        );
    }

    #[test]
    pub fn test_find_hole2() {
        let mut map = LineMap::default();
        map.mark(0);
        map.mark(10);
        map.mark(11);
        map.mark(12);
        map.mark(13);
        map.mark(14);
        map.mark(20);
        assert_eq!(
            map.find_hole(20 * LINE_SIZE_BYTES),
            Some((15 * LINE_SIZE_BYTES, 19 * LINE_SIZE_BYTES))
        );
    }

    #[test]
    pub fn test_find_hole3() {
        let mut map = LineMap::default();
        map.mark(0);
        map.mark(10);
        map.mark(11);
        map.mark(12);
        map.mark(13);
        map.mark(14);
        map.mark(18);
        map.mark(19);
        map.mark(20);
        assert_eq!(
            map.find_hole(20 * LINE_SIZE_BYTES),
            Some((15 * LINE_SIZE_BYTES, 17 * LINE_SIZE_BYTES))
        );
    }

    #[test]
    pub fn test_find_hole4() {
        let mut map = LineMap::default();
        map.mark(0);
        map.mark(10);
        map.mark(11);
        map.mark(12);
        map.mark(13);
        map.mark(14);
        map.mark(18);
        assert_eq!(
            map.find_hole(20 * LINE_SIZE_BYTES),
            Some((15 * LINE_SIZE_BYTES, 17 * LINE_SIZE_BYTES))
        );
    }

    #[test]
    pub fn test_find_hole5() {
        let mut map = LineMap::default();
        map.mark(124);
        map.mark(125);
        map.mark(126);
        map.mark(127);
        assert_eq!(
            map.find_hole(128 * LINE_SIZE_BYTES),
            Some((0, 123 * LINE_SIZE_BYTES))
        );
    }

    #[test]
    pub fn test_count_holes() {
        let mut map = LineMap::default();
        map.mark(5);
        map.mark(10);
        map.mark(100);
        map.mark(200);
        map.mark(201);
        map.mark(202);
        let (count_holes, free, marked) = map.stats();
        assert_eq!(free, 250);
        assert_eq!(marked, 6);
        assert_eq!(count_holes, 5);
    }

    /// Parse a line map from a dump like
    ///
    /// 0xfffffff8ef8c0007 0xfffffffdfdffffff
    /// 0xe1ffffffffffffff 0xffffffffffffffff
    ///
    pub fn linemap_from_dump(dump: &str) -> LineMap {
        let re = Regex::new(r#"0x(\S+) 0x(\S+)\n0x(\S+) 0x(\S+)"#).unwrap();
        if let Some(caps) = re.captures(dump) {
            let components: Vec<_> = caps
                .iter()
                .skip(1)
                .map(|s| u64::from_str_radix(s.unwrap().as_str(), 16).unwrap())
                .map(|u| u.reverse_bits())
                .collect();
            let dword1 = components[0] as u128 | ((components[1] as u128) << 64);
            let dword2 = components[2] as u128 | ((components[3] as u128) << 64);
            LineMap(Bitmap::from([dword1, dword2]))
        } else {
            panic!("can't parse linemap");
        }
    }

    #[test]
    pub fn test_real_linemap() {
        let m = linemap_from_dump(
            "0xe00031f71fffffff 0xffffffbfbfffffff\n0xffffffffffffff87 0xffffffffffffffff",
        );

        if let Some((lo, hi)) = m.find_hole(BLOCK_SIZE_BYTES) {
            assert_eq!(lo, 23680);
            assert_eq!(hi, 24064);
        }
    }
}
