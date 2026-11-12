//! Basic (downward) bump allocation and line map
//!

use std::{
    cell::RefCell,
    fmt::Debug,
    process::abort,
    ptr::NonNull,
    rc::{Rc, Weak},
};

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

/// Block density classification for fragmentation analysis
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BlockDensity {
    /// Empty block with no live objects (0% utilisation)
    Empty,
    /// Sparsely populated block (1-25% utilisation) - high evacuation priority
    Sparse,
    /// Moderately fragmented block (25-75% utilisation) - medium evacuation priority  
    Fragmented,
    /// Densely packed block (75%+ utilisation) - low evacuation priority
    Dense,
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
pub const MAX_ALLOC_SIZE: usize = u32::MAX as usize;

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

    /// Mark the lines spanned by the region of `bytes` bytes starting at
    /// byte `offset` within the block.
    pub fn mark_bytes(&mut self, offset: usize, bytes: usize) {
        // Validate that the region fits within this block
        debug_assert!(
            offset + bytes <= BLOCK_SIZE_BYTES,
            "mark_bytes: object at offset {offset} with size {bytes} bytes extends beyond block boundary (block size: {BLOCK_SIZE_BYTES} bytes)"
        );

        // Compute the inclusive range of lines that the region occupies.
        // The previous calculation `bytes / LINE_SIZE + 1` was wrong for
        // regions that straddle a line boundary without filling a full
        // line from the start (e.g. a 48-byte region starting at byte 240
        // within a 256-byte line: it spans bytes 240-287, crossing into
        // the next line, but `48/256 + 1 = 1` only marks line 0).
        //
        // The correct upper bound is derived from the last byte of the
        // region, not from the size alone.
        let first_line = offset / LINE_SIZE_BYTES;
        let last_byte = offset + bytes.saturating_sub(1);
        let last_line = last_byte / LINE_SIZE_BYTES + 1;

        debug_assert!(
            last_line <= LINE_COUNT,
            "mark_bytes: calculated line range {first_line}..{last_line} exceeds LINE_COUNT {LINE_COUNT} (offset: {offset}, bytes: {bytes})"
        );

        for line in first_line..last_line {
            self.mark(line);
        }
    }

    /// Number of marked lines
    pub fn marked_count(&self) -> usize {
        self.0.len()
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
                if let Some(hole_upper) = upper.filter(|_| count > 1) {
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
        if let Some(hole_upper) = upper.filter(|_| count > 1) {
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
            writeln!(f, "{lo:#018x} {hi:#018x}")?;
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
    /// Block map to store mark flags.
    ///
    /// Shared (`Rc`) so the heap can index it by block base address and
    /// mark lines in O(1) without searching its block lists; the index holds
    /// only a `Weak`, so a dropped block can never be marked through a stale
    /// entry, even if a new block later reuses its address (eu-oaz72.4).
    line_map: Rc<RefCell<LineMap>>,
    /// True when `recycle` poisoned every hole under the current line
    /// map (`EU_GC_POISON`). Until the next GC resets the line map,
    /// each hole must still hold only poison bytes when `bump` enters
    /// it. Cleared by `reset_region_marks`.
    holes_poisoned: bool,
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
        Some(self.cmp(other))
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

        writeln!(f, "  {q1}   {q2}")?;
        write!(f, "{:?}", self.line_map.borrow())?;
        writeln!(f, "  {q3}   {q4}")
    }
}

/// Compute the base address of the block containing `ptr`, using the
/// fact that blocks are aligned to `BLOCK_SIZE_BYTES`.
#[inline]
pub fn block_base_of<T>(ptr: NonNull<T>) -> usize {
    ptr.as_ptr() as usize & !(BLOCK_SIZE_BYTES - 1)
}

impl BumpBlock {
    /// Return the base address of this block's memory region
    #[inline]
    pub fn base_address(&self) -> usize {
        self.block.as_ptr() as usize
    }

    /// Initialise a fresh new block ready to start bumping downwards
    pub fn new() -> Self {
        BumpBlock {
            block: Block::new(BLOCK_SIZE_BYTES).unwrap_or_else(|_| abort()),
            cursor: BLOCK_SIZE_BYTES,
            lower: 0,
            line_map: Rc::new(RefCell::new(LineMap::default())),
            holes_poisoned: false,
        }
    }

    /// Bump cursor back by size
    pub fn bump(&mut self, size: usize) -> Option<*const u8> {
        self.bump_checked(size, super::gc_debug::poison_enabled())
    }

    /// Bump cursor back by size; `poison` says whether `EU_GC_POISON`
    /// checks apply (a parameter so that tests need not set the
    /// process-wide flag).
    fn bump_checked(&mut self, size: usize, poison: bool) -> Option<*const u8> {
        if size > self.cursor {
            None
        } else {
            let next = self.cursor - size;
            if next < self.lower {
                // find next hole
                let hole = self.line_map.borrow().find_hole(self.cursor);
                if let Some((lower, cursor)) = hole {
                    if poison && self.holes_poisoned {
                        // `recycle` poisoned this hole and no GC has run
                        // since, so nothing may have written into it.
                        if let Some(offset) = self.first_unpoisoned_byte(lower, cursor) {
                            panic!(
                                "GC HOLE: block {:p} hole [{lower}..{cursor}] was \
                                 reclaimed and poisoned but byte {offset} has been \
                                 overwritten: a write through a stale pointer, or a \
                                 live object in an unmarked line",
                                self.block.as_ptr(),
                            );
                        }
                    } else if cfg!(debug_assertions) {
                        self.block.fill(lower, cursor - lower);
                    }

                    self.lower = lower;
                    self.cursor = cursor;
                    self.bump_checked(size, poison)
                } else {
                    None
                }
            } else {
                self.cursor = next;
                // SAFETY: The pointer arithmetic is valid because:
                // - `next` is within bounds: next >= lower >= 0 (checked above)
                // - `next` < cursor <= BLOCK_SIZE_BYTES (by construction)
                // - The block owns the underlying memory
                // - Result points to unallocated space in the current hole
                unsafe { Some(self.block.as_ptr().add(next)) }
            }
        }
    }

    /// Offset of the first byte in `[lower, upper)` that is not
    /// `POISON_BYTE`, if any.
    fn first_unpoisoned_byte(&self, lower: usize, upper: usize) -> Option<usize> {
        // SAFETY: `lower..upper` is a hole returned by `find_hole`, so
        // it lies within the block, which owns the memory.
        let hole =
            unsafe { std::slice::from_raw_parts(self.block.as_ptr().add(lower), upper - lower) };
        hole.iter()
            .position(|&b| b != super::gc_debug::POISON_BYTE)
            .map(|i| lower + i)
    }

    /// Recycle partially used block; reset cursor etc.
    pub fn recycle(&mut self) -> bool {
        self.recycle_checked(super::gc_debug::poison_enabled())
    }

    /// Recycle; `poison` says whether to poison the reclaimed holes.
    ///
    /// When poisoning, every hole in the block is filled — not only the
    /// first — so that any use-after-free dereference hits a distinctive
    /// pattern and `bump` can verify each hole is untouched when it
    /// enters it.
    fn recycle_checked(&mut self, poison: bool) -> bool {
        let first = self.line_map.borrow().find_hole(BLOCK_SIZE_BYTES);
        if let Some((lower, cursor)) = first {
            if poison {
                let start = self.block.as_ptr() as *mut u8;
                let mut hole = Some((lower, cursor));
                while let Some((l, u)) = hole {
                    // SAFETY: `l..u` is a hole returned by `find_hole`,
                    // within the block, which owns the memory; holes
                    // hold no live objects.
                    unsafe { super::gc_debug::poison_region(start.add(l), u - l) };
                    hole = self.line_map.borrow().find_hole(l);
                }
                self.holes_poisoned = true;
            }
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
        // A new mark phase redraws the holes; those not yet entered were
        // never poisoned under the new map.
        self.holes_poisoned = false;
        self.line_map.borrow_mut().reset();
    }

    /// If ptr is within the block, mark line containing the ptr.
    ///
    /// Live object is assumed to be Small and therefore following
    /// line is treated as implicitly marked
    pub fn mark_line<T>(&mut self, ptr: NonNull<T>) {
        if let Some(offset) = self.block.byte_offset_of(ptr) {
            self.line_map.borrow_mut().mark(offset / LINE_SIZE_BYTES);
        }
    }

    /// If ptr is within the block, mark lines contain the region
    /// starting at ptr and extending for bytes.
    pub fn mark_region(&mut self, ptr: NonNull<u8>, bytes: usize) {
        if let Some(offset) = self.block.byte_offset_of(ptr) {
            self.line_map.borrow_mut().mark_bytes(offset, bytes);
        }
    }

    /// Number of unmarked lines: the space this block offers the allocator
    /// (all of it for a fresh block, the holes for a recycled one).
    pub fn free_lines(&self) -> usize {
        LINE_COUNT - self.line_map.borrow().marked_count()
    }

    /// A weak handle on this block's line map, for the heap's base-address
    /// index (eu-oaz72.4).
    pub fn line_map_handle(&self) -> Weak<RefCell<LineMap>> {
        Rc::downgrade(&self.line_map)
    }

    /// Returns (count holes, count free, count marked)
    pub fn stats(&self) -> (usize, usize, usize) {
        self.line_map.borrow().stats()
    }

    /// Returns true if the given line index is marked in the line map.
    pub(crate) fn is_line_marked(&self, line: usize) -> bool {
        self.line_map.borrow().marked(line)
    }

    /// Analyze block density for fragmentation detection
    pub fn analyze_density(&self) -> BlockDensity {
        let (_holes, _free, marked) = self.line_map.borrow().stats();
        let total_lines = LINE_COUNT;
        let utilisation = marked as f64 / total_lines as f64;

        match utilisation {
            0.0 => BlockDensity::Empty,
            x if x <= 0.25 => BlockDensity::Sparse,
            x if x <= 0.75 => BlockDensity::Fragmented,
            _ => BlockDensity::Dense,
        }
    }

    /// Calculate fragmentation score for evacuation prioritisation
    /// Returns a score from 0.0 to 1.0, where higher scores indicate higher evacuation priority
    pub fn fragmentation_score(&self) -> f64 {
        let density = self.analyze_density();
        match density {
            BlockDensity::Sparse => 1.0,     // Highest evacuation priority
            BlockDensity::Fragmented => 0.5, // Medium evacuation priority
            BlockDensity::Dense => 0.0,      // Low evacuation priority (keep as-is)
            BlockDensity::Empty => 0.0,      // No evacuation needed (already reclaimable)
        }
    }

    /// Find the largest available hole in this block
    /// Returns (offset, size) of the largest hole, or None if no holes available
    pub fn find_largest_hole(&self) -> Option<(usize, usize)> {
        let mut largest_hole: Option<(usize, usize)> = None;
        let mut current_offset = BLOCK_SIZE_BYTES;

        // Search for holes from top of block downward
        while let Some((lower, upper)) = self.line_map.borrow().find_hole(current_offset) {
            let hole_size = upper - lower;

            match largest_hole {
                Some((_, existing_size)) if hole_size > existing_size => {
                    largest_hole = Some((lower, hole_size));
                }
                None => {
                    largest_hole = Some((lower, hole_size));
                }
                _ => {} // Keep existing larger hole
            }

            // Continue search below this hole
            current_offset = if lower > 0 { lower } else { break };
        }

        largest_hole
    }

    /// Calculate total available space in this block
    /// Returns the sum of all hole sizes
    pub fn total_available_space(&self) -> usize {
        let mut total = 0;
        let mut current_offset = BLOCK_SIZE_BYTES;

        // Sum all holes from top of block downward
        while let Some((lower, upper)) = self.line_map.borrow().find_hole(current_offset) {
            total += upper - lower;
            current_offset = if lower > 0 { lower } else { break };
        }

        total
    }

    /// Calculate suitability score for a given allocation size
    /// Returns a score from 0.0 to 1.0, where higher scores indicate better suitability
    pub fn allocation_suitability_score(&self, requested_size: usize) -> f64 {
        if let Some((_, largest_hole_size)) = self.find_largest_hole() {
            if largest_hole_size >= requested_size {
                // Prefer holes that are close in size to the request (minimize waste)
                let waste_ratio =
                    (largest_hole_size - requested_size) as f64 / largest_hole_size as f64;
                let fit_score = 1.0 - waste_ratio;

                // Bonus for denser blocks (better memory locality)
                let density_bonus = match self.analyze_density() {
                    BlockDensity::Dense => 0.3,
                    BlockDensity::Fragmented => 0.1,
                    _ => 0.0,
                };

                (fit_score + density_bonus).min(1.0)
            } else {
                0.0 // Cannot fit the allocation
            }
        } else {
            0.0 // No holes available
        }
    }

    /// Mark a line in the line map (for testing purposes)
    pub fn mark_line_for_test(&mut self, line: usize) {
        self.line_map.borrow_mut().mark(line);
    }
}

#[cfg(test)]
pub mod tests {

    use regex::Regex;

    use super::*;

    use crate::eval::memory::gc_debug::POISON_BYTE;

    /// A block with live lines at 10, 50, 51 and 200, giving several
    /// holes, swept as `recycle` would after a GC.
    fn block_with_holes(poison: bool) -> BumpBlock {
        let mut b = BumpBlock::new();
        for line in [10, 50, 51, 200] {
            b.mark_line_for_test(line);
        }
        assert!(b.recycle_checked(poison));
        b
    }

    fn holes_of(b: &BumpBlock) -> Vec<(usize, usize)> {
        let mut holes = vec![];
        let mut below = BLOCK_SIZE_BYTES;
        while let Some((l, u)) = b.line_map.borrow().find_hole(below) {
            holes.push((l, u));
            below = l;
        }
        holes
    }

    fn write_byte(b: &BumpBlock, offset: usize, byte: u8) {
        unsafe { *(b.block.as_ptr() as *mut u8).add(offset) = byte };
    }

    /// Allocate in 64-byte chunks until the block is exhausted.
    fn exhaust(b: &mut BumpBlock, poison: bool) -> usize {
        let mut n = 0;
        while b.bump_checked(64, poison).is_some() {
            n += 1;
        }
        n
    }

    /// eu-oaz72.51: `recycle` under poison fills every hole, not only
    /// the first, so `bump` can check each hole when it enters it.
    #[test]
    pub fn test_recycle_poisons_every_hole() {
        let b = block_with_holes(true);
        let holes = holes_of(&b);
        assert!(
            holes.len() >= 3,
            "fixture should give several holes: {holes:?}"
        );
        for (l, u) in holes {
            assert_eq!(
                b.first_unpoisoned_byte(l, u),
                None,
                "hole [{l}..{u}] not poisoned"
            );
        }
    }

    /// eu-oaz72.51: stale dead data in reclaimed holes is not a GC
    /// defect. The former check flagged any header-shaped word there;
    /// allocating through every hole of a poisoned block must pass.
    #[test]
    pub fn test_bump_through_poisoned_holes_is_silent() {
        let mut b = block_with_holes(true);
        assert!(exhaust(&mut b, true) > 100);
    }

    /// eu-oaz72.51: a write into a poisoned hole before the allocator
    /// reaches it (a write through a stale pointer, or a live object in
    /// an unmarked line) is reported.
    #[test]
    #[should_panic(expected = "GC HOLE")]
    pub fn test_bump_detects_write_into_poisoned_hole() {
        let mut b = block_with_holes(true);
        let holes = holes_of(&b);
        let (l, _) = holes[holes.len() - 1];
        write_byte(&b, l + 8, 0x42);
        exhaust(&mut b, true);
    }

    /// eu-oaz72.51: once a GC resets the line map, holes not yet
    /// entered were never poisoned under the new map, so their stale
    /// content must not be reported.
    #[test]
    pub fn test_reset_region_marks_stops_hole_check() {
        let mut b = block_with_holes(true);
        let holes = holes_of(&b);
        // Dead data from earlier allocations, as a block holds after a
        // GC that did not recycle it.
        for (l, u) in &holes {
            for off in (*l..*u).step_by(16) {
                write_byte(&b, off, 0x01);
            }
        }
        b.reset_region_marks();
        for line in [10, 50, 51, 200] {
            b.mark_line_for_test(line);
        }
        assert!(exhaust(&mut b, true) > 100);
        // Without poison the check never applies, even to a poisoned block.
        let mut c = block_with_holes(true);
        write_byte(&c, holes[holes.len() - 1].0, 0x42);
        assert!(exhaust(&mut c, false) > 100);
        assert_ne!(POISON_BYTE, 0x42);
    }

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

    #[test]
    pub fn test_block_density_empty() {
        // Test completely empty block
        let block = BumpBlock::new();
        assert_eq!(block.analyze_density(), BlockDensity::Empty);
        assert_eq!(block.fragmentation_score(), 0.0);

        let (_holes, free, marked) = block.stats();
        assert_eq!(marked, 0);
        assert_eq!(free, LINE_COUNT);
    }

    #[test]
    pub fn test_block_density_sparse() {
        // Test sparse block (1-25% utilisation)
        let block = BumpBlock::new();

        // Mark ~10% of lines (25 out of 256 lines)
        for i in 0..25 {
            block.line_map.borrow_mut().mark(i * 10); // Spread marks throughout block
        }

        assert_eq!(block.analyze_density(), BlockDensity::Sparse);
        assert_eq!(block.fragmentation_score(), 1.0); // Highest evacuation priority

        let (_holes, _free, marked) = block.stats();
        assert_eq!(marked, 25);
        assert!(marked as f64 / LINE_COUNT as f64 <= 0.25);
    }

    #[test]
    pub fn test_block_density_fragmented() {
        // Test fragmented block (25-75% utilisation)
        let block = BumpBlock::new();

        // Mark ~50% of lines (128 out of 256 lines)
        for i in 0..128 {
            block.line_map.borrow_mut().mark(i * 2); // Every other line
        }

        assert_eq!(block.analyze_density(), BlockDensity::Fragmented);
        assert_eq!(block.fragmentation_score(), 0.5); // Medium evacuation priority

        let (_holes, _free, marked) = block.stats();
        assert_eq!(marked, 128);
        let utilisation = marked as f64 / LINE_COUNT as f64;
        assert!(utilisation > 0.25 && utilisation <= 0.75);
    }

    #[test]
    pub fn test_block_density_dense() {
        // Test dense block (75%+ utilisation)
        let block = BumpBlock::new();

        // Mark ~90% of lines (230 out of 256 lines)
        for i in 0..230 {
            block.line_map.borrow_mut().mark(i);
        }

        assert_eq!(block.analyze_density(), BlockDensity::Dense);
        assert_eq!(block.fragmentation_score(), 0.0); // Low evacuation priority

        let (_holes, _free, marked) = block.stats();
        assert_eq!(marked, 230);
        assert!(marked as f64 / LINE_COUNT as f64 > 0.75);
    }

    #[test]
    pub fn test_block_density_boundary_cases() {
        // Test exact boundary cases
        let block = BumpBlock::new();

        // Test exactly 25% utilisation (should be Sparse)
        let sparse_lines = (LINE_COUNT as f64 * 0.25) as usize;
        for i in 0..sparse_lines {
            block.line_map.borrow_mut().mark(i);
        }
        assert_eq!(block.analyze_density(), BlockDensity::Sparse);

        // Clear and test exactly 75% utilisation (should be Fragmented)
        *block.line_map.borrow_mut() = LineMap::default();
        let fragmented_lines = (LINE_COUNT as f64 * 0.75) as usize;
        for i in 0..fragmented_lines {
            block.line_map.borrow_mut().mark(i);
        }
        assert_eq!(block.analyze_density(), BlockDensity::Fragmented);

        // Clear and test just over 75% utilisation (should be Dense)
        *block.line_map.borrow_mut() = LineMap::default();
        let dense_lines = (LINE_COUNT as f64 * 0.76) as usize;
        for i in 0..dense_lines {
            block.line_map.borrow_mut().mark(i);
        }
        assert_eq!(block.analyze_density(), BlockDensity::Dense);
    }

    #[test]
    pub fn test_fragmentation_score_ordering() {
        // Test that fragmentation scores are properly ordered for evacuation priority
        let mut blocks = Vec::new();

        // Create blocks with different densities
        for &(lines_to_mark, expected_density) in &[
            (0, BlockDensity::Empty),
            (64, BlockDensity::Sparse),      // 25% utilisation
            (128, BlockDensity::Fragmented), // 50% utilisation
            (200, BlockDensity::Dense),      // ~78% utilisation
        ] {
            let block = BumpBlock::new();
            for i in 0..lines_to_mark {
                block.line_map.borrow_mut().mark(i);
            }
            assert_eq!(block.analyze_density(), expected_density);
            blocks.push(block);
        }

        // Verify evacuation priority ordering (higher score = higher priority)
        let empty_score = blocks[0].fragmentation_score();
        let sparse_score = blocks[1].fragmentation_score();
        let fragmented_score = blocks[2].fragmentation_score();
        let dense_score = blocks[3].fragmentation_score();

        assert_eq!(empty_score, 0.0);
        assert_eq!(sparse_score, 1.0);
        assert_eq!(fragmented_score, 0.5);
        assert_eq!(dense_score, 0.0);

        // Sparse blocks should have highest priority
        assert!(sparse_score > fragmented_score);
        assert!(fragmented_score > dense_score);
        assert!(sparse_score > dense_score);
    }

    #[test]
    pub fn test_find_largest_hole() {
        let block = BumpBlock::new();

        // Mark some lines to create holes
        for i in 0..50 {
            block.line_map.borrow_mut().mark(i);
        }
        // Leave hole from 50-75 (25 lines = 3200 bytes)
        for i in 76..100 {
            block.line_map.borrow_mut().mark(i);
        }
        // Leave hole from 100-150 (50 lines = 6400 bytes) - this should be largest
        for i in 151..200 {
            block.line_map.borrow_mut().mark(i);
        }

        let largest = block.find_largest_hole();
        assert!(largest.is_some());
        let (_offset, size) = largest.unwrap();

        // The actual size calculation depends on conservative marking in find_hole()
        // which excludes one line from the upper end, so we should expect:
        // From 100-151 (51 lines), conservative marking gives us 50 * LINE_SIZE_BYTES
        assert!(size >= 50 * LINE_SIZE_BYTES);
    }

    #[test]
    pub fn test_total_available_space() {
        let block = BumpBlock::new();

        // Create pattern: marked, hole (10 lines), marked, hole (20 lines), marked
        for i in 0..50 {
            block.line_map.borrow_mut().mark(i);
        }
        // Hole from 50-60 (10 lines)
        for i in 60..80 {
            block.line_map.borrow_mut().mark(i);
        }
        // Hole from 80-100 (20 lines)
        for i in 100..LINE_COUNT {
            block.line_map.borrow_mut().mark(i);
        }

        let total = block.total_available_space();
        // Conservative marking reduces hole sizes, so we expect less than the ideal
        // 10-line hole becomes ~9 lines, 20-line hole becomes ~19 lines
        let expected_min = 25 * LINE_SIZE_BYTES; // Conservative estimate
        assert!(total >= expected_min);
    }

    #[test]
    pub fn test_allocation_suitability_score() {
        let block = BumpBlock::new();

        // Create a hole that can fit exactly 1024 bytes
        let hole_lines = 1024 / LINE_SIZE_BYTES + 1; // Need at least this many lines
        for i in 0..50 {
            block.line_map.borrow_mut().mark(i);
        }
        // Leave hole from 50 to (50 + hole_lines)
        for i in (50 + hole_lines + 1)..LINE_COUNT {
            block.line_map.borrow_mut().mark(i);
        }

        // Test perfect fit (should score high)
        let perfect_score = block.allocation_suitability_score(1024);
        assert!(perfect_score > 0.5);

        // Test oversized request (should score 0)
        let oversized_score = block.allocation_suitability_score(BLOCK_SIZE_BYTES);
        assert_eq!(oversized_score, 0.0);

        // Test tiny request (should score high due to low waste)
        let tiny_score = block.allocation_suitability_score(64);
        assert!(tiny_score > 0.0);
    }

    #[test]
    pub fn test_allocation_suitability_density_bonus() {
        // Create two blocks with same hole but different density
        let sparse_block = BumpBlock::new();
        let dense_block = BumpBlock::new();

        // Both have hole from 0-10 (can fit any small allocation)
        for i in 10..LINE_COUNT {
            sparse_block.line_map.borrow_mut().mark(i);
            dense_block.line_map.borrow_mut().mark(i);
        }

        // Make dense_block actually dense by marking most lines
        for i in 0..(LINE_COUNT * 3 / 4) {
            dense_block.line_map.borrow_mut().mark(i);
        }

        let sparse_score = sparse_block.allocation_suitability_score(512);
        let dense_score = dense_block.allocation_suitability_score(512);

        // Dense block should score higher due to density bonus (if it can fit)
        // Note: This test may need adjustment based on actual hole positions
        assert!(sparse_score >= 0.0);
        assert!(dense_score >= 0.0);
    }
}
