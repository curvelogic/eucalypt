//! Header for all heap objects
//!
//! Currently supports allocation and garbage collection but may
//! assist dynamic typing in future.

use std::ptr::NonNull;

use bitmaps::Bitmap;

#[derive(Debug)]
pub struct HeaderBits(Bitmap<2>);

const MARK_BIT: usize = 0;
const FORWARDED_BIT: usize = 1;

impl HeaderBits {
    fn mark_with_state(&mut self, mark_state: bool) {
        self.0.set(MARK_BIT, mark_state);
    }

    fn unmark_with_state(&mut self, mark_state: bool) {
        self.0.set(MARK_BIT, !mark_state);
    }

    fn is_marked_with_state(&self, mark_state: bool) -> bool {
        self.0.get(MARK_BIT) == mark_state
    }

    /// Create unmarked header bits for the given mark state
    fn new_unmarked(mark_state: bool) -> HeaderBits {
        let mut m = HeaderBits(Bitmap::default());
        m.unmark_with_state(mark_state);
        m
    }

    fn set_forwarded(&mut self) {
        self.0.set(FORWARDED_BIT, true);
    }

    fn clear_forwarded(&mut self) {
        self.0.set(FORWARDED_BIT, false);
    }

    fn is_forwarded(&self) -> bool {
        self.0.get(FORWARDED_BIT)
    }
}

/// Object Header
///
/// Immix requires:
/// - a mark bit (for mark phase)
/// - a forwarded bit (to support evacuation)
/// - forwarding pointer (to support evacuation)
/// - pinning (supported at the block level via `Heap::pin_block` /
///   `Heap::unpin_block`, not per-object)
///
/// On 64-bit targets the natural layout is already 16 bytes. On wasm32 the
/// fields only occupy 12 bytes, and without explicit alignment objects would
/// start at `base + 12` — not 8-byte aligned — causing misaligned pointer
/// panics for types containing `f64`/`u64`. `align(16)` pads the struct to
/// 16 bytes on all targets so that `ptr.offset(-1)` in `get_header` and
/// `ptr.add(size_of::<AllocHeader>())` in `alloc` are always consistent and
/// properly aligned.
#[derive(Debug)]
#[repr(C, align(16))]
pub struct AllocHeader {
    /// Header bits for object state
    bits: HeaderBits,
    /// Count of actual object bytes (excluding header and alignment padding)
    /// This is the size that should be marked during GC, NOT the total allocation size
    alloc_length: u32,
    /// Forwarding pointer for when object is moved
    forwarded_to: Option<NonNull<()>>,
}

impl AllocHeader {
    pub fn new_with_mark_state(byte_length: u32, mark_state: bool) -> Self {
        AllocHeader {
            bits: HeaderBits::new_unmarked(mark_state),
            alloc_length: byte_length,
            forwarded_to: None,
        }
    }

    pub fn mark_with_state(&mut self, mark_state: bool) {
        self.bits.mark_with_state(mark_state)
    }

    pub fn unmark_with_state(&mut self, mark_state: bool) {
        self.bits.unmark_with_state(mark_state)
    }

    pub fn is_marked_with_state(&self, mark_state: bool) -> bool {
        self.bits.is_marked_with_state(mark_state)
    }

    pub fn set_length(&mut self, len: u32) {
        self.alloc_length = len;
    }

    pub fn length(&self) -> u32 {
        self.alloc_length
    }

    pub fn set_forwarded(&mut self, to: NonNull<()>) {
        self.bits.set_forwarded();
        self.forwarded_to = Some(to);
    }

    pub fn clear_forwarded(&mut self) {
        self.bits.clear_forwarded();
        self.forwarded_to = None;
    }

    pub fn is_forwarded(&self) -> bool {
        self.bits.is_forwarded()
    }

    /// Get the forwarding pointer, if set.
    pub fn forwarded_to(&self) -> Option<NonNull<()>> {
        self.forwarded_to
    }
}

#[cfg(test)]
pub mod tests {
    use std::mem::size_of;

    use super::*;

    #[test]
    pub fn test_expected_bitmap_size() {
        assert_eq!(size_of::<HeaderBits>(), 1);
    }

    #[test]
    pub fn test_expected_header_size() {
        // bits: ff 00 00 00 len: ff ff ff ff
        // ptr : ff ff ff ff      ff ff ff ff
        assert_eq!(size_of::<AllocHeader>(), 16);
    }
}
