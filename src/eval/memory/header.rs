//! Header for all heap objects
//!
//! Currently supports allocation and garbage collection but may
//! assist dynamic typing in future.

use std::ptr::NonNull;

use bitmaps::Bitmap;

use super::mark::mark_state;

#[derive(Debug)]
pub struct HeaderBits(Bitmap<2>);

const MARK_BIT: usize = 0;
const FORWARDED_BIT: usize = 1;

impl Default for HeaderBits {
    /// Return 'unmarked' header bits according to the current value
    /// of unmarked (which flips on each collection)
    fn default() -> HeaderBits {
        let mut m = HeaderBits(Bitmap::default());
        m.unmark();
        m
    }
}

impl HeaderBits {
    fn mark(&mut self) {
        self.0.set(MARK_BIT, mark_state());
    }

    fn unmark(&mut self) {
        self.0.set(MARK_BIT, !mark_state());
    }

    fn is_marked(&self) -> bool {
        self.0.get(MARK_BIT) == mark_state()
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
///  - a mark bit (for mark phase)
///  - a forwarded bit (to support evacuation)
///  - forwarding pointer (to support evacuation)
///  - optionally a pinning bit (which we don't support)
#[derive(Default, Debug)]
pub struct AllocHeader {
    /// Header bits for object state
    bits: HeaderBits,
    /// Count of allocated bytes (for untyped allocations)
    alloc_length: u32,
    /// Forwarding pointer for when object is moved
    forwarded_to: Option<NonNull<()>>,
}

impl AllocHeader {
    pub fn new(byte_length: u32) -> Self {
        AllocHeader {
            bits: HeaderBits::default(),
            alloc_length: byte_length,
            forwarded_to: None,
        }
    }

    pub fn mark(&mut self) {
        self.bits.mark()
    }

    pub fn unmark(&mut self) {
        self.bits.unmark()
    }

    pub fn is_marked(&self) -> bool {
        self.bits.is_marked()
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
