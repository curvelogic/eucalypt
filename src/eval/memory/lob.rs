//! A large object allocation
//!
//! A memory region that contains a single object and header

use std::alloc::{alloc, dealloc, Layout};
use std::process::abort;
use std::ptr::NonNull;

/// A memory allocation containing a single large object with its
/// header, this differs from Block in that it needn't be a power of
/// two.
#[derive(Debug)]
pub struct LargeObjectBlock {
    /// Pointer to memory
    ptr: NonNull<u8>,
    /// Size of allocation
    size: usize,
}

impl LargeObjectBlock {
    /// Create a new LargeObjectBlock of size sufficient to contain
    /// `required_size` bytes. Uses efficient sizing to minimize waste
    /// while maintaining reasonable allocation granularity.
    pub fn new(required_size: usize) -> Self {
        let size = Self::efficient_size_for(required_size);
        LargeObjectBlock {
            ptr: Self::alloc_block(size).unwrap_or_else(|_| abort()),
            size,
        }
    }

    /// Allocate a block directly from the system allocator
    fn alloc_block(size: usize) -> Result<NonNull<u8>, ()> {
        // SAFETY: The allocation is valid because:
        // - Layout is constructed with valid size/align via `from_size_align` check
        // - Alignment is clamped to reasonable bounds (8..=4096)
        // - Returned pointer is checked for null before wrapping in NonNull
        // - Memory is exclusively owned by this LargeObjectBlock instance
        // - Debug fill is within bounds (0..size)
        unsafe {
            // Use page alignment for better performance
            let align = std::cmp::max(size.next_power_of_two().min(4096), 8);
            let layout = Layout::from_size_align(size, align).map_err(|_| ())?;
            let ptr = alloc(layout);
            if ptr.is_null() {
                Err(())
            } else {
                if cfg!(debug_assertions) {
                    // Fill memory with 0xff to aid debugging
                    let mem = std::slice::from_raw_parts_mut(ptr, size);
                    mem.fill(0xff);
                }
                Ok(NonNull::new_unchecked(ptr))
            }
        }
    }

    /// Calculate efficient allocation size that minimizes waste while maintaining
    /// reasonable granularity for the underlying allocator.
    ///
    /// Uses a tiered approach:
    /// - Up to 128KB: round to next 16KB boundary (max 15KB waste = ~12%)
    /// - Up to 1MB: round to next 64KB boundary (max 63KB waste = ~6%)  
    /// - Above 1MB: round to next 256KB boundary (max 255KB waste = ~25% max, but rare)
    fn efficient_size_for(required_size: usize) -> usize {
        const KB: usize = 1024;
        const MB: usize = 1024 * KB;

        if required_size <= 128 * KB {
            // Round up to next 16KB boundary
            required_size.div_ceil(16 * KB) * (16 * KB)
        } else if required_size <= MB {
            // Round up to next 64KB boundary
            required_size.div_ceil(64 * KB) * (64 * KB)
        } else {
            // Round up to next 256KB boundary
            required_size.div_ceil(256 * KB) * (256 * KB)
        }
    }

    /// Pointer to the writeable memory area
    pub fn space(&self) -> *const u8 {
        self.ptr.as_ptr()
    }

    /// Get the actual allocated size of this large object block
    pub fn allocated_size(&self) -> usize {
        self.size
    }

    /// Check if this block can accommodate the requested size
    pub fn can_fit(&self, required_size: usize) -> bool {
        self.size >= required_size
    }

    /// Calculate waste percentage for a given required size
    pub fn waste_percentage(&self, required_size: usize) -> f64 {
        if required_size == 0 {
            100.0
        } else {
            let waste = self.size.saturating_sub(required_size);
            (waste as f64 / self.size as f64) * 100.0
        }
    }
}

impl Drop for LargeObjectBlock {
    fn drop(&mut self) {
        // SAFETY: The deallocation is valid because:
        // - `ptr` was allocated by `alloc_block` in this same struct
        // - `size` and alignment calculation match the original allocation
        // - LargeObjectBlock owns this memory exclusively
        // - This is called exactly once during drop
        unsafe {
            let align = std::cmp::max(self.size.next_power_of_two().min(4096), 8);
            let layout = Layout::from_size_align_unchecked(self.size, align);
            dealloc(self.ptr.as_ptr(), layout);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_efficient_size_for_small_range() {
        // Test 16KB boundaries for sizes up to 128KB

        // Exact boundaries should not change
        assert_eq!(LargeObjectBlock::efficient_size_for(16 * 1024), 16 * 1024);
        assert_eq!(LargeObjectBlock::efficient_size_for(32 * 1024), 32 * 1024);
        assert_eq!(LargeObjectBlock::efficient_size_for(128 * 1024), 128 * 1024);

        // Values just above boundaries should round up
        assert_eq!(LargeObjectBlock::efficient_size_for(33 * 1024), 48 * 1024);
        assert_eq!(LargeObjectBlock::efficient_size_for(50 * 1024), 64 * 1024);
        assert_eq!(LargeObjectBlock::efficient_size_for(65 * 1024), 80 * 1024);
    }

    #[test]
    fn test_efficient_size_for_medium_range() {
        // Test 64KB boundaries for 128KB < size <= 1MB

        // Exact boundaries
        assert_eq!(LargeObjectBlock::efficient_size_for(192 * 1024), 192 * 1024);
        assert_eq!(LargeObjectBlock::efficient_size_for(256 * 1024), 256 * 1024);
        assert_eq!(
            LargeObjectBlock::efficient_size_for(1024 * 1024),
            1024 * 1024
        );

        // Round up cases
        assert_eq!(LargeObjectBlock::efficient_size_for(129 * 1024), 192 * 1024);
        assert_eq!(LargeObjectBlock::efficient_size_for(200 * 1024), 256 * 1024);
        assert_eq!(LargeObjectBlock::efficient_size_for(900 * 1024), 960 * 1024);
        // 15 * 64KB
    }

    #[test]
    fn test_efficient_size_for_large_range() {
        // Test 256KB boundaries for size > 1MB

        // Exact boundaries
        assert_eq!(
            LargeObjectBlock::efficient_size_for(1280 * 1024),
            1280 * 1024
        );
        assert_eq!(
            LargeObjectBlock::efficient_size_for(2048 * 1024),
            2048 * 1024
        );

        // Round up cases
        assert_eq!(
            LargeObjectBlock::efficient_size_for(1025 * 1024),
            1280 * 1024
        );
        assert_eq!(
            LargeObjectBlock::efficient_size_for(1500 * 1024),
            1536 * 1024
        );
    }

    #[test]
    fn test_waste_percentage_calculation() {
        let lob = LargeObjectBlock::new(50 * 1024); // Should allocate 64KB

        // Perfect fit should have some waste due to rounding
        let waste = lob.waste_percentage(50 * 1024);
        assert!(waste > 0.0);
        assert!(waste < 30.0); // Should be reasonable

        // Very small allocation in large block should have high waste
        let high_waste = lob.waste_percentage(1024);
        assert!(high_waste > 90.0);

        // Zero size should be 100% waste
        assert_eq!(lob.waste_percentage(0), 100.0);
    }

    #[test]
    fn test_can_fit() {
        let lob = LargeObjectBlock::new(50 * 1024); // Allocates 64KB

        assert!(lob.can_fit(32 * 1024));
        assert!(lob.can_fit(50 * 1024));
        assert!(lob.can_fit(64 * 1024));
        assert!(!lob.can_fit(65 * 1024));
        assert!(!lob.can_fit(100 * 1024));
    }

    #[test]
    fn test_size_efficiency_vs_power_of_two() {
        // Compare our efficient sizing vs power-of-two for various sizes
        let test_sizes = [
            33 * 1024,  // 33KB
            50 * 1024,  // 50KB
            100 * 1024, // 100KB
            200 * 1024, // 200KB
            500 * 1024, // 500KB
        ];

        for &size in &test_sizes {
            let efficient = LargeObjectBlock::efficient_size_for(size);
            let power_of_two = size.next_power_of_two();

            // Our efficient sizing should always be <= power of two
            assert!(efficient <= power_of_two);

            // For most sizes, we should be significantly more efficient
            if size > 32 * 1024 {
                let efficient_waste = (efficient - size) as f64 / efficient as f64;
                let power_waste = (power_of_two - size) as f64 / power_of_two as f64;

                // Our algorithm should generally produce less waste
                assert!(efficient_waste <= power_waste);
            }
        }
    }
}
