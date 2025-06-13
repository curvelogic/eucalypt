//! The STG heap implementation

use std::collections::LinkedList;
use std::fmt::Debug;
use std::ptr::NonNull;
use std::sync::atomic::{AtomicBool, Ordering::SeqCst};
use std::time::{Duration, Instant};
use std::{cell::UnsafeCell, mem::size_of};
use std::{ptr::write, slice::from_raw_parts_mut};

use super::bump::{BLOCK_SIZE_BYTES, MAX_ALLOC_SIZE};
use super::{
    alloc::{Allocator, MutatorScope},
    bump::{self, BumpBlock},
    header::AllocHeader,
    lob::LargeObjectBlock,
};

#[derive(Debug)]
pub struct HeapStats {
    /// Number of standard blocks allocated
    pub blocks_allocated: usize,
    /// Number of large objects allocated
    pub lobs_allocated: usize,
    /// Number of blocks used and not reclaimed
    pub used: usize,
    /// Number of blocks used and recycled
    pub recycled: usize,
}

/// Object size class.
#[repr(u8)]
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum SizeClass {
    /// Small objects fit inside a line
    Small,
    /// Medium objects span lines inside a block
    Medium,
    /// Large objects are larger than a normal block
    Large,
}

impl SizeClass {
    pub fn for_size(object_size: usize) -> SizeClass {
        if object_size < bump::LINE_SIZE_BYTES {
            SizeClass::Small
        } else if object_size < bump::BLOCK_SIZE_BYTES {
            SizeClass::Medium
        } else {
            SizeClass::Large
        }
    }
}

/// Simple heap of bump blocks with no reclaim for now
pub struct HeapState {
    /// For allocating small objects
    head: Option<BumpBlock>,
    /// For allocating medium objects
    overflow: Option<BumpBlock>,
    /// Recycled - part used but reclaimed
    recycled: LinkedList<BumpBlock>,
    /// Part used - not yet reclaimed
    rest: LinkedList<BumpBlock>,
    /// Large object blocks - each contains single object
    lobs: Vec<LargeObjectBlock>,
}

impl Default for HeapState {
    fn default() -> Self {
        HeapState::new()
    }
}

impl std::fmt::Debug for HeapState {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for block in &self.rest {
            writeln!(f, "(XX) {:?}", block)?;
        }

        for block in &self.recycled {
            writeln!(f, "(Cy) {:?}", block)?;
        }

        if let Some(head) = &self.head {
            writeln!(f, "(Hd) {:?}", head)?;
        }

        if let Some(of) = &self.overflow {
            writeln!(f, "(Ov) Overflow:\n\n{:?}", of)?;
        }

        for lob in &self.lobs {
            writeln!(f, "{:?}", lob)?;
        }

        writeln!(f, "\n")
    }
}

impl HeapState {
    pub fn new() -> Self {
        HeapState {
            head: None,
            overflow: None,
            recycled: LinkedList::default(),
            rest: LinkedList::default(),
            lobs: vec![],
        }
    }

    /// Get head block, creating if necessary
    pub fn head(&mut self) -> &mut BumpBlock {
        if self.head.is_none() {
            self.head = Some(BumpBlock::new());
        }

        self.head.as_mut().unwrap()
    }

    /// Get overflow block, creating if necessary
    pub fn overflow(&mut self) -> &mut BumpBlock {
        if self.overflow.is_none() {
            self.overflow = Some(BumpBlock::new());
        }

        self.overflow.as_mut().unwrap()
    }

    pub fn replace_head(&mut self) -> &mut BumpBlock {
        let replacement = self.recycled.pop_front().unwrap_or_default();

        self.head.replace(replacement).and_then(|old| {
            self.rest.push_back(old);
            None as Option<BumpBlock>
        });
        self.head.as_mut().unwrap()
    }

    pub fn replace_overflow(&mut self) -> &mut BumpBlock {
        self.overflow.replace(BumpBlock::new()).and_then(|old| {
            self.rest.push_back(old);
            None as Option<BumpBlock>
        });
        self.overflow.as_mut().unwrap()
    }

    /// Create and return a new large object block able to store data
    /// of the specified size
    pub fn lob(&mut self, size: usize) -> &mut LargeObjectBlock {
        self.lobs.push(LargeObjectBlock::new(size));
        self.lobs.last_mut().unwrap()
    }

    /// Look for reclaimable blocks and move to recycled list
    pub fn sweep(&mut self) {
        let mut unusable: LinkedList<BumpBlock> = LinkedList::default();

        while let Some(mut block) = self.rest.pop_front() {
            if block.recycle() {
                self.recycled.push_back(block);
            } else {
                unusable.push_back(block);
            }
        }

        self.rest.append(&mut unusable);
    }

    /// Statistics
    pub fn stats(&self) -> HeapStats {
        HeapStats {
            blocks_allocated: self.rest.len()
                + self.recycled.len()
                + self.head.iter().count()
                + self.overflow.iter().count(),
            lobs_allocated: self.lobs.len(),
            used: self.rest.len(),
            recycled: self.recycled.len(),
        }
    }
}

/// Detailed heap context for error diagnostics
#[derive(Debug, Clone, PartialEq)]
pub struct HeapContext {
    /// Total blocks allocated
    pub blocks_allocated: usize,
    /// Blocks in use (not reclaimed)
    pub blocks_used: usize,
    /// Blocks recycled and available for reuse
    pub blocks_recycled: usize,
    /// Large object blocks allocated
    pub lobs_allocated: usize,
    /// Requested allocation size that failed
    pub requested_size: usize,
    /// Size class of the failed allocation
    pub size_class: SizeClass,
    /// Current head block utilisation percentage (if exists)
    pub head_utilisation_percent: Option<f64>,
    /// Current overflow block utilisation percentage (if exists)
    pub overflow_utilisation_percent: Option<f64>,
    /// Emergency collection attempts made
    pub emergency_collections_attempted: bool,
    /// Heap limit (if configured)
    pub heap_limit: Option<usize>,
}

impl HeapContext {
    /// Calculate memory utilisation percentage
    pub fn memory_utilisation_percent(&self) -> f64 {
        if self.blocks_allocated == 0 {
            0.0
        } else {
            (self.blocks_used as f64 / self.blocks_allocated as f64) * 100.0
        }
    }

    /// Calculate fragmentation indicator (lower recycled ratio = more fragmented)
    pub fn fragmentation_indicator(&self) -> f64 {
        if self.blocks_allocated == 0 {
            0.0
        } else {
            (self.blocks_recycled as f64 / self.blocks_allocated as f64) * 100.0
        }
    }

    /// Total heap size in bytes
    pub fn total_heap_bytes(&self) -> usize {
        self.blocks_allocated * BLOCK_SIZE_BYTES
    }

    /// Used heap size in bytes
    pub fn used_heap_bytes(&self) -> usize {
        self.blocks_used * BLOCK_SIZE_BYTES
    }
}

/// Heap-level errors for memory allocation and management with detailed context
#[derive(Debug, Clone)]
pub enum HeapError {
    /// Out of memory - no more blocks can be allocated
    OutOfMemory { context: HeapContext },
    /// Emergency collection failed to free sufficient memory
    EmergencyCollectionFailed { context: HeapContext },
    /// Invalid allocation size requested
    InvalidAllocationSize { 
        requested_size: usize,
        max_size: usize,
    },
    /// Heap fragmentation prevents allocation despite available memory
    FragmentationError { context: HeapContext },
    /// Block allocation failure from underlying allocator
    BlockAllocationFailed { context: HeapContext },
    /// Emergency collection was attempted but failed to free sufficient memory
    EmergencyCollectionInsufficient { context: HeapContext },
}

impl std::fmt::Display for HeapError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            HeapError::OutOfMemory { context } => {
                write!(
                    f,
                    "out of memory: failed to allocate {} bytes ({:?}) | heap: {:.1}% used ({}/{} blocks), {:.1}% fragmented, {} LOBs{}{}",
                    context.requested_size,
                    context.size_class,
                    context.memory_utilisation_percent(),
                    context.blocks_used,
                    context.blocks_allocated,
                    context.fragmentation_indicator(),
                    context.lobs_allocated,
                    if let Some(limit) = context.heap_limit {
                        format!(" | limit: {} blocks", limit)
                    } else {
                        " | no limit".to_string()
                    },
                    if context.emergency_collections_attempted {
                        " | emergency collection attempted"
                    } else {
                        ""
                    }
                )
            }
            HeapError::EmergencyCollectionFailed { context } => {
                write!(
                    f,
                    "emergency collection failed: could not free sufficient memory for {} bytes ({:?}) | heap: {:.1}% used ({}/{} blocks), {:.1}% fragmented{}",
                    context.requested_size,
                    context.size_class,
                    context.memory_utilisation_percent(),
                    context.blocks_used,
                    context.blocks_allocated,
                    context.fragmentation_indicator(),
                    if let Some(head_util) = context.head_utilisation_percent {
                        format!(" | head: {:.1}% utilised", head_util)
                    } else {
                        "".to_string()
                    }
                )
            }
            HeapError::InvalidAllocationSize { requested_size, max_size } => {
                write!(
                    f,
                    "invalid allocation size: requested {} bytes exceeds maximum {} bytes",
                    requested_size,
                    max_size
                )
            }
            HeapError::FragmentationError { context } => {
                write!(
                    f,
                    "heap fragmentation prevents allocation: {} bytes ({:?}) cannot fit | heap: {:.1}% used, {:.1}% fragmented ({} recycled/{} total blocks){}",
                    context.requested_size,
                    context.size_class,
                    context.memory_utilisation_percent(),
                    context.fragmentation_indicator(),
                    context.blocks_recycled,
                    context.blocks_allocated,
                    if let Some(head_util) = context.head_utilisation_percent {
                        format!(" | head: {:.1}% utilised", head_util)
                    } else {
                        "".to_string()
                    }
                )
            }
            HeapError::BlockAllocationFailed { context } => {
                write!(
                    f,
                    "block allocation failed: underlying allocator error | heap: {:.1}% used ({}/{} blocks), {} LOBs | attempting {} bytes ({:?})",
                    context.memory_utilisation_percent(),
                    context.blocks_used,
                    context.blocks_allocated,
                    context.lobs_allocated,
                    context.requested_size,
                    context.size_class
                )
            }
            HeapError::EmergencyCollectionInsufficient { context } => {
                write!(
                    f,
                    "emergency collection insufficient: freed space but not enough for {} bytes ({:?}) | heap: {:.1}% used, {:.1}% fragmented{}",
                    context.requested_size,
                    context.size_class,
                    context.memory_utilisation_percent(),
                    context.fragmentation_indicator(),
                    if let Some(head_util) = context.head_utilisation_percent {
                        format!(" | head: {:.1}% utilised", head_util)
                    } else {
                        "".to_string()
                    }
                )
            }
        }
    }
}

impl std::error::Error for HeapError {}

impl From<super::bump::AllocError> for HeapError {
    fn from(e: super::bump::AllocError) -> Self {
        match e {
            super::bump::AllocError::OOM => HeapError::OutOfMemory { 
                context: HeapContext {
                    blocks_allocated: 0,
                    blocks_used: 0,
                    blocks_recycled: 0,
                    lobs_allocated: 0,
                    requested_size: 0,
                    size_class: SizeClass::Small,
                    head_utilisation_percent: None,
                    overflow_utilisation_percent: None,
                    emergency_collections_attempted: false,
                    heap_limit: None,
                }
            },
            super::bump::AllocError::BadRequest => HeapError::InvalidAllocationSize { 
                requested_size: 0,
                max_size: MAX_ALLOC_SIZE,
            },
        }
    }
}

/// Emergency collection state to prevent infinite loops and track collection attempts
#[derive(Debug, Clone)]
enum EmergencyState {
    /// Normal operation, emergency collection allowed
    Normal,
    /// Currently performing emergency collection (reentrancy guard)
    InEmergencyCollection,
    /// Recent emergency collection performed (cooldown period)
    RecentEmergencyCollection { 
        performed_at: Instant,
        cooldown_duration: Duration,
    },
}

impl EmergencyState {
    fn new() -> Self {
        EmergencyState::Normal
    }

    fn can_attempt_emergency_collection(&self) -> bool {
        match self {
            EmergencyState::Normal => true,
            EmergencyState::InEmergencyCollection => false,
            EmergencyState::RecentEmergencyCollection { performed_at, cooldown_duration } => {
                performed_at.elapsed() > *cooldown_duration
            }
        }
    }

    fn start_emergency_collection(&mut self) {
        *self = EmergencyState::InEmergencyCollection;
    }

    fn complete_emergency_collection(&mut self) {
        *self = EmergencyState::RecentEmergencyCollection {
            performed_at: Instant::now(),
            cooldown_duration: Duration::from_millis(100), // 100ms cooldown
        };
    }
}

/// A heap (with interior mutability)
pub struct Heap {
    state: UnsafeCell<HeapState>,
    limit: Option<usize>,
    /// Mark state for this heap instance - flipped each collection to avoid clearing marks
    mark_state: AtomicBool,
    /// Emergency collection state tracking
    emergency_state: UnsafeCell<EmergencyState>,
}

#[cfg(test)]
mod oom_tests {
    use super::*;
    use crate::eval::memory::{mutator::MutatorHeapView, syntax::StgBuilder};

    #[test]
    fn test_allocation_returns_results() {
        // This test verifies that allocation methods return Results instead of panicking
        let heap = Heap::new();
        let view = MutatorHeapView::new(&heap);

        // Test that string allocation returns a Result
        let result = view.str("test string");
        assert!(
            result.is_ok(),
            "String allocation should succeed for reasonable sizes"
        );

        // Test that unit allocation returns a Result
        let result = view.unit();
        assert!(result.is_ok(), "Unit allocation should succeed");

        println!("✅ All allocation methods return Results instead of panicking");
    }

    #[test]
    fn test_find_space_returns_result() {
        // Test that the low-level find_space method returns Result instead of panicking
        let heap = Heap::new();

        // Test normal allocation
        let result = heap.find_space(64);
        assert!(result.is_ok(), "Small allocation should succeed");

        // Test allocation that should exceed block size limits
        let result = heap.find_space(BLOCK_SIZE_BYTES * 2); // Larger than single block
                                                            // This should either succeed or return HeapError, not panic
        match result {
            Ok(_) => println!("Large allocation succeeded (system has lots of memory)"),
            Err(HeapError::OutOfMemory { .. }) => {
                println!("Large allocation failed gracefully with OutOfMemory")
            }
            Err(e) => println!("Large allocation failed gracefully: {:?}", e),
        }

        println!("✅ find_space returns HeapError Results instead of panicking");
    }

    #[test]
    fn test_heap_error_conversions() {
        // Test HeapError conversions
        use super::bump::AllocError;

        // Test bump error conversion
        let heap_err: HeapError = AllocError::OOM.into();
        assert!(matches!(heap_err, HeapError::OutOfMemory { .. }));

        let heap_err: HeapError = AllocError::BadRequest.into();
        assert!(matches!(heap_err, HeapError::InvalidAllocationSize { .. }));

        // Test ExecutionError conversion - create a proper HeapError with context
        use crate::eval::error::ExecutionError;
        let heap = Heap::new();
        let heap_err = heap.out_of_memory_error(1024, false);
        let exec_err: ExecutionError = heap_err.into();
        assert!(matches!(exec_err, ExecutionError::AllocationError));

        println!("✅ HeapError conversions working correctly");
    }

    #[test]
    fn test_emergency_collection_state_tracking() {
        // Test that emergency collection state prevents infinite loops
        let heap = Heap::new();
        
        // Verify initial state allows emergency collection
        let emergency_state = unsafe { &*heap.emergency_state.get() };
        assert!(emergency_state.can_attempt_emergency_collection());
        
        // Simulate emergency collection attempt (this will fail but should update state)
        let result = heap.attempt_emergency_collection(1024);
        assert!(result.is_err());
        assert!(matches!(result.unwrap_err(), HeapError::EmergencyCollectionInsufficient { .. }));
        
        // After emergency collection, there should be a cooldown period
        let emergency_state = unsafe { &*heap.emergency_state.get() };
        assert!(!emergency_state.can_attempt_emergency_collection());
        
        println!("✅ Emergency collection state tracking works correctly");
    }

    #[test]
    fn test_emergency_collection_reentrancy_protection() {
        // Test that emergency collection cannot be called recursively
        let heap = Heap::new();
        
        // Manually set state to InEmergencyCollection
        let emergency_state = unsafe { &mut *heap.emergency_state.get() };
        emergency_state.start_emergency_collection();
        
        // Verify emergency collection is not allowed
        assert!(!emergency_state.can_attempt_emergency_collection());
        
        println!("✅ Emergency collection reentrancy protection works correctly");
    }

    #[test]
    fn test_emergency_collection_implementation() {
        // Test that emergency collection can free space
        let heap = Heap::new();
        
        // Force allocation of multiple blocks by allocating large amounts
        let heap_state = unsafe { &mut *heap.state.get() };
        let _block1 = heap_state.head(); // Forces first block allocation
        let _block2 = heap_state.overflow(); // Forces second block allocation
        
        // Move blocks to "rest" list by replacing them
        heap_state.replace_head();
        heap_state.replace_overflow();
        
        let stats_before = heap_state.stats();
        eprintln!("Before emergency collection: {} blocks allocated, {} in rest, {} recycled", 
                 stats_before.blocks_allocated, stats_before.used, stats_before.recycled);
        
        // Now attempt emergency collection
        let result = heap.attempt_emergency_collection(1024);
        
        let stats_after = heap.stats();
        eprintln!("After emergency collection: {} blocks allocated, {} in rest, {} recycled", 
                 stats_after.blocks_allocated, stats_after.used, stats_after.recycled);
        
        // Emergency collection should have attempted to free space
        // Even if it doesn't succeed (because blocks might not be reclaimable), 
        // it should not crash and should provide proper error reporting
        
        match result {
            Ok(()) => {
                println!("✅ Emergency collection successfully freed space");
                assert!(stats_after.recycled > stats_before.recycled, 
                       "Should have recycled some blocks");
            }
            Err(HeapError::EmergencyCollectionInsufficient { .. }) => {
                println!("✅ Emergency collection completed but insufficient space freed (expected)");
                // This is fine - the emergency collection ran but couldn't free enough space
            }
            Err(e) => {
                panic!("Unexpected error from emergency collection: {:?}", e);
            }
        }
        
        // Verify cooldown is in effect
        let emergency_state = unsafe { &*heap.emergency_state.get() };
        assert!(!emergency_state.can_attempt_emergency_collection(), 
               "Should be in cooldown period after emergency collection");
    }

    #[test]
    fn test_enhanced_error_diagnostics() {
        // Test that enhanced error messages include detailed heap context
        let heap = Heap::new();
        
        // Create a heap context error (64 bytes = Small, 1024 bytes = Medium)
        let error = heap.out_of_memory_error(64, false);
        let error_message = format!("{}", error);
        
        // Print the actual error message to see its format
        println!("Enhanced error message: {}", error_message);
        
        // Verify the error message includes detailed diagnostic information
        assert!(error_message.contains("64 bytes"));
        assert!(error_message.contains("Small"));  // Size class
        assert!(error_message.contains("% used"));
        assert!(error_message.contains("% fragmented"));
        assert!(error_message.contains("no limit"));
        
        // Test InvalidAllocationSize error
        let invalid_error = HeapError::InvalidAllocationSize {
            requested_size: u32::MAX as usize + 1,
            max_size: u32::MAX as usize,
        };
        let invalid_message = format!("{}", invalid_error);
        assert!(invalid_message.contains("exceeds maximum"));
        assert!(invalid_message.contains(&format!("{}", u32::MAX as usize)));
        
        println!("Invalid allocation error: {}", invalid_message);
        
        // Test emergency collection insufficient error with context
        let emergency_error = heap.emergency_collection_insufficient_error(2048);
        let emergency_message = format!("{}", emergency_error);
        assert!(emergency_message.contains("2048 bytes"));
        assert!(emergency_message.contains("emergency collection insufficient"));
        
        println!("Emergency collection error: {}", emergency_message);
        
        println!("✅ Enhanced error diagnostics provide detailed context");
    }

    #[test]
    fn demonstrate_enhanced_diagnostics() {
        println!("\n=== Enhanced Error Diagnostics Demonstration ===");
        
        use crate::eval::memory::syntax::Ref;
        
        // Scenario 1: Fresh heap - clean state
        println!("\n🔍 Scenario 1: Fresh Heap Out-of-Memory");
        let heap = Heap::new();
        let fresh_error = heap.out_of_memory_error(1024, false);
        println!("   {}", fresh_error);
        
        // Scenario 2: Heap with limit - realistic constraint
        println!("\n🔍 Scenario 2: Limited Heap with Allocated Objects");
        let limited_heap = Heap::with_limit(1); // 1 MiB limit = 32 blocks
        
        // Allocate several objects to create fragmentation
        for i in 0..10 {
            let _ = limited_heap.alloc(Ref::num(i));
        }
        let _ = limited_heap.alloc_bytes(1024); // Medium allocation
        
        let limited_error = limited_heap.out_of_memory_error(512, false);
        println!("   {}", limited_error);
        
        // Scenario 3: Emergency collection attempted
        println!("\n🔍 Scenario 3: Emergency Collection Scenario");
        let emergency_error = limited_heap.emergency_collection_insufficient_error(2048);
        println!("   {}", emergency_error);
        
        // Scenario 4: Invalid allocation sizes
        println!("\n🔍 Scenario 4: Invalid Allocation Sizes");
        
        let invalid_too_large = HeapError::InvalidAllocationSize {
            requested_size: u32::MAX as usize + 1,
            max_size: u32::MAX as usize,
        };
        println!("   Large: {}", invalid_too_large);
        
        let invalid_zero = HeapError::InvalidAllocationSize {
            requested_size: 0,
            max_size: MAX_ALLOC_SIZE,
        };
        println!("   Zero:  {}", invalid_zero);
        
        // Scenario 5: Fragmentation scenarios
        println!("\n🔍 Scenario 5: Fragmentation Analysis");
        
        // Simulate a fragmented heap
        let frag_heap = Heap::new();
        for _ in 0..5 {
            let _ = frag_heap.alloc(Ref::num(42));
        }
        
        let frag_error = frag_heap.fragmentation_error(8192);
        println!("   {}", frag_error);
        
        // Scenario 6: Size class examples
        println!("\n🔍 Scenario 6: Different Size Classes");
        
        let small_error = heap.out_of_memory_error(64, false);  // Small (< 128 bytes)
        println!("   Small:  {}", small_error);
        
        let medium_error = heap.out_of_memory_error(1024, true); // Medium (128B - 32KB)
        println!("   Medium: {}", medium_error);
        
        let large_error = heap.out_of_memory_error(40960, true); // Large (> 32KB)
        println!("   Large:  {}", large_error);
        
        // Show actual heap stats for context
        println!("\n📊 Heap Statistics:");
        let stats = limited_heap.stats();
        println!("   - Total blocks: {} ({} KB)", stats.blocks_allocated, stats.blocks_allocated * 32);
        println!("   - Used blocks:  {} ({:.1}%)", stats.used, 
                (stats.used as f64 / stats.blocks_allocated as f64) * 100.0);
        println!("   - Recycled:     {} ({:.1}%)", stats.recycled,
                (stats.recycled as f64 / stats.blocks_allocated as f64) * 100.0);
        println!("   - Large objects: {}", stats.lobs_allocated);
        
        println!("\n=== End Demonstration ===\n");
    }
}

impl MutatorScope for Heap {}

impl Debug for Heap {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        unsafe { (*self.state.get()).fmt(f) }
    }
}

impl Default for Heap {
    fn default() -> Self {
        Heap::new()
    }
}

impl Heap {
    pub fn new() -> Self {
        Heap {
            state: UnsafeCell::new(HeapState::new()),
            limit: None,
            mark_state: AtomicBool::new(false),
            emergency_state: UnsafeCell::new(EmergencyState::new()),
        }
    }

    pub fn with_limit(limit_mib: usize) -> Self {
        let block_limit = (limit_mib * 1_048_576) / BLOCK_SIZE_BYTES;
        println!("block limit {:?}", block_limit);
        Heap {
            state: UnsafeCell::new(HeapState::new()),
            limit: Some(block_limit),
            mark_state: AtomicBool::new(false),
            emergency_state: UnsafeCell::new(EmergencyState::new()),
        }
    }

    pub fn stats(&self) -> HeapStats {
        unsafe { (*self.state.get()).stats() }
    }

    /// Get the current mark state for this heap
    pub fn mark_state(&self) -> bool {
        self.mark_state.load(SeqCst)
    }

    /// Flip the mark state for this heap (called after each collection)
    pub fn flip_mark_state(&self) {
        self.mark_state.fetch_xor(true, SeqCst);
    }

    pub fn policy_requires_collection(&self) -> bool {
        if let Some(limit) = self.limit {
            let stats = self.stats();
            stats.blocks_allocated >= limit
                && (stats.recycled as f32 / stats.blocks_allocated as f32) < 0.25
        } else {
            false
        }
    }
}

impl Allocator for Heap {
    /// Allocate an object into the backing bump blocks
    fn alloc<T>(&self, object: T) -> Result<NonNull<T>, HeapError>
    where
        T: super::alloc::StgObject,
    {
        let header_size = size_of::<AllocHeader>();
        let object_size = size_of::<T>();
        let alloc_size = Self::alloc_size_of(header_size + object_size);

        let space = self.find_space(alloc_size)?;

        let header = AllocHeader::new_with_mark_state(0, self.mark_state());

        unsafe {
            write(space as *mut AllocHeader, header);
            let object_space = space.add(header_size);
            write(object_space as *mut T, object);
            Ok(NonNull::new_unchecked(object_space as *mut T))
        }
    }

    /// Allocate a region of bytes (for array / vector implementations)
    fn alloc_bytes(&self, size_bytes: usize) -> Result<NonNull<u8>, HeapError> {
        if size_bytes > u32::MAX as usize {
            return Err(HeapError::InvalidAllocationSize {
                requested_size: size_bytes,
                max_size: u32::MAX as usize,
            });
        }

        let header_size = size_of::<AllocHeader>();
        let alloc_size = Self::alloc_size_of(header_size + size_bytes);

        let space = self.find_space(alloc_size)?;

        let header = AllocHeader::new_with_mark_state(alloc_size as u32, self.mark_state());

        unsafe {
            write(space as *mut AllocHeader, header);
            let array_space = space.add(header_size);
            let array = from_raw_parts_mut(array_space as *mut u8, size_bytes);
            for byte in array {
                *byte = 0
            }
            Ok(NonNull::new_unchecked(array_space as *mut u8))
        }
    }

    /// Get header from object pointer
    fn get_header<T>(&self, object: NonNull<T>) -> NonNull<AllocHeader> {
        let header_ptr =
            unsafe { NonNull::new_unchecked(object.cast::<AllocHeader>().as_ptr().offset(-1)) };
        debug_assert!(header_ptr.as_ptr() as usize > 0);
        header_ptr
    }

    /// Get object from header pointer
    fn get_object(&self, header: NonNull<AllocHeader>) -> NonNull<()> {
        unsafe { NonNull::new_unchecked(header.as_ptr().offset(1)).cast::<()>() }
    }
}

impl Heap {
    /// Allocate space, with emergency collection on failure
    fn find_space(&self, size_bytes: usize) -> Result<*const u8, HeapError> {
        // First attempt: normal allocation
        if let Ok(space) = self.try_allocate(size_bytes) {
            return Ok(space);
        }

        // Check if we can attempt emergency collection
        let emergency_state = unsafe { &mut *self.emergency_state.get() };
        if emergency_state.can_attempt_emergency_collection() {
            // Attempt emergency collection
            if self.attempt_emergency_collection(size_bytes).is_ok() {
                // Retry allocation after emergency collection
                if let Ok(space) = self.try_allocate(size_bytes) {
                    return Ok(space);
                }
            }
        }

        // All attempts failed
        Err(self.out_of_memory_error(size_bytes, emergency_state.can_attempt_emergency_collection()))
    }

    /// Try to allocate without emergency collection
    fn try_allocate(&self, size_bytes: usize) -> Result<*const u8, HeapError> {
        let heap_state = unsafe { &mut *self.state.get() };
        let head = heap_state.head();

        let space = match SizeClass::for_size(size_bytes) {
            SizeClass::Large => {
                let lob = heap_state.lob(size_bytes);
                lob.space()
            }
            SizeClass::Medium if size_bytes > head.current_hole_size() => heap_state
                .overflow()
                .bump(size_bytes)
                .or_else(|| heap_state.replace_overflow().bump(size_bytes))
                .ok_or_else(|| self.out_of_memory_error(size_bytes, false))?,
            _ => head
                .bump(size_bytes)
                .or_else(|| heap_state.replace_head().bump(size_bytes))
                .ok_or_else(|| self.out_of_memory_error(size_bytes, false))?,
        };

        Ok(space)
    }

    /// Attempt emergency garbage collection
    fn attempt_emergency_collection(&self, requested_size: usize) -> Result<(), HeapError> {
        // Set emergency state to prevent reentrancy
        let emergency_state = unsafe { &mut *self.emergency_state.get() };
        emergency_state.start_emergency_collection();

        eprintln!("Emergency collection: attempting to free space for {} bytes", requested_size);

        // Perform conservative emergency collection
        let success = self.perform_emergency_sweep(requested_size);
        
        // Mark collection as completed
        emergency_state.complete_emergency_collection();
        
        if success {
            eprintln!("Emergency collection: successfully freed space");
            Ok(())
        } else {
            eprintln!("Emergency collection: insufficient space freed");
            Err(self.emergency_collection_insufficient_error(requested_size))
        }
    }

    /// Perform conservative emergency sweep without full marking
    /// This is safe because it only reclaims blocks that can be proven unused
    fn perform_emergency_sweep(&self, _requested_size: usize) -> bool {
        let heap_state = unsafe { &mut *self.state.get() };
        let stats_before = heap_state.stats();
        
        eprintln!("Emergency collection: before sweep - {} blocks allocated, {} recycled", 
                 stats_before.blocks_allocated, stats_before.recycled);

        // Strategy 1: Try to reclaim blocks that are completely unused
        // This is safe because we're not relying on reachability analysis
        heap_state.sweep();
        
        // Strategy 2: If we still need space, try to free the current head block
        // if it's mostly empty (this is more aggressive but still relatively safe)
        if stats_before.recycled == heap_state.stats().recycled {
            self.try_emergency_head_replacement(heap_state);
        }

        let stats_after = heap_state.stats();
        eprintln!("Emergency collection: after sweep - {} blocks allocated, {} recycled", 
                 stats_after.blocks_allocated, stats_after.recycled);

        // Check if we freed enough space
        let blocks_freed = stats_after.recycled - stats_before.recycled;
        let bytes_freed = blocks_freed * BLOCK_SIZE_BYTES;
        
        eprintln!("Emergency collection: freed {} blocks ({} bytes)", blocks_freed, bytes_freed);
        
        // Success if we freed at least one block worth of space
        // (This is conservative - we could be more sophisticated about size requirements)
        blocks_freed > 0
    }

    /// Try to replace the head block if it's mostly empty
    /// This is more aggressive but still relatively safe
    fn try_emergency_head_replacement(&self, heap_state: &mut HeapState) {
        // Only replace head if it exists and has very little allocated space
        if let Some(ref head) = heap_state.head {
            let hole_size = head.current_hole_size();
            let utilisation_percent = ((BLOCK_SIZE_BYTES - hole_size) as f64 / BLOCK_SIZE_BYTES as f64) * 100.0;
            
            eprintln!("Emergency collection: head block utilisation {:.1}% (hole size: {} bytes)", 
                     utilisation_percent, hole_size);
            
            // If head block is less than 10% utilised, replace it
            // This is aggressive but in an emergency situation it's reasonable
            if utilisation_percent < 10.0 {
                eprintln!("Emergency collection: replacing underutilised head block");
                heap_state.replace_head();
            }
        }
    }

    /// Return the allocated size of an object as it's size_of::<T>() value rounded
    /// up to a double-word boundary
    ///
    /// TODO this isn't correctly implemented, as aligning the object to a double-word
    /// boundary while considering header size (which is not known to this libarary
    /// until compile time) means touching numerous bump-allocation code points with
    /// some math and bitwise ops I haven't worked out yet
    pub fn alloc_size_of(object_size: usize) -> usize {
        let align = size_of::<usize>(); // * 2;
        (object_size + (align - 1)) & !(align - 1)
    }

    // Error context creation methods

    /// Create heap context for error reporting
    fn create_heap_context(&self, requested_size: usize, emergency_attempted: bool) -> HeapContext {
        let stats = self.stats();
        let heap_state = unsafe { &*self.state.get() };
        let size_class = SizeClass::for_size(requested_size);

        // Calculate head block utilisation if it exists
        let head_utilisation_percent = heap_state.head.as_ref().map(|head| {
            let hole_size = head.current_hole_size();
            ((BLOCK_SIZE_BYTES - hole_size) as f64 / BLOCK_SIZE_BYTES as f64) * 100.0
        });

        // Calculate overflow block utilisation if it exists
        let overflow_utilisation_percent = heap_state.overflow.as_ref().map(|overflow| {
            let hole_size = overflow.current_hole_size();
            ((BLOCK_SIZE_BYTES - hole_size) as f64 / BLOCK_SIZE_BYTES as f64) * 100.0
        });

        HeapContext {
            blocks_allocated: stats.blocks_allocated,
            blocks_used: stats.used,
            blocks_recycled: stats.recycled,
            lobs_allocated: stats.lobs_allocated,
            requested_size,
            size_class,
            head_utilisation_percent,
            overflow_utilisation_percent,
            emergency_collections_attempted: emergency_attempted,
            heap_limit: self.limit,
        }
    }

    /// Create contextual OutOfMemory error
    fn out_of_memory_error(&self, requested_size: usize, emergency_attempted: bool) -> HeapError {
        HeapError::OutOfMemory {
            context: self.create_heap_context(requested_size, emergency_attempted),
        }
    }

    /// Create contextual EmergencyCollectionInsufficient error
    fn emergency_collection_insufficient_error(&self, requested_size: usize) -> HeapError {
        HeapError::EmergencyCollectionInsufficient {
            context: self.create_heap_context(requested_size, true),
        }
    }

    /// Create contextual FragmentationError
    fn fragmentation_error(&self, requested_size: usize) -> HeapError {
        HeapError::FragmentationError {
            context: self.create_heap_context(requested_size, false),
        }
    }

    // GC functions

    /// Reset all region marks ready for a fresh GC
    pub fn reset_region_marks(&self) {
        let heap_state = unsafe { &mut *self.state.get() };

        if let Some(head) = &mut heap_state.head {
            head.reset_region_marks();
        }

        for block in &mut heap_state.rest {
            block.reset_region_marks();
        }
    }

    /// Check wither an object is marked
    pub fn is_marked<T>(&self, ptr: NonNull<T>) -> bool {
        let header: NonNull<AllocHeader> = self.get_header(ptr);
        debug_assert!(header.as_ptr() as usize > 0);
        let current_mark_state = self.mark_state();
        unsafe { (*header.as_ptr()).is_marked_with_state(current_mark_state) }
    }

    /// Mark an object as live
    pub fn mark_object<T>(&self, ptr: NonNull<T>) {
        debug_assert!(ptr != NonNull::dangling() && ptr.as_ptr() as usize != 0xffffffffffffffff);
        let header: NonNull<AllocHeader> = self.get_header(ptr);
        let current_mark_state = self.mark_state();
        unsafe {
            (*header.as_ptr()).mark_with_state(current_mark_state);
        }
    }

    /// Unmark object
    pub fn unmark_object<T>(&self, ptr: NonNull<T>) {
        debug_assert!(ptr != NonNull::dangling() && ptr.as_ptr() as usize != 0xffffffffffffffff);
        let header = self.get_header(ptr);
        let current_mark_state = self.mark_state();
        unsafe { (*header.as_ptr()).unmark_with_state(current_mark_state) }
    }

    /// Mark lines for and object (array) that uses untyped backing
    /// store of bytes.
    ///
    /// We need to consult the header for size.
    pub fn mark_lines_for_bytes(&mut self, ptr: NonNull<u8>) {
        let header = unsafe { self.get_header(ptr).as_mut() };
        let heap_state = unsafe { &mut *self.state.get() };

        let bytes = header.length() as usize;
        // TODO: fix
        if let Some(head) = &mut heap_state.head {
            head.mark_region(ptr, bytes);
        }

        for block in &mut heap_state.rest {
            block.mark_region(ptr, bytes);
        }
    }

    /// Mark the line in the appropriate block map
    pub fn mark_line<T>(&mut self, ptr: NonNull<T>) {
        // depending on size of object + header, mark line or lines
        let heap_state = unsafe { &mut *self.state.get() };

        let size = size_of::<T>();
        // TODO: fix this - go directly to the right block!
        if SizeClass::for_size(size) == SizeClass::Medium {
            if let Some(head) = &mut heap_state.head {
                head.mark_region(ptr.cast(), size);
            }

            for block in &mut heap_state.rest {
                block.mark_region(ptr.cast(), size);
            }
        } else {
            if let Some(head) = &mut heap_state.head {
                head.mark_line(ptr);
            }

            for block in &mut heap_state.rest {
                block.mark_line(ptr);
            }
        }
    }

    /// Unmark the line in the appropriate block map
    pub fn unmark_line<T>(&mut self, ptr: NonNull<T>) {
        // depending on size of object + header, unmark line or lines
        let _header = self.get_header(ptr);
        todo!();
    }

    pub fn sweep(&mut self) {
        self.state.get_mut().sweep()
    }
}

#[cfg(test)]
pub mod tests {
    use std::iter::repeat_with;

    use crate::{
        common::sourcemap::Smid,
        eval::memory::{
            mutator::MutatorHeapView,
            syntax::{LambdaForm, Ref, StgBuilder},
        },
    };

    use super::*;

    #[test]
    pub fn test_simple_allocations() {
        let heap = Heap::new();

        let ptr = heap.alloc(Ref::num(99)).unwrap();
        unsafe { assert_eq!(*ptr.as_ref(), Ref::num(99)) };

        let header_ptr = heap.get_header(ptr);
        let difference = ptr.as_ptr() as usize - header_ptr.as_ptr() as usize;
        assert_eq!(difference, size_of::<AllocHeader>());

        unsafe {
            assert!(!(*header_ptr.as_ptr()).is_marked());
        }
    }

    #[test]
    pub fn test_several_blocks() {
        let heap = Heap::new();

        for i in 0..32000 {
            let ptr = heap.alloc(Ref::num(i)).unwrap();
            unsafe { assert_eq!(*ptr.as_ref(), Ref::num(i)) };
        }
    }

    #[test]
    pub fn test_large_object_block() {
        let heap = Heap::new();
        let view = MutatorHeapView::new(&heap);

        let ids = repeat_with(|| -> LambdaForm {
            LambdaForm::new(1, view.atom(Ref::L(0)).unwrap().as_ptr(), Smid::default())
        })
        .take(32000)
        .collect::<Vec<_>>();
        let idarray = view.array(ids.as_slice());

        view.let_(
            idarray,
            view.app(Ref::L(0), view.singleton(view.sym_ref("foo").unwrap()))
                .unwrap(),
        )
        .unwrap();

        assert_eq!(heap.stats().lobs_allocated, 1);
    }
}
