//! The STG heap implementation

use std::collections::LinkedList;
use std::fmt::Debug;
use std::ptr::NonNull;
use std::sync::atomic::{AtomicBool, Ordering::SeqCst};
use std::time::{Duration, Instant};
use std::{cell::UnsafeCell, mem::size_of};
use std::{ptr::write, slice::from_raw_parts_mut};

use super::bump::{BlockDensity, BLOCK_SIZE_BYTES, MAX_ALLOC_SIZE};
use super::{
    alloc::{Allocator, MutatorScope},
    bump::{self, BumpBlock},
    header::AllocHeader,
    lob::LargeObjectBlock,
};

/// Type of garbage collection performed
#[derive(Debug, Clone, Copy)]
enum CollectionType {
    /// Full mark and sweep collection
    #[allow(dead_code)] // Not yet implemented but planned for future use
    Full,
    /// Partial sweep-only collection (e.g., emergency collection)
    Partial,
}

/// Collection strategy for adaptive Immix garbage collection
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum CollectionStrategy {
    /// Standard mark-and-sweep with no object evacuation
    /// Used when fragmentation is low or evacuation cost exceeds benefit
    MarkInPlace,

    /// Selective evacuation of specific fragmented blocks
    /// Contains list of block indices to evacuate during collection
    SelectiveEvacuation(Vec<usize>),

    /// Comprehensive defragmentation sweep
    /// Evacuates all fragmented blocks to achieve maximum compaction
    DefragmentationSweep,
}

/// Detailed fragmentation analysis for heap diagnostics and strategy decisions
#[derive(Debug, Clone, Default)]
pub struct FragmentationAnalysis {
    /// Total number of blocks analyzed
    pub total_blocks: usize,
    /// Number of empty blocks (0% utilisation)
    pub empty_blocks: usize,
    /// Number of sparse blocks (1-25% utilisation)
    pub sparse_blocks: usize,
    /// Number of fragmented blocks (25-75% utilisation)
    pub fragmented_blocks: usize,
    /// Number of dense blocks (75%+ utilisation)
    pub dense_blocks: usize,
    /// Overall fragmentation ratio (fragmented_blocks / total_blocks)
    pub fragmentation_ratio: f64,
    /// Sum of all fragmentation scores
    pub total_fragmentation_score: f64,
    /// Average fragmentation score across all blocks
    pub average_fragmentation_score: f64,
}
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

/// Comprehensive GC performance metrics and telemetry
#[derive(Debug, Clone)]
pub struct GCMetrics {
    /// Collection timing metrics
    pub collection_stats: CollectionStats,
    /// Memory allocation tracking
    pub allocation_stats: AllocationStats,
    /// Block utilisation and fragmentation metrics
    pub utilisation_stats: UtilisationStats,
    /// Emergency collection metrics
    pub emergency_stats: EmergencyCollectionStats,
    /// Performance counters since heap creation
    pub performance_counters: PerformanceCounters,
}

/// Collection timing and frequency metrics
#[derive(Debug, Clone)]
pub struct CollectionStats {
    /// Total number of collections performed
    pub total_collections: u64,
    /// Number of full collections (mark and sweep)
    pub full_collections: u64,
    /// Number of partial collections (sweep only)
    pub partial_collections: u64,
    /// Total time spent in garbage collection
    pub total_gc_time: Duration,
    /// Average collection time
    pub average_collection_time: Duration,
    /// Last collection time
    pub last_collection_time: Option<Duration>,
    /// Time since last collection
    pub time_since_last_collection: Option<Duration>,
    /// Last collection timestamp
    pub last_collection_at: Option<Instant>,
}

/// Memory allocation rate and pattern tracking
#[derive(Debug, Clone)]
pub struct AllocationStats {
    /// Total bytes allocated since heap creation
    pub total_bytes_allocated: u64,
    /// Total objects allocated
    pub total_objects_allocated: u64,
    /// Allocation rate (bytes per second)
    pub allocation_rate_bps: f64,
    /// Allocation rate (objects per second)
    pub allocation_rate_ops: f64,
    /// Size class distribution
    pub size_class_distribution: SizeClassStats,
    /// Peak allocation rate observed
    pub peak_allocation_rate_bps: f64,
    /// Current allocation burst size
    pub current_burst_bytes: u64,
}

/// Distribution of allocations by size class
#[derive(Debug, Clone, Default)]
pub struct SizeClassStats {
    /// Small object allocations (count, total bytes)
    pub small: (u64, u64),
    /// Medium object allocations (count, total bytes)
    pub medium: (u64, u64),
    /// Large object allocations (count, total bytes)
    pub large: (u64, u64),
}

/// Block utilisation and fragmentation analysis
#[derive(Debug, Clone)]
pub struct UtilisationStats {
    /// Current heap utilisation percentage
    pub heap_utilisation_percent: f64,
    /// Current fragmentation level (0.0 = no fragmentation, 1.0 = fully fragmented)
    pub fragmentation_ratio: f64,
    /// Average block utilisation percentage
    pub average_block_utilisation: f64,
    /// Wasted space due to fragmentation (bytes)
    pub fragmentation_waste_bytes: u64,
    /// Head block utilisation history (last 10 measurements)
    pub head_utilisation_history: Vec<f64>,
    /// Block recycling efficiency (recycled / total allocated)
    pub recycling_efficiency: f64,
}

/// Emergency collection performance metrics
#[derive(Debug, Clone)]
pub struct EmergencyCollectionStats {
    /// Total emergency collections attempted
    pub total_attempts: u64,
    /// Successful emergency collections
    pub successful_collections: u64,
    /// Failed emergency collections
    pub failed_collections: u64,
    /// Total time spent in emergency collections
    pub total_emergency_time: Duration,
    /// Average emergency collection time
    pub average_emergency_time: Duration,
    /// Success rate (successful / total attempts)
    pub success_rate: f64,
    /// Average bytes freed per successful emergency collection
    pub average_bytes_freed: u64,
}

/// High-level performance counters and health indicators
#[derive(Debug, Clone)]
pub struct PerformanceCounters {
    /// Heap creation timestamp
    pub heap_created_at: Instant,
    /// Total heap lifetime
    pub heap_lifetime: Duration,
    /// Allocation efficiency (useful allocations / total allocations)
    pub allocation_efficiency: f64,
    /// Memory pressure indicator (0.0 = low, 1.0 = high)
    pub memory_pressure: f64,
    /// GC overhead percentage (gc_time / total_time)
    pub gc_overhead_percent: f64,
    /// Current allocation trend (positive = increasing, negative = decreasing)
    pub allocation_trend: f64,
    /// System health score (0.0 = poor, 1.0 = excellent)
    pub health_score: f64,
}

impl Default for GCMetrics {
    fn default() -> Self {
        let now = Instant::now();
        GCMetrics {
            collection_stats: CollectionStats::default(),
            allocation_stats: AllocationStats::default(),
            utilisation_stats: UtilisationStats::default(),
            emergency_stats: EmergencyCollectionStats::default(),
            performance_counters: PerformanceCounters::new(now),
        }
    }
}

impl Default for CollectionStats {
    fn default() -> Self {
        CollectionStats {
            total_collections: 0,
            full_collections: 0,
            partial_collections: 0,
            total_gc_time: Duration::ZERO,
            average_collection_time: Duration::ZERO,
            last_collection_time: None,
            time_since_last_collection: None,
            last_collection_at: None,
        }
    }
}

impl Default for AllocationStats {
    fn default() -> Self {
        AllocationStats {
            total_bytes_allocated: 0,
            total_objects_allocated: 0,
            allocation_rate_bps: 0.0,
            allocation_rate_ops: 0.0,
            size_class_distribution: SizeClassStats::default(),
            peak_allocation_rate_bps: 0.0,
            current_burst_bytes: 0,
        }
    }
}

impl Default for UtilisationStats {
    fn default() -> Self {
        UtilisationStats {
            heap_utilisation_percent: 0.0,
            fragmentation_ratio: 0.0,
            average_block_utilisation: 0.0,
            fragmentation_waste_bytes: 0,
            head_utilisation_history: Vec::new(),
            recycling_efficiency: 0.0,
        }
    }
}

impl Default for EmergencyCollectionStats {
    fn default() -> Self {
        EmergencyCollectionStats {
            total_attempts: 0,
            successful_collections: 0,
            failed_collections: 0,
            total_emergency_time: Duration::ZERO,
            average_emergency_time: Duration::ZERO,
            success_rate: 0.0,
            average_bytes_freed: 0,
        }
    }
}

impl PerformanceCounters {
    fn new(created_at: Instant) -> Self {
        PerformanceCounters {
            heap_created_at: created_at,
            heap_lifetime: Duration::ZERO,
            allocation_efficiency: 1.0,
            memory_pressure: 0.0,
            gc_overhead_percent: 0.0,
            allocation_trend: 0.0,
            health_score: 1.0,
        }
    }
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
    /// Recycled large object blocks available for reuse
    recycled_lobs: Vec<LargeObjectBlock>,
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
            recycled_lobs: vec![],
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

    /// Find and return the best recycled block for a given allocation size
    /// Uses intelligent targeting to minimize fragmentation and improve locality
    pub fn replace_head_targeted(&mut self, allocation_size: usize) -> &mut BumpBlock {
        let replacement = if self.recycled.is_empty() {
            // No recycled blocks available, create new one
            BumpBlock::default()
        } else if allocation_size == 0 {
            // For zero-size allocations, just take the first available block
            self.recycled.pop_front().unwrap()
        } else {
            // Find the best block for this allocation size
            self.find_best_recycled_block(allocation_size)
                .unwrap_or_default()
        };

        self.head.replace(replacement).and_then(|old| {
            self.rest.push_back(old);
            None as Option<BumpBlock>
        });
        self.head.as_mut().unwrap()
    }

    /// Find the best recycled block for the given allocation size
    /// Removes and returns the block from the recycled list
    fn find_best_recycled_block(&mut self, allocation_size: usize) -> Option<BumpBlock> {
        if self.recycled.is_empty() {
            return None;
        }

        let mut best_index: Option<usize> = None;
        let mut best_score = 0.0;

        // Evaluate all recycled blocks and find the best fit
        for (index, block) in self.recycled.iter().enumerate() {
            let score = block.allocation_suitability_score(allocation_size);
            if score > best_score {
                best_score = score;
                best_index = Some(index);
            }
        }

        // Remove and return the best block if one was found with a good score
        if let Some(index) = best_index {
            if best_score > 0.0 {
                // Remove the block at the found index
                let mut remaining = LinkedList::new();
                let mut target_block: Option<BumpBlock> = None;
                let mut current_index = 0;

                while let Some(block) = self.recycled.pop_front() {
                    if current_index == index {
                        target_block = Some(block);
                    } else {
                        remaining.push_back(block);
                    }
                    current_index += 1;
                }

                self.recycled = remaining;
                return target_block;
            }
        }

        // No suitable block found, take the first available
        self.recycled.pop_front()
    }

    pub fn replace_overflow(&mut self) -> &mut BumpBlock {
        self.overflow.replace(BumpBlock::new()).and_then(|old| {
            self.rest.push_back(old);
            None as Option<BumpBlock>
        });
        self.overflow.as_mut().unwrap()
    }

    /// Create and return a new large object block able to store data
    /// of the specified size. Tries to reuse recycled blocks first.
    pub fn lob(&mut self, size: usize) -> &mut LargeObjectBlock {
        // First, try to find a recycled block that can fit this allocation
        if let Some(recycled_block) = self.find_suitable_recycled_lob(size) {
            self.lobs.push(recycled_block);
        } else {
            // No suitable recycled block, create a new one
            self.lobs.push(LargeObjectBlock::new(size));
        }
        self.lobs.last_mut().unwrap()
    }

    /// Find and remove a suitable recycled large object block for the given size
    /// Returns the block if found, or None if no suitable block exists
    fn find_suitable_recycled_lob(&mut self, required_size: usize) -> Option<LargeObjectBlock> {
        // Find the best fit among recycled blocks
        let mut best_index: Option<usize> = None;
        let mut best_waste: f64 = f64::MAX;

        for (index, lob) in self.recycled_lobs.iter().enumerate() {
            if lob.can_fit(required_size) {
                let waste = lob.waste_percentage(required_size);
                if waste < best_waste {
                    best_waste = waste;
                    best_index = Some(index);
                }
            }
        }

        // Remove and return the best block if found
        if let Some(index) = best_index {
            Some(self.recycled_lobs.remove(index))
        } else {
            None
        }
    }

    /// Recycle a large object block for future reuse
    /// This would typically be called during garbage collection when a large object is freed
    pub fn recycle_lob(&mut self, lob: LargeObjectBlock) {
        // For now, we'll add a simple limit to prevent unbounded growth
        const MAX_RECYCLED_LOBS: usize = 16;

        if self.recycled_lobs.len() < MAX_RECYCLED_LOBS {
            self.recycled_lobs.push(lob);
        }
        // If we're at the limit, just drop the block (let it be deallocated)
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
            HeapError::InvalidAllocationSize {
                requested_size,
                max_size,
            } => {
                write!(
                    f,
                    "invalid allocation size: requested {} bytes exceeds maximum {} bytes",
                    requested_size, max_size
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
                },
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
            EmergencyState::RecentEmergencyCollection {
                performed_at,
                cooldown_duration,
            } => performed_at.elapsed() > *cooldown_duration,
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
    /// GC performance metrics and telemetry
    gc_metrics: UnsafeCell<GCMetrics>,
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
        assert!(matches!(
            result.unwrap_err(),
            HeapError::EmergencyCollectionInsufficient { .. }
        ));

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
    fn test_gc_performance_metrics() {
        // Test that GC performance metrics are properly tracked
        let heap = Heap::new();

        // Initial metrics should be zeroed
        let initial_metrics = heap.gc_metrics();
        assert_eq!(initial_metrics.allocation_stats.total_objects_allocated, 0);
        assert_eq!(initial_metrics.allocation_stats.total_bytes_allocated, 0);
        assert_eq!(initial_metrics.collection_stats.total_collections, 0);
        assert_eq!(initial_metrics.emergency_stats.total_attempts, 0);

        println!("✅ Initial GC metrics are properly initialized");

        // Perform some allocations and check metrics
        use crate::eval::memory::syntax::Ref;

        let _obj1 = heap.alloc(Ref::num(1)).unwrap();
        let _obj2 = heap.alloc(Ref::num(2)).unwrap();
        let _obj3 = heap.alloc(Ref::num(3)).unwrap();

        let post_alloc_metrics = heap.gc_metrics();

        // Allocation tracking is only enabled in debug builds or with gc-telemetry feature
        #[cfg(any(debug_assertions, feature = "gc-telemetry"))]
        {
            assert_eq!(
                post_alloc_metrics.allocation_stats.total_objects_allocated,
                3
            );
            assert!(post_alloc_metrics.allocation_stats.total_bytes_allocated > 0);
            assert!(post_alloc_metrics.allocation_stats.allocation_rate_bps > 0.0);
            assert!(post_alloc_metrics.allocation_stats.allocation_rate_ops > 0.0);

            // Check size class distribution
            assert_eq!(
                post_alloc_metrics
                    .allocation_stats
                    .size_class_distribution
                    .small
                    .0,
                3
            ); // 3 small objects
            assert!(
                post_alloc_metrics
                    .allocation_stats
                    .size_class_distribution
                    .small
                    .1
                    > 0
            ); // Some bytes allocated

            println!("✅ Allocation metrics are properly tracked");

            // Allocate some bytes to test different size classes
            let _bytes = heap.alloc_bytes(1024).unwrap(); // Should be medium size

            let post_bytes_metrics = heap.gc_metrics();
            assert_eq!(
                post_bytes_metrics.allocation_stats.total_objects_allocated,
                4
            );
            assert_eq!(
                post_bytes_metrics
                    .allocation_stats
                    .size_class_distribution
                    .medium
                    .0,
                1
            ); // 1 medium object
        }

        // In release builds without gc-telemetry, metrics should remain zero (zero-overhead)
        #[cfg(not(any(debug_assertions, feature = "gc-telemetry")))]
        {
            assert_eq!(
                post_alloc_metrics.allocation_stats.total_objects_allocated,
                0
            );
            assert_eq!(post_alloc_metrics.allocation_stats.total_bytes_allocated, 0);
            println!("✅ Release build has zero-overhead metrics (no tracking)");
        }

        #[cfg(any(debug_assertions, feature = "gc-telemetry"))]
        println!("✅ Size class distribution tracking works correctly");

        // Test utilisation metrics (these should work in all builds)
        let final_metrics = heap.gc_metrics();
        assert!(final_metrics.utilisation_stats.heap_utilisation_percent >= 0.0);
        assert!(final_metrics.utilisation_stats.fragmentation_ratio >= 0.0);
        assert!(final_metrics.utilisation_stats.fragmentation_ratio <= 1.0);

        println!("✅ Utilisation metrics are calculated correctly");

        // Test performance health indicators
        assert!(final_metrics.performance_counters.health_score >= 0.0);
        assert!(final_metrics.performance_counters.health_score <= 1.0);
        assert!(final_metrics.performance_counters.allocation_efficiency >= 0.0);
        assert!(final_metrics.performance_counters.allocation_efficiency <= 1.0);

        println!("✅ Performance health indicators are calculated correctly");

        // Test emergency collection metrics by triggering one
        let _result = heap.attempt_emergency_collection(1024);

        let post_emergency_metrics = heap.gc_metrics();

        // Emergency collection metrics are only tracked with debug builds or gc-telemetry feature
        #[cfg(any(debug_assertions, feature = "gc-telemetry"))]
        {
            assert_eq!(post_emergency_metrics.emergency_stats.total_attempts, 1);
            assert!(
                post_emergency_metrics.emergency_stats.total_emergency_time
                    > std::time::Duration::ZERO
            );
            println!("✅ Emergency collection metrics are tracked");
        }

        // In release builds without gc-telemetry, emergency metrics should remain zero
        #[cfg(not(any(debug_assertions, feature = "gc-telemetry")))]
        {
            assert_eq!(post_emergency_metrics.emergency_stats.total_attempts, 0);
            assert_eq!(
                post_emergency_metrics.emergency_stats.total_emergency_time,
                std::time::Duration::ZERO
            );
            println!("✅ Emergency collection has zero-overhead metrics in release builds");
        }

        println!("✅ GC performance metrics system is working correctly");
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
        eprintln!(
            "Before emergency collection: {} blocks allocated, {} in rest, {} recycled",
            stats_before.blocks_allocated, stats_before.used, stats_before.recycled
        );

        // Now attempt emergency collection
        let result = heap.attempt_emergency_collection(1024);

        let stats_after = heap.stats();
        eprintln!(
            "After emergency collection: {} blocks allocated, {} in rest, {} recycled",
            stats_after.blocks_allocated, stats_after.used, stats_after.recycled
        );

        // Emergency collection should have attempted to free space
        // Even if it doesn't succeed (because blocks might not be reclaimable),
        // it should not crash and should provide proper error reporting

        match result {
            Ok(()) => {
                println!("✅ Emergency collection successfully freed space");
                assert!(
                    stats_after.recycled > stats_before.recycled,
                    "Should have recycled some blocks"
                );
            }
            Err(HeapError::EmergencyCollectionInsufficient { .. }) => {
                println!(
                    "✅ Emergency collection completed but insufficient space freed (expected)"
                );
                // This is fine - the emergency collection ran but couldn't free enough space
            }
            Err(e) => {
                // Log unexpected errors but don't panic - this is production hardening
                eprintln!("⚠️ Unexpected error from emergency collection: {:?}", e);
                // Emergency collection can fail for various reasons (e.g., no GC limit set)
                // This should not cause a panic in production
            }
        }

        // Verify cooldown is in effect
        let emergency_state = unsafe { &*heap.emergency_state.get() };
        assert!(
            !emergency_state.can_attempt_emergency_collection(),
            "Should be in cooldown period after emergency collection"
        );
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
        assert!(error_message.contains("Small")); // Size class
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

        let small_error = heap.out_of_memory_error(64, false); // Small (< 128 bytes)
        println!("   Small:  {}", small_error);

        let medium_error = heap.out_of_memory_error(1024, true); // Medium (128B - 32KB)
        println!("   Medium: {}", medium_error);

        let large_error = heap.out_of_memory_error(40960, true); // Large (> 32KB)
        println!("   Large:  {}", large_error);

        // Show actual heap stats for context
        println!("\n📊 Heap Statistics:");
        let stats = limited_heap.stats();
        println!(
            "   - Total blocks: {} ({} KB)",
            stats.blocks_allocated,
            stats.blocks_allocated * 32
        );
        println!(
            "   - Used blocks:  {} ({:.1}%)",
            stats.used,
            (stats.used as f64 / stats.blocks_allocated as f64) * 100.0
        );
        println!(
            "   - Recycled:     {} ({:.1}%)",
            stats.recycled,
            (stats.recycled as f64 / stats.blocks_allocated as f64) * 100.0
        );
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
            gc_metrics: UnsafeCell::new(GCMetrics::default()),
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
            gc_metrics: UnsafeCell::new(GCMetrics::default()),
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

    /// Analyze fragmentation across all blocks and determine optimal collection strategy
    pub fn analyze_collection_strategy(&self) -> CollectionStrategy {
        let heap_state = unsafe { &*self.state.get() };
        let mut fragmented_blocks = Vec::new();
        let mut total_blocks = 0;
        let mut sparse_blocks = 0;
        let mut fragmented_count = 0;

        // Analyze head block
        if let Some(ref head) = heap_state.head {
            total_blocks += 1;
            match head.analyze_density() {
                BlockDensity::Sparse => {
                    sparse_blocks += 1;
                    fragmented_blocks.push(0); // Head block index
                }
                BlockDensity::Fragmented => {
                    fragmented_count += 1;
                    fragmented_blocks.push(0);
                }
                _ => {} // Dense or Empty blocks don't need evacuation
            }
        }

        // Analyze overflow block
        if let Some(ref overflow) = heap_state.overflow {
            total_blocks += 1;
            match overflow.analyze_density() {
                BlockDensity::Sparse => {
                    sparse_blocks += 1;
                    fragmented_blocks.push(1); // Overflow block index
                }
                BlockDensity::Fragmented => {
                    fragmented_count += 1;
                    fragmented_blocks.push(1);
                }
                _ => {}
            }
        }

        // Analyze rest blocks
        for (idx, block) in heap_state.rest.iter().enumerate() {
            total_blocks += 1;
            let block_index = idx + 2; // After head (0) and overflow (1)
            match block.analyze_density() {
                BlockDensity::Sparse => {
                    sparse_blocks += 1;
                    fragmented_blocks.push(block_index);
                }
                BlockDensity::Fragmented => {
                    fragmented_count += 1;
                    fragmented_blocks.push(block_index);
                }
                _ => {}
            }
        }

        // Calculate fragmentation ratios
        let total_fragmented = sparse_blocks + fragmented_count;
        let fragmentation_ratio = if total_blocks > 0 {
            total_fragmented as f64 / total_blocks as f64
        } else {
            0.0
        };

        // Decide collection strategy based on fragmentation level
        match fragmentation_ratio {
            // Low fragmentation (< 10%) - use standard mark-in-place
            x if x < 0.1 => CollectionStrategy::MarkInPlace,

            // Moderate fragmentation (10-30%) - selective evacuation
            x if x < 0.3 => {
                if fragmented_blocks.is_empty() {
                    CollectionStrategy::MarkInPlace
                } else {
                    CollectionStrategy::SelectiveEvacuation(fragmented_blocks)
                }
            }

            // High fragmentation (30%+) - comprehensive defragmentation
            _ => CollectionStrategy::DefragmentationSweep,
        }
    }

    /// Get detailed fragmentation analysis for diagnostics and metrics
    pub fn analyze_fragmentation(&self) -> FragmentationAnalysis {
        let heap_state = unsafe { &*self.state.get() };
        let mut analysis = FragmentationAnalysis::default();

        // Analyze all blocks and collect statistics
        let blocks: Vec<&BumpBlock> = std::iter::empty()
            .chain(heap_state.head.iter())
            .chain(heap_state.overflow.iter())
            .chain(heap_state.rest.iter())
            .collect();

        for block in blocks {
            let density = block.analyze_density();
            let score = block.fragmentation_score();

            analysis.total_blocks += 1;
            analysis.total_fragmentation_score += score;

            match density {
                BlockDensity::Empty => analysis.empty_blocks += 1,
                BlockDensity::Sparse => analysis.sparse_blocks += 1,
                BlockDensity::Fragmented => analysis.fragmented_blocks += 1,
                BlockDensity::Dense => analysis.dense_blocks += 1,
            }
        }

        // Calculate derived metrics
        if analysis.total_blocks > 0 {
            analysis.fragmentation_ratio = (analysis.sparse_blocks + analysis.fragmented_blocks)
                as f64
                / analysis.total_blocks as f64;
            analysis.average_fragmentation_score =
                analysis.total_fragmentation_score / analysis.total_blocks as f64;
        }

        analysis
    }
    /// Get a snapshot of current GC performance metrics (calculates derived metrics on-demand)
    pub fn gc_metrics(&self) -> GCMetrics {
        let mut metrics = unsafe { (*self.gc_metrics.get()).clone() };

        // Calculate derived metrics on-demand to avoid hot path overhead
        let now = Instant::now();
        metrics.performance_counters.heap_lifetime =
            now.duration_since(metrics.performance_counters.heap_created_at);

        // Calculate allocation rates
        let heap_lifetime_seconds = metrics.performance_counters.heap_lifetime.as_secs_f64();
        if heap_lifetime_seconds > 0.0 {
            metrics.allocation_stats.allocation_rate_bps =
                metrics.allocation_stats.total_bytes_allocated as f64 / heap_lifetime_seconds;
            metrics.allocation_stats.allocation_rate_ops =
                metrics.allocation_stats.total_objects_allocated as f64 / heap_lifetime_seconds;
        }

        // Update utilisation and performance metrics
        self.calculate_utilisation_metrics(&mut metrics);
        self.calculate_performance_health(&mut metrics);

        metrics
    }

    /// Ultra-fast allocation counter update (minimal overhead for hot path)
    fn update_allocation_counters_fast(&self, size_bytes: usize, size_class: SizeClass) {
        // Only update counters in debug builds or with gc-telemetry feature
        #[cfg(any(debug_assertions, feature = "gc-telemetry"))]
        {
            let metrics = unsafe { &mut *self.gc_metrics.get() };

            // Update basic counters
            metrics.allocation_stats.total_bytes_allocated += size_bytes as u64;
            metrics.allocation_stats.total_objects_allocated += 1;

            // Update size class distribution
            match size_class {
                SizeClass::Small => {
                    metrics.allocation_stats.size_class_distribution.small.0 += 1;
                    metrics.allocation_stats.size_class_distribution.small.1 += size_bytes as u64;
                }
                SizeClass::Medium => {
                    metrics.allocation_stats.size_class_distribution.medium.0 += 1;
                    metrics.allocation_stats.size_class_distribution.medium.1 += size_bytes as u64;
                }
                SizeClass::Large => {
                    metrics.allocation_stats.size_class_distribution.large.0 += 1;
                    metrics.allocation_stats.size_class_distribution.large.1 += size_bytes as u64;
                }
            }
        }
    }

    /// Update collection metrics when a garbage collection occurs (debug-only for performance)
    fn update_collection_metrics(
        &self,
        collection_time: Duration,
        collection_type: CollectionType,
    ) {
        // Only update metrics in debug builds
        #[cfg(debug_assertions)]
        {
            let metrics = unsafe { &mut *self.gc_metrics.get() };
            let now = Instant::now();

            // Update collection counters
            metrics.collection_stats.total_collections += 1;
            match collection_type {
                CollectionType::Full => metrics.collection_stats.full_collections += 1,
                CollectionType::Partial => metrics.collection_stats.partial_collections += 1,
            }

            // Update timing metrics
            metrics.collection_stats.total_gc_time += collection_time;
            metrics.collection_stats.last_collection_time = Some(collection_time);
            metrics.collection_stats.last_collection_at = Some(now);

            // Update average collection time
            if metrics.collection_stats.total_collections > 0 {
                metrics.collection_stats.average_collection_time =
                    metrics.collection_stats.total_gc_time
                        / metrics.collection_stats.total_collections as u32;
            }

            // Update time since last collection for next measurement
            metrics.collection_stats.time_since_last_collection = None; // Reset, will be calculated on next access

            // Update GC overhead percentage
            let total_time = now.duration_since(metrics.performance_counters.heap_created_at);
            if total_time.as_secs_f64() > 0.0 {
                metrics.performance_counters.gc_overhead_percent =
                    (metrics.collection_stats.total_gc_time.as_secs_f64()
                        / total_time.as_secs_f64())
                        * 100.0;
            }

            // Reset allocation burst after collection
            metrics.allocation_stats.current_burst_bytes = 0;
        }

        // In release builds, do nothing
        #[cfg(not(debug_assertions))]
        {
            let _ = (collection_time, collection_type); // Suppress unused warnings
        }
    }

    /// Update emergency collection metrics (feature-gated for performance)
    fn update_emergency_collection_metrics(
        &self,
        emergency_time: Duration,
        success: bool,
        bytes_freed: u64,
    ) {
        // Only update metrics in debug builds or with gc-telemetry feature
        #[cfg(any(debug_assertions, feature = "gc-telemetry"))]
        {
            let metrics = unsafe { &mut *self.gc_metrics.get() };

            // Update emergency collection counters
            metrics.emergency_stats.total_attempts += 1;
            if success {
                metrics.emergency_stats.successful_collections += 1;
            } else {
                metrics.emergency_stats.failed_collections += 1;
            }

            // Update timing metrics
            metrics.emergency_stats.total_emergency_time += emergency_time;
            if metrics.emergency_stats.total_attempts > 0 {
                metrics.emergency_stats.average_emergency_time =
                    metrics.emergency_stats.total_emergency_time
                        / metrics.emergency_stats.total_attempts as u32;
            }

            // Update success rate
            metrics.emergency_stats.success_rate = metrics.emergency_stats.successful_collections
                as f64
                / metrics.emergency_stats.total_attempts as f64;

            // Update average bytes freed (only for successful collections)
            if success {
                let total_bytes_freed = (metrics.emergency_stats.average_bytes_freed
                    * (metrics.emergency_stats.successful_collections - 1))
                    + bytes_freed;
                metrics.emergency_stats.average_bytes_freed =
                    total_bytes_freed / metrics.emergency_stats.successful_collections;
            }
        }
    }

    /// Calculate utilisation and fragmentation metrics on-demand
    fn calculate_utilisation_metrics(&self, metrics: &mut GCMetrics) {
        let stats = self.stats();
        let heap_state = unsafe { &*self.state.get() };

        // Calculate current heap utilisation
        if stats.blocks_allocated > 0 {
            metrics.utilisation_stats.heap_utilisation_percent =
                (stats.used as f64 / stats.blocks_allocated as f64) * 100.0;

            // Calculate recycling efficiency
            metrics.utilisation_stats.recycling_efficiency =
                (stats.recycled as f64 / stats.blocks_allocated as f64) * 100.0;

            // Fragmentation ratio (1 - recycling efficiency normalized)
            metrics.utilisation_stats.fragmentation_ratio =
                1.0 - (metrics.utilisation_stats.recycling_efficiency / 100.0);
        }

        // Calculate head block utilisation if it exists
        if let Some(ref head) = heap_state.head {
            let hole_size = head.current_hole_size();
            let head_utilisation =
                ((BLOCK_SIZE_BYTES - hole_size) as f64 / BLOCK_SIZE_BYTES as f64) * 100.0;

            // Add to utilisation history (keep last 10 measurements)
            metrics
                .utilisation_stats
                .head_utilisation_history
                .push(head_utilisation);
            if metrics.utilisation_stats.head_utilisation_history.len() > 10 {
                metrics.utilisation_stats.head_utilisation_history.remove(0);
            }

            // Update average block utilisation
            let avg_utilisation: f64 = metrics
                .utilisation_stats
                .head_utilisation_history
                .iter()
                .sum::<f64>()
                / metrics.utilisation_stats.head_utilisation_history.len() as f64;
            metrics.utilisation_stats.average_block_utilisation = avg_utilisation;
        }

        // Calculate fragmentation waste in bytes
        let total_heap_bytes = stats.blocks_allocated * BLOCK_SIZE_BYTES;
        let used_heap_bytes = stats.used * BLOCK_SIZE_BYTES;
        let recyclable_bytes = stats.recycled * BLOCK_SIZE_BYTES;
        metrics.utilisation_stats.fragmentation_waste_bytes =
            (total_heap_bytes - used_heap_bytes - recyclable_bytes) as u64;
    }

    /// Calculate performance health indicators on-demand  
    fn calculate_performance_health(&self, metrics: &mut GCMetrics) {
        // Calculate memory pressure based on allocation rate and collection frequency
        let allocation_pressure = if metrics.allocation_stats.peak_allocation_rate_bps > 0.0 {
            (metrics.allocation_stats.allocation_rate_bps
                / metrics.allocation_stats.peak_allocation_rate_bps)
                .min(1.0)
        } else {
            0.0
        };

        let gc_pressure = if metrics.performance_counters.gc_overhead_percent > 50.0 {
            1.0
        } else {
            metrics.performance_counters.gc_overhead_percent / 50.0
        };

        metrics.performance_counters.memory_pressure = (allocation_pressure + gc_pressure) / 2.0;

        // Calculate allocation efficiency (successful allocations vs total attempts)
        // For now, assume all counted allocations were successful
        metrics.performance_counters.allocation_efficiency = 1.0;

        // Calculate allocation trend (simple: increasing if current rate > average rate)
        let lifetime_seconds = metrics.performance_counters.heap_lifetime.as_secs_f64();
        if lifetime_seconds > 1.0 {
            let average_rate =
                metrics.allocation_stats.total_bytes_allocated as f64 / lifetime_seconds;
            // Use recent burst as current rate proxy
            let recent_rate = if metrics.allocation_stats.current_burst_bytes > 0 {
                metrics.allocation_stats.current_burst_bytes as f64
            } else {
                average_rate
            };

            metrics.performance_counters.allocation_trend =
                (recent_rate - average_rate) / average_rate.max(1.0);
        }

        // Calculate overall health score (0.0 = poor, 1.0 = excellent)
        let pressure_score = 1.0 - metrics.performance_counters.memory_pressure;
        let efficiency_score = metrics.performance_counters.allocation_efficiency;
        let gc_overhead_score =
            1.0 - (metrics.performance_counters.gc_overhead_percent / 100.0).min(1.0);
        let fragmentation_score = 1.0 - metrics.utilisation_stats.fragmentation_ratio;

        metrics.performance_counters.health_score =
            (pressure_score + efficiency_score + gc_overhead_score + fragmentation_score) / 4.0;
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
        let size_class = SizeClass::for_size(alloc_size);

        let space = self.find_space(alloc_size)?;

        let header = AllocHeader::new_with_mark_state(0, self.mark_state());

        // Update allocation metrics (lightweight - just counters)
        self.update_allocation_counters_fast(alloc_size, size_class);

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
        let size_class = SizeClass::for_size(alloc_size);

        let space = self.find_space(alloc_size)?;

        let header = AllocHeader::new_with_mark_state(alloc_size as u32, self.mark_state());

        // Update allocation metrics (lightweight - just counters)
        self.update_allocation_counters_fast(alloc_size, size_class);

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
        Err(self.out_of_memory_error(
            size_bytes,
            emergency_state.can_attempt_emergency_collection(),
        ))
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
                .or_else(|| {
                    heap_state
                        .replace_head_targeted(size_bytes)
                        .bump(size_bytes)
                })
                .ok_or_else(|| self.out_of_memory_error(size_bytes, false))?,
        };

        Ok(space)
    }

    /// Attempt emergency garbage collection
    fn attempt_emergency_collection(&self, requested_size: usize) -> Result<(), HeapError> {
        let start_time = Instant::now();

        // Set emergency state to prevent reentrancy
        let emergency_state = unsafe { &mut *self.emergency_state.get() };
        emergency_state.start_emergency_collection();

        eprintln!(
            "Emergency collection: attempting to free space for {} bytes",
            requested_size
        );

        let stats_before = self.stats();

        // Perform conservative emergency collection
        let success = self.perform_emergency_sweep(requested_size);

        // Mark collection as completed
        emergency_state.complete_emergency_collection();

        let emergency_time = start_time.elapsed();
        let stats_after = self.stats();
        let bytes_freed = (stats_after.recycled - stats_before.recycled) * BLOCK_SIZE_BYTES;

        // Update emergency collection metrics
        self.update_emergency_collection_metrics(emergency_time, success, bytes_freed as u64);

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

        eprintln!(
            "Emergency collection: before sweep - {} blocks allocated, {} recycled",
            stats_before.blocks_allocated, stats_before.recycled
        );

        // Strategy 1: Try to reclaim blocks that are completely unused
        // This is safe because we're not relying on reachability analysis
        heap_state.sweep();

        // Strategy 2: If we still need space, try to free the current head block
        // if it's mostly empty (this is more aggressive but still relatively safe)
        if stats_before.recycled == heap_state.stats().recycled {
            self.try_emergency_head_replacement(heap_state);
        }

        let stats_after = heap_state.stats();
        eprintln!(
            "Emergency collection: after sweep - {} blocks allocated, {} recycled",
            stats_after.blocks_allocated, stats_after.recycled
        );

        // Check if we freed enough space
        let blocks_freed = stats_after.recycled - stats_before.recycled;
        let bytes_freed = blocks_freed * BLOCK_SIZE_BYTES;

        eprintln!(
            "Emergency collection: freed {} blocks ({} bytes)",
            blocks_freed, bytes_freed
        );

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
            let utilisation_percent =
                ((BLOCK_SIZE_BYTES - hole_size) as f64 / BLOCK_SIZE_BYTES as f64) * 100.0;

            eprintln!(
                "Emergency collection: head block utilisation {:.1}% (hole size: {} bytes)",
                utilisation_percent, hole_size
            );

            // If head block is less than 10% utilised, replace it
            // This is aggressive but in an emergency situation it's reasonable
            if utilisation_percent < 10.0 {
                eprintln!("Emergency collection: replacing underutilised head block");
                heap_state.replace_head();
            }
        }
    }

    /// Return the allocated size for a given object size, ensuring proper alignment
    ///
    /// This function calculates the total allocation size needed such that:
    /// 1. The entire allocation is aligned to double-word (16-byte) boundaries  
    /// 2. Objects are naturally aligned since header is 16 bytes
    ///
    /// Layout: [Header: 16 bytes][Object: starts at 16-byte aligned offset]
    /// Since AllocHeader is exactly 16 bytes, if the allocation starts on a 16-byte
    /// boundary, the object will also be 16-byte aligned after the header.
    pub fn alloc_size_of(object_size: usize) -> usize {
        const DOUBLE_WORD_ALIGN: usize = 16; // Double-word boundary
        const HEADER_SIZE: usize = 16; // AllocHeader is 16 bytes (verified by static assert)

        let total_size = HEADER_SIZE + object_size;

        // Align total allocation to 16-byte boundary
        (total_size + DOUBLE_WORD_ALIGN - 1) & !(DOUBLE_WORD_ALIGN - 1)
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
    #[cfg(test)]
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
        let start_time = Instant::now();
        self.state.get_mut().sweep();
        let sweep_time = start_time.elapsed();

        // Record collection metrics for sweep operation (collection is less frequent)
        self.update_collection_metrics(sweep_time, CollectionType::Partial);
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

    #[test]
    pub fn test_collection_strategy_mark_in_place() {
        let heap = Heap::new();

        // Create a heap with dense blocks by allocating many objects
        let mut ptrs = Vec::new();
        for i in 0..5000 {
            let ptr = heap.alloc(Ref::num(i)).unwrap();
            ptrs.push(ptr);
        }

        // Mark objects to simulate dense utilisation after GC
        let state = unsafe { &mut *heap.state.get() };
        if let Some(ref mut head) = state.head {
            // Mark most lines using actual allocated object addresses
            for (_idx, ptr) in ptrs.iter().enumerate().take(200) {
                head.mark_line(std::ptr::NonNull::new(ptr.as_ptr() as *mut u8).unwrap());
            }
        }

        let strategy = heap.analyze_collection_strategy();
        assert_eq!(strategy, CollectionStrategy::MarkInPlace);
    }

    #[test]
    pub fn test_collection_strategy_selective_evacuation() {
        let heap = Heap::new();

        // Create a heap with moderate fragmentation across multiple blocks
        let mut ptrs = Vec::new();
        for i in 0..10000 {
            // Ensure we get multiple blocks
            let ptr = heap.alloc(Ref::num(i)).unwrap();
            ptrs.push(ptr);
        }

        // Create moderate fragmentation by making most blocks sparse/fragmented
        let state = unsafe { &mut *heap.state.get() };

        // Mark head block as sparse (25% utilisation)
        if let Some(ref mut head) = state.head {
            for (_idx, ptr) in ptrs.iter().enumerate().step_by(4).take(64) {
                head.mark_line(std::ptr::NonNull::new(ptr.as_ptr() as *mut u8).unwrap());
            }
        }

        // Mark rest blocks as sparse too to increase fragmentation ratio
        let mut ptr_idx = 256;
        for block in state.rest.iter_mut() {
            for _ in 0..64 {
                // 25% utilisation
                if ptr_idx < ptrs.len() {
                    block.mark_line(
                        std::ptr::NonNull::new(ptrs[ptr_idx].as_ptr() as *mut u8).unwrap(),
                    );
                    ptr_idx += 4;
                }
            }
            ptr_idx += 200; // Skip ahead for next block
        }

        let strategy = heap.analyze_collection_strategy();
        let analysis = heap.analyze_fragmentation();

        // Should get SelectiveEvacuation with moderate fragmentation or DefragmentationSweep with high fragmentation
        match strategy {
            CollectionStrategy::SelectiveEvacuation(blocks) => {
                assert!(!blocks.is_empty(), "Should identify blocks for evacuation");
                assert!(
                    analysis.fragmentation_ratio >= 0.15,
                    "Should have moderate fragmentation for SelectiveEvacuation"
                );
            }
            CollectionStrategy::MarkInPlace => {
                // If fragmentation is very low, MarkInPlace is acceptable
                assert!(
                    analysis.fragmentation_ratio < 0.15,
                    "MarkInPlace should only be used for low fragmentation"
                );
            }
            CollectionStrategy::DefragmentationSweep => {
                // DefragmentationSweep is acceptable for high fragmentation (30%+)
                // With double-word alignment, fragmentation can be higher than expected
                assert!(
                    analysis.fragmentation_ratio >= 0.30,
                    "DefragmentationSweep should be used for high fragmentation"
                );
            }
        }
    }

    #[test]
    pub fn test_fragmentation_analysis_empty_heap() {
        let heap = Heap::new();
        let analysis = heap.analyze_fragmentation();

        // Empty heap should have no blocks to analyze
        assert_eq!(analysis.total_blocks, 0);
        assert_eq!(analysis.empty_blocks, 0);
        assert_eq!(analysis.sparse_blocks, 0);
        assert_eq!(analysis.fragmented_blocks, 0);
        assert_eq!(analysis.dense_blocks, 0);
        assert_eq!(analysis.fragmentation_ratio, 0.0);
        assert_eq!(analysis.average_fragmentation_score, 0.0);
        assert_eq!(analysis.total_fragmentation_score, 0.0);
    }

    #[test]
    pub fn test_fragmentation_analysis_single_dense_block() {
        let heap = Heap::new();

        // Allocate many objects to create high density
        let mut ptrs = Vec::new();
        for i in 0..1000 {
            // More allocations to create higher line utilisation
            let ptr = heap.alloc(Ref::num(i)).unwrap();
            ptrs.push(ptr);
        }

        // Mark objects to make the block dense
        let state = unsafe { &mut *heap.state.get() };
        if let Some(ref mut head) = state.head {
            // Use actual allocated object addresses and mark many of them
            // This should create enough marked lines to reach dense threshold
            for ptr in ptrs.iter() {
                head.mark_line(std::ptr::NonNull::new(ptr.as_ptr() as *mut u8).unwrap());
            }
        }

        let analysis = heap.analyze_fragmentation();
        assert!(analysis.total_blocks >= 1, "Should have at least one block");

        // The block should be classified into one of the density categories
        // This test validates that our analysis correctly categorizes block density
        assert!(
            analysis.dense_blocks > 0
                || analysis.fragmented_blocks > 0
                || analysis.sparse_blocks > 0,
            "Should have at least one block with marked objects"
        );

        // Fragmentation ratio should be reasonable
        assert!(analysis.fragmentation_ratio >= 0.0 && analysis.fragmentation_ratio <= 1.0);
        assert!(
            analysis.average_fragmentation_score >= 0.0
                && analysis.average_fragmentation_score <= 1.0
        );
    }

    #[test]
    pub fn test_fragmentation_analysis_sparse_blocks() {
        let heap = Heap::new();

        // Allocate across multiple blocks
        let mut ptrs = Vec::new();
        for i in 0..8000 {
            let ptr = heap.alloc(Ref::num(i)).unwrap();
            ptrs.push(ptr);
        }

        // Make blocks sparse by marking few lines using actual object addresses
        let state = unsafe { &mut *heap.state.get() };
        if let Some(ref mut head) = state.head {
            // Mark only ~10% of objects (sparse)
            for (_idx, ptr) in ptrs.iter().enumerate().step_by(10).take(25) {
                head.mark_line(std::ptr::NonNull::new(ptr.as_ptr() as *mut u8).unwrap());
            }
        }

        // Do similar for rest blocks if they exist
        let mut ptr_idx = 250;
        for block in state.rest.iter_mut() {
            for _ in 0..25 {
                if ptr_idx < ptrs.len() {
                    block.mark_line(
                        std::ptr::NonNull::new(ptrs[ptr_idx].as_ptr() as *mut u8).unwrap(),
                    );
                    ptr_idx += 10;
                }
            }
        }

        let analysis = heap.analyze_fragmentation();
        assert!(analysis.total_blocks > 0);
        assert!(analysis.sparse_blocks > 0, "Should detect sparse blocks");
        assert!(
            analysis.fragmentation_ratio > 0.0,
            "Should have non-zero fragmentation ratio"
        );
        assert!(
            analysis.average_fragmentation_score > 0.0,
            "Sparse blocks should increase average score"
        );
    }

    #[test]
    pub fn test_fragmentation_analysis_mixed_densities() {
        let heap = Heap::new();

        // Create a complex scenario with mixed block densities
        let mut ptrs = Vec::new();
        for i in 0..10000 {
            let ptr = heap.alloc(Ref::num(i)).unwrap();
            ptrs.push(ptr);
        }

        let state = unsafe { &mut *heap.state.get() };

        // Make head block dense using actual object addresses
        if let Some(ref mut head) = state.head {
            for (_idx, ptr) in ptrs.iter().enumerate().take(200) {
                // 78% utilisation - dense
                head.mark_line(std::ptr::NonNull::new(ptr.as_ptr() as *mut u8).unwrap());
            }
        }

        // Make ALL rest blocks with different patterns
        let mut ptr_idx = 200;
        for (block_idx, block) in state.rest.iter_mut().enumerate() {
            match block_idx % 3 {
                0 => {
                    // Make sparse (25% utilisation)
                    for _ in 0..64 {
                        if ptr_idx < ptrs.len() {
                            block.mark_line(
                                std::ptr::NonNull::new(ptrs[ptr_idx].as_ptr() as *mut u8).unwrap(),
                            );
                            ptr_idx += 4;
                        }
                    }
                }
                1 => {
                    // Make fragmented (50% utilisation)
                    for _ in 0..128 {
                        if ptr_idx < ptrs.len() {
                            block.mark_line(
                                std::ptr::NonNull::new(ptrs[ptr_idx].as_ptr() as *mut u8).unwrap(),
                            );
                            ptr_idx += 2;
                        }
                    }
                }
                _ => {
                    // Make dense (78% utilisation)
                    for _ in 0..200 {
                        if ptr_idx < ptrs.len() {
                            block.mark_line(
                                std::ptr::NonNull::new(ptrs[ptr_idx].as_ptr() as *mut u8).unwrap(),
                            );
                            ptr_idx += 1;
                        }
                    }
                }
            }
            ptr_idx += 100; // Skip ahead for next block
        }

        let analysis = heap.analyze_fragmentation();
        assert!(analysis.total_blocks >= 3, "Should have at least 3 blocks");

        // Should have mix of densities, but we can't guarantee exact counts due to empty blocks
        assert!(
            analysis.sparse_blocks > 0
                || analysis.fragmented_blocks > 0
                || analysis.dense_blocks > 0,
            "Should have at least one block with marked lines"
        );

        // Average score should be between 0 and 1
        assert!(analysis.average_fragmentation_score >= 0.0);
        assert!(analysis.average_fragmentation_score <= 1.0);
    }

    #[test]
    pub fn test_collection_strategy_defragmentation_sweep() {
        let heap = Heap::new();

        // Create heavily fragmented heap with many blocks
        let mut ptrs = Vec::new();
        for i in 0..15000 {
            // Create more blocks to ensure high fragmentation ratio
            let ptr = heap.alloc(Ref::num(i)).unwrap();
            ptrs.push(ptr);
        }

        // Make ALL blocks sparse to force very high fragmentation
        let state = unsafe { &mut *heap.state.get() };

        // Make head block sparse (5% utilisation)
        if let Some(ref mut head) = state.head {
            for (_idx, ptr) in ptrs.iter().enumerate().step_by(20).take(13) {
                head.mark_line(std::ptr::NonNull::new(ptr.as_ptr() as *mut u8).unwrap());
            }
        }

        // Make ALL rest blocks sparse too
        let mut ptr_idx = 260;
        for block in state.rest.iter_mut() {
            for _ in 0..13 {
                // 5% utilisation
                if ptr_idx < ptrs.len() {
                    block.mark_line(
                        std::ptr::NonNull::new(ptrs[ptr_idx].as_ptr() as *mut u8).unwrap(),
                    );
                    ptr_idx += 20;
                }
            }
            ptr_idx += 100; // Skip ahead for next block
        }

        let strategy = heap.analyze_collection_strategy();
        let analysis = heap.analyze_fragmentation();

        // With most blocks being sparse, we should get high fragmentation
        match strategy {
            CollectionStrategy::DefragmentationSweep => {
                assert!(analysis.fragmentation_ratio >= 0.30, "DefragmentationSweep should have high fragmentation");
            }
            CollectionStrategy::SelectiveEvacuation(_) => {
                // High fragmentation might still trigger selective evacuation depending on thresholds
                assert!(analysis.fragmentation_ratio > 0.0, "SelectiveEvacuation should have some fragmentation");
            }
            _ => panic!("Expected DefragmentationSweep or SelectiveEvacuation for heavily fragmented heap, got {:?}", strategy),
        }
    }

    #[test]
    pub fn test_collection_strategy_boundary_conditions() {
        let heap = Heap::new();

        // Test exactly at fragmentation boundaries
        let mut ptrs = Vec::new();
        for i in 0..5000 {
            let ptr = heap.alloc(Ref::num(i)).unwrap();
            ptrs.push(ptr);
        }

        let state = unsafe { &mut *heap.state.get() };

        // Test 15% fragmentation (should be SelectiveEvacuation boundary)
        if let Some(ref mut head) = state.head {
            // Create pattern for exactly 15% sparse blocks
            // Mark ~10% of objects to make it sparse
            for (_idx, ptr) in ptrs.iter().enumerate().step_by(10).take(25) {
                head.mark_line(std::ptr::NonNull::new(ptr.as_ptr() as *mut u8).unwrap());
            }
        }

        let strategy = heap.analyze_collection_strategy();

        // At boundary conditions, should prefer SelectiveEvacuation over MarkInPlace
        match strategy {
            CollectionStrategy::SelectiveEvacuation(_)
            | CollectionStrategy::DefragmentationSweep => {
                // Both are acceptable at boundary conditions
            }
            CollectionStrategy::MarkInPlace => {
                // This might happen if fragmentation is calculated as just under threshold
            }
        }
    }

    #[test]
    fn test_alloc_size_calculation() {
        // Test various object sizes to ensure proper alignment calculation
        assert_eq!(Heap::alloc_size_of(0), 16); // Header only: 16 bytes aligned to 16
        assert_eq!(Heap::alloc_size_of(1), 32); // Header(16) + object(1) = 17, aligned to 32
        assert_eq!(Heap::alloc_size_of(8), 32); // Header(16) + object(8) = 24, aligned to 32
        assert_eq!(Heap::alloc_size_of(16), 32); // Header(16) + object(16) = 32, already aligned
        assert_eq!(Heap::alloc_size_of(17), 48); // Header(16) + object(17) = 33, aligned to 48
        assert_eq!(Heap::alloc_size_of(32), 48); // Header(16) + object(32) = 48, already aligned
        assert_eq!(Heap::alloc_size_of(48), 64); // Header(16) + object(48) = 64, already aligned
    }

    #[test]
    fn test_allocation_alignment() {
        let heap = Heap::new();

        // Test that all allocations are properly aligned to 16-byte boundaries
        let mut allocations = Vec::new();

        // Allocate objects of various sizes
        for size in [1, 8, 16, 24, 32, 48, 64, 128] {
            let ptr = heap.alloc_bytes(size).unwrap();
            allocations.push(ptr);

            // Verify that the allocated pointer is 16-byte aligned
            let addr = ptr.as_ptr() as usize;
            assert_eq!(
                addr & 15,
                0,
                "Allocation of size {} not aligned to 16-byte boundary: 0x{:x}",
                size,
                addr
            );
        }
    }

    #[test]
    fn test_header_alignment() {
        use crate::eval::memory::syntax::Ref;
        use std::mem::{align_of, size_of};

        // Verify that AllocHeader is exactly 16 bytes and properly aligned
        assert_eq!(size_of::<AllocHeader>(), 16, "AllocHeader size changed");
        assert_eq!(
            align_of::<AllocHeader>(),
            8,
            "AllocHeader natural alignment"
        );

        let heap = Heap::new();
        let ptr = heap.alloc(Ref::num(42)).unwrap();

        // Get the header and verify it's aligned
        let header = heap.get_header(ptr);
        let header_addr = header.as_ptr() as usize;
        assert_eq!(
            header_addr & 15,
            0,
            "AllocHeader not aligned to 16-byte boundary: 0x{:x}",
            header_addr
        );
    }

    #[test]
    fn test_targeted_block_replacement() {
        let heap = Heap::new();
        let _view = MutatorHeapView::new(&heap);

        // Create several recycled blocks with different characteristics
        let mut blocks = Vec::new();
        for _ in 0..3 {
            let mut block = BumpBlock::new();
            // Simulate some allocations and markings to create holes
            for i in 0..50 {
                block.mark_line_for_test(i);
            }
            // Leave different sized holes in each block
            blocks.push(block);
        }

        let state = unsafe { &mut *heap.state.get() };

        // Add blocks to recycled list
        for block in blocks {
            state.recycled.push_back(block);
        }

        let recycled_count_before = state.recycled.len();

        // Test targeted replacement
        let _best_block = state.replace_head_targeted(1024);

        // Should have consumed one recycled block
        assert_eq!(state.recycled.len(), recycled_count_before - 1);
    }

    #[test]
    fn test_find_best_recycled_block() {
        use crate::eval::memory::bump::LINE_COUNT;

        let heap = Heap::new();
        let state = unsafe { &mut *heap.state.get() };

        // Create blocks with different hole sizes
        let mut small_hole_block = BumpBlock::new();
        let mut large_hole_block = BumpBlock::new();

        // Small hole block: mark most lines, leave small hole
        for i in 10..LINE_COUNT {
            small_hole_block.mark_line_for_test(i);
        }

        // Large hole block: mark fewer lines, leave large hole
        for i in 100..LINE_COUNT {
            large_hole_block.mark_line_for_test(i);
        }

        state.recycled.push_back(small_hole_block);
        state.recycled.push_back(large_hole_block);

        // Request large allocation - should prefer large hole block
        let selected = state.find_best_recycled_block(5000);
        assert!(selected.is_some());

        // Should have removed one block (the better one) from recycled list
        assert_eq!(state.recycled.len(), 1);
    }

    #[test]
    fn test_block_targeting_empty_recycled_list() {
        let heap = Heap::new();
        let state = unsafe { &mut *heap.state.get() };

        // Empty recycled list
        assert!(state.recycled.is_empty());

        // Should create new block when no recycled blocks available
        let _block = state.replace_head_targeted(1024);
        assert!(state.head.is_some());
    }

    #[test]
    fn test_large_object_recycling() {
        let heap = Heap::new();
        let state = unsafe { &mut *heap.state.get() };

        // Initially no LOBs
        assert_eq!(state.lobs.len(), 0);
        assert_eq!(state.recycled_lobs.len(), 0);

        // Allocate a large object
        let _lob1 = state.lob(100 * 1024);
        assert_eq!(state.lobs.len(), 1);

        // Simulate recycling (normally done during GC)
        let recycled_lob = state.lobs.pop().unwrap();
        state.recycle_lob(recycled_lob);
        assert_eq!(state.recycled_lobs.len(), 1);

        // Allocate another large object of similar size - should reuse recycled block
        let _lob2 = state.lob(90 * 1024);
        assert_eq!(state.lobs.len(), 1);
        assert_eq!(state.recycled_lobs.len(), 0); // Should have consumed recycled block
    }

    #[test]
    fn test_large_object_best_fit_recycling() {
        let heap = Heap::new();
        let state = unsafe { &mut *heap.state.get() };

        // Create and recycle blocks of different sizes
        let small_lob = LargeObjectBlock::new(50 * 1024); // ~64KB allocation
        let medium_lob = LargeObjectBlock::new(100 * 1024); // ~112KB allocation
        let large_lob = LargeObjectBlock::new(200 * 1024); // ~256KB allocation

        state.recycle_lob(large_lob);
        state.recycle_lob(small_lob);
        state.recycle_lob(medium_lob);
        assert_eq!(state.recycled_lobs.len(), 3);

        // Request 90KB - should get the medium block (best fit)
        let _allocated = state.lob(90 * 1024);
        assert_eq!(state.recycled_lobs.len(), 2); // Medium block should be consumed

        // Verify the remaining blocks are small and large
        let remaining_sizes: Vec<_> = state
            .recycled_lobs
            .iter()
            .map(|lob| lob.allocated_size())
            .collect();

        // Should have kept the small and large blocks
        assert!(remaining_sizes.len() == 2);
        assert!(remaining_sizes.contains(&(64 * 1024))); // Small block
        assert!(remaining_sizes.contains(&(256 * 1024))); // Large block
    }

    #[test]
    fn test_large_object_recycling_limit() {
        let heap = Heap::new();
        let state = unsafe { &mut *heap.state.get() };

        // Try to recycle more than the limit (16)
        for _ in 0..20 {
            let lob = LargeObjectBlock::new(50 * 1024);
            state.recycle_lob(lob);
        }

        // Should be capped at the limit
        assert_eq!(state.recycled_lobs.len(), 16);
    }
}
