# Garbage Collection Implementation

This document describes the current state of Eucalypt's Immix-style garbage collector implementation.

## Overview

Eucalypt uses a generational, mark-and-sweep garbage collector based on the Immix algorithm. The implementation provides automatic memory management for the STG (Spineless Tagless G-machine) runtime with support for multiple object size classes and block recycling.

## Architecture

### Memory Layout

The GC uses a block-based allocation strategy with the following hierarchy:

- **Blocks**: 32KB (2^15 bytes) memory regions
- **Lines**: 128-byte (2^7 bytes) allocation units within blocks  
- **Objects**: Variable-sized objects with 16-byte headers

```
Block (32KB)
├── Line 0 (128B) ┐
├── Line 1 (128B) │ 256 lines per block
├── ...           │
└── Line 255      ┘
```

### Size Classes

Objects are categorized into three size classes:

1. **Small** (< 128 bytes): Fit within a single line
2. **Medium** (128B - 32KB): Span multiple lines within a block
3. **Large** (> 32KB): Allocated in dedicated Large Object Blocks (LOBs)

*Implementation*: `src/eval/memory/heap.rs:29-51`

### Object Headers

Every allocated object has a 16-byte header containing:

- **Mark bits** (2 bits): Current mark state and forwarding flag
- **Size field** (32 bits): For untyped byte allocations
- **Forwarding pointer** (64 bits): For potential moving collection

*Implementation*: `src/eval/memory/header.rs:61-69`

## Allocation Strategy

### Bump Allocation

Each block uses downward bump allocation with:
- **Cursor**: Current allocation position
- **Lower limit**: Boundary of current usable region
- **Line map**: Bitmap tracking which lines contain live objects

*Implementation*: `src/eval/memory/bump.rs:181-191`

### Block Management

The heap maintains several block categories:

- **Head block**: Active small object allocation
- **Overflow block**: Active medium object allocation  
- **Rest blocks**: Previously used, pending collection
- **Recycled blocks**: Reclaimed with usable holes
- **LOBs**: Large object storage

*Implementation*: `src/eval/memory/heap.rs:54-65`

### Allocation Paths

**All allocations** flow through the GC system via:

1. `Heap::alloc<T>()` - Typed objects (`src/eval/memory/heap.rs:240`)
2. `Heap::alloc_bytes()` - Arrays/vectors (`src/eval/memory/heap.rs:261`)

Both paths:
- Calculate total size (header + object)
- Find appropriate space based on size class
- Write header and object data
- Return typed pointer

## Collection Algorithm

### Mark Phase

Uses tricolor marking with breadth-first traversal:

1. **Reset** all line maps in blocks
2. **Root scanning** from machine state (stack, globals, locals)
3. **Transitive closure** following object references
4. **Line marking** for objects and backing storage

*Implementation*: `src/eval/memory/collect.rs:123-144`

### Sweep Phase

Conservative Immix sweep with hole identification:

1. **Scan line maps** in each block
2. **Identify holes** requiring 2+ consecutive free lines
3. **Apply conservative marking** (exclude boundary lines)
4. **Move recyclable blocks** to recycled list

*Implementation*: `src/eval/memory/bump.rs:295-306`

### Mark State Management

Uses global mark bit flipping to avoid clearing marks:
- **MARK_STATE**: Global atomic boolean indicating current "marked" value
- **flip_mark_state()**: Called after each collection
- **Header marking**: Compares object mark bit with current MARK_STATE

*Implementation*: `src/eval/memory/mark.rs`

## Collection Triggering

### Policy-Based Collection

Collection occurs only when:
1. User sets `--heap-limit-mib` command line option
2. Allocated blocks ≥ limit AND recycled blocks < 25% of total
3. Check performed every 500 execution steps

*Implementation*: `src/eval/machine/vm.rs:810-818`

### No Emergency Collection

**Critical limitation**: Allocation failures cause panics rather than triggering emergency collection:

```rust
.expect("aargh")  // heap.rs:313,317
```

### Collection Frequency

- **Check interval**: Every 500 VM execution steps
- **Default behavior**: No automatic collection (no heap limit set)
- **Final collection**: Always performed at program termination

## Memory Management Integration

### STG Machine Integration

The GC integrates with the STG machine through:

- **Root scanning**: Machine state implements `GcScannable`
- **Allocation scoping**: `MutatorHeapView` provides safe allocation
- **Collection timing**: Integrated with VM execution loop

### Object Tracing

Objects implement `GcScannable` trait:
- **scan()** method returns referenced objects
- **Collector scope** ensures lifetime safety
- **Polymorphic scanning** handles different object types

*Implementation*: `src/eval/memory/collect.rs:47-53`

## Performance Characteristics

### Allocation Performance

- **Bump allocation**: O(1) for small/medium objects
- **Block recycling**: Efficient hole-finding algorithm
- **Size-based routing**: Optimal strategy per size class

### Collection Performance

- **Mark phase**: Proportional to live object count
- **Sweep phase**: Proportional to total block count
- **Pause times**: Single-threaded stop-the-world collection

### Memory Utilization

- **Conservative marking**: May retain some garbage near live objects
- **Block recycling**: Reduces allocation overhead
- **Fragmentation**: Managed through hole coalescing

## Testing and Validation

### Unit Tests

Current test coverage includes:

- **Basic allocation**: Simple object allocation and retrieval
- **Multi-block allocation**: Stress testing block management
- **Collection cycles**: Mark-sweep correctness validation
- **Large objects**: LOB allocation and collection

*Implementation*: `src/eval/memory/collect.rs:164-259`

### Benchmark Tests

Available benchmark programs:

- `004_generations.eu`: Multi-generational allocation patterns
- Other benchmarks in `harness/test/bench/`

### Missing Test Coverage

**Critical gaps identified**:

1. **Long-running tests**: No sustained allocation testing
2. **Emergency scenarios**: No OOM recovery testing  
3. **Performance regression**: No GC timing benchmarks
4. **Leak detection**: No long-term memory usage validation

## Current Limitations

### Error Handling

1. **Panic on OOM**: No graceful memory exhaustion handling
2. **No fallback strategies**: No emergency collection triggers
3. **Limited diagnostics**: Minimal runtime error reporting

### Configuration

1. **Manual heap limits**: Users must explicitly enable GC
2. **Fixed parameters**: No tunable collection thresholds
3. **No runtime metrics**: Limited observability

### Performance

1. **Conservative collection frequency**: May miss allocation bursts
2. **Single-threaded**: No concurrent or parallel collection
3. **Stop-the-world**: Full program pause during collection

## Implementation Status

### Completed Features ✅

- ✅ Block-based allocation with line maps
- ✅ Three-tier size class handling
- ✅ Mark-and-sweep collection algorithm
- ✅ Block recycling and hole management
- ✅ Integration with STG machine
- ✅ Object header management
- ✅ Large object support

### Production Readiness Concerns ⚠️

- ⚠️ Panic-on-OOM instead of graceful handling
- ⚠️ No default garbage collection (manual heap limits required)
- ⚠️ Limited long-running stability testing
- ⚠️ No runtime performance monitoring

### Future Enhancements 🔄

- 🔄 Emergency collection on allocation failure
- 🔄 Automatic heap limit detection
- 🔄 Comprehensive stress testing
- 🔄 GC performance metrics and telemetry
- 🔄 Concurrent/parallel collection options

## Code Organization

The GC implementation spans several modules:

```
src/eval/memory/
├── mod.rs              # Module declarations
├── heap.rs             # Main heap and allocation logic
├── collect.rs          # Mark-and-sweep collector
├── bump.rs             # Block allocation and line maps
├── header.rs           # Object header management
├── mark.rs             # Mark state management
├── mutator.rs          # Mutator heap access
├── alloc.rs            # Allocation traits
├── array.rs            # Array/vector support
├── block.rs            # Low-level block management
├── lob.rs              # Large object blocks
└── ...
```

## Command Line Interface

GC-related options:

- `--heap-limit-mib <SIZE>`: Set heap limit in MiB (enables automatic GC)
- `--heap-dump-at-gc`: Dump heap state during collection (debugging)
- `-S, --statistics`: Print execution metrics including GC stats

## Analysis Against Immix Standards

### Research Summary

Based on analysis of the original Immix paper (Blackburn & McKinley, 2008) and comparison with open-source implementations, several important disparities have been identified in Eucalypt's implementation.

### Specification Compliance

**✅ Correct Implementations:**
- **Block size**: 32KB (matches original paper specification)
- **Line size**: 128 bytes (consistent across all Immix implementations)
- **Conservative line marking**: Properly requires 2+ consecutive free lines for holes
- **Downward bump allocation**: Correct allocation direction within blocks
- **Line map organization**: Proper bitmap implementation for line marking
- **Memory layout**: Correct block/line hierarchy

**⚠️ Significant Disparities:**

#### 1. **Missing Opportunistic Defragmentation** (Critical)
- **Standard Immix**: Core innovation is opportunistic evacuation of fragmented blocks
- **Eucalypt**: No evacuation or moving collection implemented
- **Impact**: Loses primary performance advantage of Immix over mark-sweep
- **Evidence**: Headers have forwarding fields but they're never used

#### 2. **Eager vs Lazy Sweeping** (Performance Impact)
- **Standard Immix**: Lazy sweeping - blocks swept just before allocation
- **Eucalypt**: Eager sweeping - all blocks swept immediately after marking
- **Impact**: Higher collection pause times, less responsive allocation

#### 3. **Conservative Marking Interpretation** (Semantic Difference)
- **Standard Immix**: Conservative refers to stack/root scanning without precise type info
- **Eucalypt**: Conservative refers to line boundary exclusion during hole detection
- **Impact**: Different meaning of "conservative" - both valid but semantically different

#### 4. **Fragmentation Detection** (Missing Core Feature)
- **Standard Immix**: Decides whether to evacuate based on fragmentation analysis
- **Eucalypt**: No fragmentation detection or evacuation decisions
- **Impact**: Cannot adapt collection strategy to heap state

### Algorithm Classification

**Current Status**: Eucalypt now implements true Immix garbage collection with adaptive evacuation strategies (as of Phase 4 implementation).

**Previously Missing Core Immix Features (Now Implemented in Phase 4):**
- ✅ Opportunistic defragmentation (adaptive collection strategy selection)
- ✅ Block evacuation (object copying with forwarding pointers)
- ✅ Fragmentation-based collection decisions (automatic strategy analysis)
- ✅ Moving/copying collection phases (integrated evacuation)
- ⚠️ Lazy sweeping optimisation (future enhancement)

**Remaining Enhancement Opportunities:**
- Reference updating after evacuation (for full object graph relocation)
- Concurrent/parallel collection phases
- More sophisticated block allocation strategies

### Performance Implications

**Retained Benefits:**
- Efficient allocation through bump allocation
- Good cache locality from line-based organization
- Effective hole identification and reuse

**Lost Benefits:**
- Defragmentation to combat fragmentation
- Adaptive collection based on heap state
- Lower pause times from lazy sweeping
- Space efficiency improvements from compaction

### Technical Quality Assessment

**Code Quality**: ⭐⭐⭐⭐⭐ Excellent
- Clean, well-structured implementation
- Proper safety invariants
- Good abstraction boundaries
- Comprehensive unit tests

**Algorithm Completeness**: ⭐⭐⭐ Partial  
- Implements ~60% of full Immix algorithm
- Missing core performance innovations
- Solid foundation for full implementation

**Production Readiness**: ⭐⭐ Needs Work
- Error handling limitations
- Missing adaptive collection
- Incomplete algorithm implementation

### Implementation Recommendations

#### Immediate Improvements (Current Algorithm)
1. **Lazy sweeping**: Sweep blocks just before allocation
2. **Better collection policy**: More sophisticated triggering
3. **Error handling**: Graceful OOM recovery

#### Full Immix Implementation (Major Enhancement)
1. **Fragmentation detection**: Track fragmented vs dense blocks
2. **Evacuation phase**: Implement object moving during collection
3. **Adaptive collection**: Choose mark-in-place vs evacuation per cycle
4. **Conservative root scanning**: Stack scanning without precise types

### Conclusion

Eucalypt's GC implementation demonstrates excellent engineering and captures many of Immix's organizational benefits. However, it's functionally a hybrid "mark-sweep with Immix memory layout" rather than full Immix.

The missing opportunistic defragmentation represents the core innovation that distinguishes Immix from traditional collectors. While the current implementation is functional and well-engineered, adding evacuation would unlock significant performance improvements, particularly for long-running applications with fragmentation concerns.

The implementation provides an excellent foundation for either completing the full Immix algorithm or remaining as a high-quality mark-sweep variant with Immix-inspired memory organization.