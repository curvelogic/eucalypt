# Garbage Collection Implementation

This document describes Eucalypt's Immix-style garbage collector as it
stands in `src/eval/memory/`. Where a section names a function, the code
is the authority.

## Overview

Eucalypt uses a single-generation, stop-the-world, mark-region collector
modelled on Immix. Memory is carved into blocks and lines; marking records
live lines as well as live objects; sweeping is lazy, done block by block
as the allocator needs space; and each collection decides whether to mark
in place or to evacuate (copy out) the live objects of sparse and
fragmented blocks. There is no generational or concurrent collection.

## Architecture

### Memory Layout

The GC uses a block-based allocation strategy with the following hierarchy:

- **Blocks**: 32KB (2^15 bytes) memory regions
- **Lines**: 128-byte (2^7 bytes) units within blocks, each with a bit in
  the block's line map
- **Objects**: Variable-sized objects with 16-byte headers

```
Block (32KB)
├── Line 0 (128B) ┐
├── Line 1 (128B) │ 256 lines per block
├── ...           │
└── Line 255      ┘
```

*Implementation*: `BLOCK_SIZE_BITS`, `LINE_SIZE_BITS` and `LINE_COUNT` in
`src/eval/memory/bump.rs`; `Block` in `src/eval/memory/block.rs`.

### Size Classes

Objects (header included, rounded up to 16 bytes) fall into three size
classes:

1. **Small** (under 128 bytes): fit within a single line
2. **Medium** (128 bytes to under 32KB): span lines within a block
3. **Large** (32KB and over): each gets a dedicated large object block
   (LOB)

*Implementation*: `SizeClass::for_size` in `src/eval/memory/heap.rs`.

### Object Headers

Every allocated object has a 16-byte header (`#[repr(C, align(16))]`, so
16 bytes on wasm32 too) containing:

- **Header bits** (2 bits): the mark bit and the forwarded bit
- **Length** (32 bits): the object's payload bytes, excluding header and
  padding; marking and evacuation use it to find the lines an object spans
- **Forwarding pointer**: set when evacuation copies the object, pointing
  at the new copy

*Implementation*: `AllocHeader` and `HeaderBits` in
`src/eval/memory/header.rs`.

## Allocation Strategy

### Bump Allocation

Each block uses downward bump allocation with:
- **Cursor**: Current allocation position, moving down
- **Lower limit**: Bottom of the current hole
- **Line map**: Bitmap of the lines the last mark found live

When the cursor reaches the lower limit, `bump` asks the line map for the
next hole below it.

*Implementation*: `BumpBlock::bump` and `LineMap::find_hole` in
`src/eval/memory/bump.rs`.

### Block Management

`HeapState` holds every block, in these lists:

- **`head`**: the block small objects (and medium ones that fit its
  current hole) are bumped into
- **`overflow`**: the block for medium objects that do not fit the head's
  hole
- **`rest`**: blocks retired from `head` or `overflow`, and evacuation
  targets, since the last collection
- **`unswept`**: blocks awaiting lazy sweep; every bump block goes here at
  the end of a collection
- **`recycled`**: swept blocks with usable holes — partly used ones at the
  front, fully free ones at the back
- **`lobs`** and **`recycled_lobs`**: large object blocks in use and
  available for reuse
- **`evacuation_target`** and **`filled_evacuation_blocks`**: fresh blocks
  receiving evacuated copies during a collection

*Implementation*: `HeapState` in `src/eval/memory/heap.rs`.

### Allocation Paths

**All allocations** flow through the `Allocator` implementation for `Heap`:

1. `Heap::alloc<T>()` - typed objects
2. `Heap::alloc_bytes()` - zeroed byte regions (array and vector backing
   stores)
3. `Heap::alloc_bytes_uninit()` - byte regions the caller fills before
   reading

Each one rounds header plus payload up to 16 bytes, finds space with
`find_space`, writes the header with the heap's current mark state set as
*unmarked*, and returns a pointer to the payload.

`try_allocate` routes by size class:

- **Large**: a LOB, reusing a recycled one where it wastes at most half
  its space
- **Medium** larger than the head's current hole: the overflow block; when
  full, `replace_overflow` takes a fully free swept block (lazily sweeping
  `unswept` to find one), then a recycled block with a large enough hole,
  then a fresh block
- **Otherwise**: the head block; when full, `replace_head_targeted` lazily
  sweeps if `recycled` is empty, then takes the best-scoring of the first
  16 recycled blocks, or a fresh block

If `try_allocate` fails, `find_space` makes one *emergency collection*
attempt (`attempt_emergency_collection`, at most once per 100 ms): it sweeps
the blocks still in `unswept` and retries. It runs no mark phase. A system
allocation failure for a fresh block aborts the process
(`BumpBlock::new`).

*Implementation*: `impl Allocator for Heap`, `Heap::find_space`,
`Heap::try_allocate` and `Heap::perform_emergency_sweep` in
`src/eval/memory/heap.rs`.

## Collection Algorithm

`collect` in `src/eval/memory/collect.rs` asks
`Heap::analyze_collection_strategy` for a strategy (see *Choosing blocks to
evacuate* below), then runs one of two paths. Both are stop-the-world and
single-threaded, and both trace precisely from the machine's roots: the
bytecode machine state implements `GcScannable`
(`src/eval/bytecode/machine.rs`).

### Mark Phase

Marking is a breadth-first traversal over a work queue:

1. **Reset** the line maps of every bump block (`CollectorHeapView::reset`)
2. **Scan the roots**, queueing each object they reach
3. **Scan queued objects** until the queue is empty; each `scan` marks the
   objects it reports and queues those newly marked

Marking an object sets its header's mark bit and marks every line its
header and payload span, finding the block's line map by its base address
(`HeapState::mark_region_in_block`, through the `line_map_index` for most
blocks). It also adds the object's bytes to the live
volume the next collection trigger is set from.

*Implementation*: `CollectorHeapView::mark`, `mark_raw_bytes` and
`mark_array`, and `collect_mark_in_place`, in `src/eval/memory/collect.rs`.

### Evacuation

When the strategy names candidate blocks, `collect_with_evacuation`
replaces the mark-in-place path:

1. **Mark and evacuate**: as above, but each heap object taken from the
   queue in a candidate block, unless the block is pinned, is copied
   (header and payload) into a fresh evacuation target block; the old
   header gets the forwarded bit and a forwarding pointer, and the new copy
   is marked. Every heap object visited is recorded for the next step.
2. **Update**: the roots, then every recorded object (its new copy where
   it moved), rewrite their pointers to forwarded objects
   (`GcScannable::scan_and_update`).
3. **Release**: `HeapState::release_evacuated_lines` redraws each candidate
   block's line map without the old copies, so their lines are free in this
   collection; `finalise_evacuation` moves the target blocks to `rest`.
4. **Sweep**: as for mark in place.

An object stays where it is if its block is pinned or no target space can
be had. Large objects are never evacuated.

*Implementation*: `collect_with_evacuation`, `try_evacuate` and
`CollectorHeapView::evacuate` in `src/eval/memory/collect.rs`.

### Pinning

Code that holds a raw pointer into the heap across an allocation pins its
block: `MutatorHeapView::pin` returns a `PinGuard` that unpins on drop.
Pins are counted per block, and pinned blocks are never evacuation
candidates. For example, `src/eval/stg/support.rs` pins a string while an
intrinsic reads it.

*Implementation*: `Heap::pin_block`, `Heap::unpin_block` and
`Heap::is_block_pinned` in `src/eval/memory/heap.rs`; `PinGuard` in
`src/eval/memory/mutator.rs`.

### Sweep Phase

Sweeping is lazy. At the end of a collection `defer_sweep` moves every
block in `rest` and `recycled` to `unswept`; large object blocks, which
have no line map, are swept there and then (`sweep_large_objects`). An
unswept block is swept only when the allocator wants a block
(`lazy_sweep_next`, `take_free_block`) or an emergency collection runs.
Sweeping a block (`BumpBlock::recycle`) finds its holes from its line
map: a hole needs two or more consecutive free lines, and the top line of
each hole is left out (Immix's conservative line marking). Blocks with a
hole go to `recycled`; the rest go to `rest`.

*Implementation*: `HeapState::defer_sweep`, `HeapState::lazy_sweep_next`
and `HeapState::sweep_large_objects` in `src/eval/memory/heap.rs`;
`BumpBlock::recycle` and `LineMap::find_hole` in `src/eval/memory/bump.rs`.

### Mark State Management

Marks are never cleared: each `Heap` holds a mark state that flips after
every collection, and an object is marked when its mark bit equals the
current state. New objects are allocated unmarked for the current state.

*Implementation*: `Heap::mark_state` and `Heap::flip_mark_state` in
`src/eval/memory/heap.rs`. (`src/eval/memory/mark.rs` is an empty
placeholder left from when the mark state was a process-wide global.)

## Collection Triggering

### Policy-Based Collection (allocation volume, eu-oaz72.4)

Collection is triggered by **allocation volume since the last collection**,
not by heap size, with or without a heap limit:

1. The heap counts the lines it hands to the allocator: all of a fresh
   block's, only the free lines of a recycled one, and the size of each
   large object.
2. Each collection measures the live volume: the bytes of every object the
   mark phase marks for the first time (an evacuated copy is not counted
   again).
3. The next collection runs once `max(32 MiB, growth × live)` has been
   allocated. The growth factor starts at 4 and doubles, up to 8, after a
   collection that reclaims less than a quarter of what was allocated; it
   resets after one that reclaims more than half.
4. Under `--heap-limit-mib`, the trigger is capped at the headroom left
   below the limit; if more than 7/8 of the limit is live after a
   collection, evaluation stops with a "heap limit exceeded" error instead
   of collecting on every poll.
5. The check runs every 500 execution steps (`gc_poll`).

The previous rule (collect only once allocated blocks reach the limit) never
collected under the 32 GiB default and thrashed under a small limit.

*Implementation*: `Heap::policy_requires_collection`,
`gc_trigger_blocks_for`, `next_gc_growth_factor` and
`Heap::record_collection` in `src/eval/memory/heap.rs`; `gc_poll` in
`src/eval/bytecode/machine.rs`.

Marking finds an object's line map through a base-address index
(`HeapState::line_map_index`) rather than by searching the block lists, and
evacuation resolves its candidate blocks to a set of base addresses once per
collection; both searches were O(blocks) per marked object, which the old
policy hid by rarely collecting.

### Choosing blocks to evacuate

Before each collection `Heap::analyze_collection_strategy` grades every bump
block (`head`, `overflow`, `rest`, `unswept` and `recycled`) by the lines the
last mark left marked: sparse up to 25%, fragmented up to 75%. Most blocks
sit in `unswept` or `recycled` by then, since every block goes to `unswept`
after a collection.

1. Under 10% of blocks sparse or fragmented: mark in place.
2. 30% or more: mark in place unless the footprint passes 3/4 of the heap
   limit (`footprint_under_pressure`). An evacuating collection updates every
   live object however few blocks it moves, so it is worth paying only when
   the blocks it empties can be given back to the system. They are freed in
   the same collection: the mark phase line-marks an object before
   evacuating it, so once the update phase has rewritten every pointer,
   `HeapState::release_evacuated_lines` redraws each candidate block's line
   map from the regions marked in it, leaving out those whose header is
   forwarded (eu-oaz72.108). Objects that stayed (in a pinned block, never
   reached as a heap object, or with no target space) keep their lines.
3. Otherwise evacuate the unpinned sparse and fragmented blocks, sparsest
   first, while their marked lines fit `evacuation_budget_lines` (an eighth
   of the heap's lines).

`EU_GC_STRESS=1` instead evacuates every unpinned block on every collection.

### Verifying forwarding (`EU_GC_VERIFY`, eu-oaz72.136)

Because evacuated lines are freed in the collection that evacuates them, a
pointer the update phase misses — one an object's `scan` reports but its
`scan_and_update` does not rewrite — is a use-after-free as soon as the
line is reused. `EU_GC_VERIFY` catches it at marking. Every scan site marks
a heap object before pushing it, and only pushed objects are evacuated, so
an object this collection forwards is always marked with the current mark
state; any other forwarded header is an old copy reached through a stale
pointer.

| Level | Where | Panics when marking meets |
|---|---|---|
| 1 | Mark phase and post-mark re-traversal of a non-evacuating collection | any forwarded header |
| 1 | Mark phase and post-mark re-traversal of an evacuating collection | a forwarded header not marked by this collection (forwarded by an earlier one) |
| 2 | After the update phase, before `release_evacuated_lines` frees the old copies: a traversal from the roots with a private visited set (heap marks, line maps and live bytes untouched) | any forwarded header |

The panic names the pointer, its type and the forwarding target.

Level 2 is the real gate: its traversal catches the miss in the collection
that makes it, while the old copy is intact. The level-1 checks are a
cheaper backstop with known gaps. They catch a stale pointer in a later
collection only while its old copy's header survives (a freed line is soon
reused), and since the mark state alternates, an old copy forwarded two
collections earlier reads as marked, so an evacuating mark accepts it.

No check sees a pointer `scan` never reports, a pointer the mutator holds
outside the roots, or an update that writes a wrong but unforwarded address.

With `EU_GC_VERIFY` unset (and no `EU_GC_POISON`), the cost is nil beyond a
flag test: each `CollectorHeapView` computes once whether any debug check
is on, `mark` tests that flag where it used to test `EU_GC_POISON`, and the
check bodies are out of line. `mark_raw_bytes` and `mark_array` gain one
test of whether a forwarding check is on; no extra pass runs.

*Implementation*: `ForwardingCheck`, `CollectorHeapView::debug_mark` and
`verify_no_forwarded_references` in `src/eval/memory/collect.rs`.

### Collection Frequency

- **Check interval**: Every 500 VM execution steps (`gc_poll`)
- **Default behaviour**: Collection on allocation volume (see above)
- **Final collection**: At the end of a run, only if the volume trigger has been reached (`policy_requires_collection`)

## Memory Management Integration

### STG Machine Integration

The GC integrates with the bytecode machine through:

- **Root scanning**: the machine state implements `GcScannable`
- **Allocation scoping**: `MutatorHeapView` provides safe allocation
- **Collection timing**: `gc_poll` in the execution loop

### Object Tracing

Heap objects implement the `GcScannable` trait:

- **`scan`** marks the objects an object references and reports those
  newly marked for scanning
- **`scan_and_update`** rewrites those references to forwarded objects
  after an evacuation; a reference `scan` reports but `scan_and_update`
  misses is the use-after-free `EU_GC_VERIFY` exists to catch
- **Collector scope** ensures lifetime safety

*Implementation*: `GcScannable` and `ScanPtr` in
`src/eval/memory/collect.rs`.

## Performance Characteristics

### Allocation Performance

- **Bump allocation**: O(1) for small and medium objects within a hole
- **Block replacement**: bounded scans of the recycled list (the first 16
  blocks for the head, 8 for the overflow block)
- **Size-based routing**: a strategy per size class

### Collection Performance

- **Mark phase**: proportional to the live heap
- **Evacuation**: bounded by `evacuation_budget_lines` (an eighth of the
  heap's lines) of copying, but an evacuating collection also runs an
  update pass over every live object
- **Sweep**: lazy, one block at a time as the allocator needs one; large
  objects are swept at the end of each collection
- **Pause times**: single-threaded stop-the-world collection

### Memory Utilisation

- **Conservative line marking**: a hole loses its top line, and a line
  stays live while any object in it is
- **Block recycling**: holes in partly used blocks are reused before the
  heap grows
- **Fragmentation**: addressed by evacuation when the footprint presses on
  the heap limit (see *Choosing blocks to evacuate*); fully free blocks
  are returned to the system under a limit (`enforce_footprint_limit`)

## Testing and Validation

### Unit Tests

The `tests` modules of `src/eval/memory/collect.rs`,
`src/eval/memory/heap.rs` and `src/eval/memory/bump.rs` cover allocation,
mark-in-place and evacuating collection, pointer update through every
pointer shape, the forwarding checks, pinning, large object churn, the
collection strategy (`test_collection_strategy_*`) and the trigger
policy.

### GC-Verified Harness

CI's `test-gc-verified` job runs the whole harness with
`EU_GC_VERIFY=2`, `EU_GC_STRESS=1` (evacuate every unpinned block on every
collection) and `EU_GC_POISON=1`. To reproduce it locally:

```bash
cargo build --release
EU_GC_VERIFY=2 EU_GC_STRESS=1 EU_GC_POISON=1 \
  timeout 600 ./target/release/eu test --allow-io tests/harness
```

### Benchmark Tests

Allocation-heavy benchmark programs in `tests/harness/bench/` include
`004_generations.eu`, `007_short_lived.eu`, `008_long_lived_graph.eu` and
`009_fragmentation.eu`.

## Limitations

### Threading Model
- **Single-threaded**: No concurrent or parallel collection
- **Stop-the-world**: Full program pause during collection

### Configuration
- **Fixed parameters**: Collection thresholds are not user-configurable
- **Heap limit**: `--heap-limit-mib` bounds live data and the footprint

### Algorithm Differences from Full Immix
- **No generations**: every collection traces the whole live heap
- **Evacuation is rarer**: Immix defragments opportunistically whenever
  the heap is fragmented; here a heap with 30% or more of its blocks sparse or
  fragmented is evacuated only under heap-limit pressure, because an
  evacuating collection pays for an update pass over every live object
- **Separate update pass**: Immix updates references as it traces; here
  the update runs after marking, over the recorded live objects
- **No reserved headroom**: evacuation copies into fresh blocks rather
  than a set of blocks reserved for it
- **Block-level pinning**: pinning holds a whole block in place rather
  than a single object
- **Large objects never move**

## Code Organisation

The GC implementation spans several modules:

```
src/eval/memory/
├── mod.rs              # Module declarations
├── heap.rs             # Heap, block lists, allocation, trigger and strategy
├── collect.rs          # Mark, evacuation and update phases; GcScannable
├── bump.rs             # Bump blocks, line maps and hole finding
├── block.rs            # Raw block allocation
├── lob.rs              # Large object blocks
├── header.rs           # Object header
├── mark.rs             # Empty placeholder (mark state is per heap)
├── mutator.rs          # Mutator heap access and pinning
├── alloc.rs            # Allocation traits
├── array.rs            # Array/vector support
├── gc_debug.rs         # EU_GC_POISON / EU_GC_VERIFY switches
├── gc_verify.rs        # EU_GC_VERIFY=2 structural checks
└── ...                 # Heap object types (strings, dicts, sets, …)
```

## Usage and Behaviour

### Allocation Patterns
- Small objects (under 128 bytes) are allocated within single lines
- Medium objects (128 bytes to under 32KB) span multiple lines within blocks
- Large objects (32KB and over) receive dedicated allocation blocks

### Collection Triggering
- Collection occurs after an allocation volume set from the live volume,
  or under a heap limit once the blocks held pass a footprint threshold
- An allocation failure gets one emergency sweep of the unswept blocks
  before it is reported

### Memory Layout Optimisation
- Allocations are aligned to 16-byte boundaries
- Large object blocks are rounded up to 16KB up to 128KB, to 64KB up to
  1MB, and to 256KB above that (`LargeObjectBlock::efficient_size_for` in
  `src/eval/memory/lob.rs`)
- Head replacement prefers the recycled block whose holes best suit the
  allocation (`BumpBlock::allocation_suitability_score`)

## Command Line Interface

GC-related options:

- `--heap-limit-mib <SIZE>`: Set heap limit in MiB (default 32768; 0 for
  unbounded); live data beyond 7/8 of it is an error
- `-S, --statistics`: Print execution metrics including GC stats

GC-related environment variables (`EU_GC_POISON`, `EU_GC_VERIFY`,
`EU_GC_STRESS`) are described above and in `CLAUDE.md`.
