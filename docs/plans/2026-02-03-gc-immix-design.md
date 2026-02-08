# GC Immix Completion Design

Date: 2026-02-03

## Overview and Goals

Complete the Immix garbage collector implementation with reliable validation
at every step. Three phases of work, strictly ordered:

1. **Benchmarking infrastructure** -- Statistics enhancements,
   `--statistics-file`, Criterion GC benchmarks, end-to-end bench script,
   agent workflow documentation
2. **Lazy sweeping** -- Defer sweep work to allocation time, reducing
   stop-the-world pause duration
3. **Opportunistic evacuation** -- Object moving/compaction for fragmented
   blocks, completing the Immix algorithm

Each phase produces measurable before/after evidence using the infrastructure
from phase 1. No phase begins until the previous one is validated.

### Success criteria

- An AI agent can follow the documented workflow to make a GC change and
  produce reliable before/after comparison
- Lazy sweep measurably reduces total GC pause time in the bench suite
- Evacuation measurably reduces fragmentation in the stress programs
- No correctness regressions (all existing tests pass throughout)

---

## Phase 1: Statistics and Benchmarking Infrastructure

### 1a: Statistics struct enhancements

**File**: `src/driver/statistics.rs`

Add the following fields to `Statistics`:

- `collections_count: u64` -- how many GC cycles ran
- `peak_heap_blocks: usize` -- high-water mark of allocated blocks
- `total_mark_time: Duration` -- aggregate mark phase time
- `total_sweep_time: Duration` -- aggregate sweep phase time

These get populated from the existing `Clock` (which already tracks
`CollectorMark` and `CollectorSweep` durations) and from a new counter in
`collect()`. The collection count needs threading through -- most likely a
counter on `Heap` that `collect()` increments, then read out at the end
alongside `HeapStats`.

Also fix the duplicate `Machine Max Stack` line in the `Display` impl.

### 1b: `--statistics-file <path>` CLI flag

**Files**: `src/driver/options.rs`, `src/driver/eval.rs`

New option in `RunArgs` / `EucalyptOptions`. When set, writes a JSON file at
program exit containing all `Statistics` fields plus the timing breakdown.

Structure:

```json
{
  "machine_ticks": 42,
  "machine_allocs": 5,
  "machine_max_stack": 2,
  "collections_count": 1,
  "blocks_allocated": 5,
  "peak_heap_blocks": 5,
  "blocks_used": 2,
  "blocks_recycled": 1,
  "timings": {
    "parse": 0.021459,
    "stg-execute": 0.000831,
    "VM-CollectorMark": 0.000213,
    "VM-CollectorSweep": 0.000002,
    "VM-Total": 0.000831
  }
}
```

Uses `serde_json` (already a dependency) for serialisation. The existing `-S`
text output remains unchanged.

### 1c: GC stress programs

Three new `.eu` files in `harness/test/bench/`:

- **`006_short_lived.eu`** -- Repeatedly builds and discards large lists
  using `foldl` over `repeat(...) take(N)` with suppressed intermediate
  results. Forces frequent collection of short-lived objects.

- **`007_long_lived_graph.eu`** -- Builds a large persistent block
  structure, then allocates throwaway lists around it. Tests mark phase
  performance with a large live set.

- **`008_fragmentation.eu`** -- Interleaves retained and discarded
  allocations to create holes in blocks. This is the scenario where
  evacuation should show improvement -- before evacuation it fragments,
  after evacuation it compacts.

These use existing eucalypt language features only (lists, blocks, folds,
`suppress` metadata). Each should run in under a second but allocate enough
to trigger multiple collections with a reasonable `--heap-limit-mib` (e.g.
2-4 MiB).

### 1d: Criterion GC benchmarks

**File**: `benches/gc.rs` (new)

Benchmarks isolating GC operations, following the existing pattern in
`benches/alloc.rs`:

- **`alloc_then_collect`** -- Allocate N objects, trigger collection,
  measure mark + sweep separately via `Clock`

- **`collect_with_survivors`** -- Allocate N objects, keep M% alive as
  roots, collect. Vary M to measure mark phase scaling with live set size.

- **`alloc_into_recycled`** -- After collection leaves holes, measure
  allocation throughput into recycled blocks vs fresh blocks.

Around 200-300 lines total. Each benchmark constructs a `Heap`, gets a
`MutatorHeapView`, and operates directly on the GC internals.

### 1e: Bench script

**File**: `scripts/gc-bench.sh`

Two modes:

```
./scripts/gc-bench.sh baseline    # Run suite, save to gc-bench-baseline.json
./scripts/gc-bench.sh compare     # Run suite, compare against baseline
```

The script:

1. Builds release (`cargo build --release`)
2. Runs each bench program 5 times with `--statistics-file` and
   `--heap-limit-mib 4`
3. Takes the median of each timing metric
4. In `baseline` mode: writes medians to `gc-bench-baseline.json`
5. In `compare` mode: runs again, computes percentage change per metric,
   flags regressions beyond a threshold (>5% slower)

Bench programs included:

- `harness/test/bench/001_naive_fib.eu`
- `harness/test/bench/002_thunk_updates.eu`
- `harness/test/bench/003_smarter_fib.eu`
- `harness/test/bench/004_generations.eu`
- `harness/test/bench/005_drop_cons.eu`
- `harness/test/bench/006_short_lived.eu` (new)
- `harness/test/bench/007_long_lived_graph.eu` (new)
- `harness/test/bench/008_fragmentation.eu` (new)

Example output:

```
004_generations:  VM-CollectorMark   0.0034s -> 0.0031s  (-8.8%)  OK
004_generations:  VM-CollectorSweep  0.0012s -> 0.0003s  (-75.0%) OK
006_short_lived:  VM-Total           0.0089s -> 0.0091s  (+2.2%)  OK
008_fragmentation: blocks_recycled   12      -> 18        (+50.0%) OK
```

### 1f: Agent workflow documentation

**File**: `docs/gc-benchmarking.md`

Step-by-step procedure for any GC modification:

1. Ensure clean working tree
2. Run `./scripts/gc-bench.sh baseline`
3. Make the GC change
4. Run `cargo test` (correctness gate)
5. Run `./scripts/gc-bench.sh compare`
6. Review output -- no metric should regress beyond threshold
7. If investigating: run `cargo bench -- gc` for Criterion detail
8. Include summary of benchmark results in commit message

---

## Phase 2: Lazy Sweeping

### 2a: What changes

Currently in `collect()` (`src/eval/memory/collect.rs:160-163`), the sweep
phase runs eagerly -- all blocks are scanned for holes and moved to the
recycled list immediately after marking. The full sweep cost is paid during
the stop-the-world pause.

With lazy sweeping, the collector only marks. Sweeping happens incrementally
at allocation time -- when the allocator needs a new block, it sweeps the
next unswept block to find holes, then either uses it (if recyclable) or
moves it to `rest` (if fully occupied).

### 2b: Heap state changes

**File**: `src/eval/memory/heap.rs`

The block categories change from:

```
head, overflow, rest, recycled, lobs, recycled_lobs
```

to:

```
head, overflow, unswept, rest, recycled, lobs, recycled_lobs
```

After collection, blocks that were in `rest` move to `unswept` instead of
being eagerly swept. The existing `rest` list retains its meaning (blocks
that have been swept but are not recyclable, i.e. fully occupied).

### 2c: Collection changes

**File**: `src/eval/memory/collect.rs`

- Remove the sweep call from `collect()`
- After marking, move all non-head/overflow blocks to `unswept`
- The `Clock` still records `CollectorSweep` but the duration during
  collection drops to near zero (just the list move)

### 2d: Allocation changes

**File**: `src/eval/memory/heap.rs` (allocation paths)

When the allocator needs a new block (head block exhausted, no recycled
blocks available):

1. Pop a block from `unswept`
2. Sweep it (scan line map, identify holes)
3. If it has usable holes: use it as the new head/recycled block
4. If it is fully live: move to `rest`, try next unswept block
5. If `unswept` is empty: allocate a fresh block from the OS

The actual per-block sweep logic (`recycle()` in `bump.rs`) is unchanged --
just called lazily instead of eagerly.

### 2e: Sweep timing

Track total sweep time for benchmarking by switching the `Clock` to
`CollectorSweep` around each individual block sweep at allocation time.
The aggregate still appears in statistics output.

### 2f: What doesn't change

- Mark phase -- identical
- Object headers -- unchanged
- `GcScannable` trait -- unchanged
- Line map format -- unchanged
- Per-block sweep logic (`recycle()` in `bump.rs`) -- unchanged
- Emergency collection -- still triggers a full mark; unswept blocks get
  lazily swept on demand

### 2g: Risk and validation

**Risks:**

- Allocation latency becomes less predictable (some allocations trigger a
  sweep). This is the standard Immix trade-off -- shorter GC pauses at the
  cost of occasional slower allocations.
- Must ensure `unswept` blocks are not accidentally used as fresh blocks
  before sweeping.

**Validation:**

1. All existing tests pass (`cargo test`)
2. Benchmark comparison shows reduced `VM-CollectorSweep` time during
   collection pause
3. `VM-Total` should remain similar or improve (sweep work is the same
   total, just distributed)
4. The stress programs (`006_short_lived` and `004_generations`) should show
   lower peak pause times

---

## Phase 3: Opportunistic Evacuation

### 3a: What this adds

The core Immix innovation: during collection, instead of always marking
objects in place, the collector can move objects out of fragmented blocks
into fresh blocks, compacting the heap. This is opportunistic -- it only
evacuates blocks where fragmentation is bad enough to justify the copy cost.

### 3b: Deciding when to evacuate

Before each collection, the collector analyses heap fragmentation using the
existing `FragmentationAnalysis` struct (already in `heap.rs` but currently
unused):

- **Candidate blocks**: Blocks where occupancy is below a threshold (e.g.
  <50% of lines marked). These are fragmented enough that evacuating their
  live objects to a fresh block saves space.
- **Available space**: Need at least one fresh block to evacuate into. If
  the heap is completely full, fall back to mark-in-place.
- **Decision**: If there are candidate blocks and space to evacuate, mark
  that collection cycle as an "evacuating" cycle. Otherwise, mark-in-place
  as today.

Uses fragmentation data from the previous collection's line maps -- we know
which blocks had poor occupancy last time.

### 3c: Evacuation during marking

During an evacuating collection, the mark phase changes behaviour for
objects in candidate blocks:

1. When scanning reaches an object in a candidate block, instead of marking
   it in place:
   - Copy the object to the evacuation target block (bump-allocate into it)
   - Set the forwarding pointer in the old object's header (the field
     already exists in `AllocHeader` but is currently unused)
   - Mark the new copy
2. When scanning reaches an object that has already been forwarded:
   - Follow the forwarding pointer to the new location
   - Use the new location for any references being updated

### 3d: Reference updating

As the mark phase traces references, if a reference points to a forwarded
object, update the reference in place to the new location. This means the
scan must be able to write to the referring object, not just read it.

The `GcScannable` trait needs extending. Currently `scan()` pushes
discovered pointers but does not support rewriting them. Add an
`update_refs()` method (or modify `scan()` to support reference rewriting)
that follows the same traversal but updates forwarded pointers.

The `CollectorHeapView` already has mutable access to the heap, so the
mutation is possible within the existing safety model.

This approach (update during scan) follows the Immix paper and avoids an
extra full-heap traversal pass.

### 3e: What changes

- **`AllocHeader`** (`src/eval/memory/header.rs`) -- the `forwarded_to`
  field gets used. Add `is_forwarded()` / `set_forwarded()` methods.

- **`CollectorHeapView`** (`src/eval/memory/collect.rs`) -- add
  `evacuate()` method that copies an object and sets forwarding pointer.

- **`GcScannable`** (`src/eval/memory/collect.rs`) -- extend with
  `scan_and_update()` or modify `scan()` to support reference rewriting.
  Needs careful design to maintain the lifetime safety model.

- **`collect()`** (`src/eval/memory/collect.rs`) -- takes a strategy enum
  indicating mark-in-place vs evacuating. Evacuation cycles use the
  modified scan path.

- **`Heap`** (`src/eval/memory/heap.rs`) -- add evacuation target block
  management. Track which blocks are candidates.

- **Lazy sweep interaction** -- evacuated candidate blocks become fully dead
  after evacuation (all live objects moved out), so they can be returned to
  the free list without sweeping.

### 3f: What doesn't change

- Allocation paths (bump allocation unchanged)
- Non-evacuating collections (mark-in-place path identical to phase 2)
- Object layout and size classes
- Large objects (LOBs are never evacuated -- too expensive to copy)

### 3g: Risk and validation

**Risks:**

- Reference updating is the most complex part. A missed pointer update is a
  use-after-move bug -- hard to debug, potentially silent corruption.
- The `GcScannable` trait change touches every heap object type. Needs
  careful review.
- Forwarding pointer chasing adds overhead to the mark phase even when it
  does not find forwarded objects.

**Validation:**

1. All existing tests pass (`cargo test`)
2. New unit tests specifically for evacuation: create fragmented heap,
   evacuate, verify objects accessible at new locations
3. `008_fragmentation.eu` stress test should show improved `blocks_recycled`
   and reduced `blocks_used` after evacuation
4. Non-fragmented workloads should show no regression (evacuation not
   triggered)
5. Benchmark comparison confirms mark phase is only slightly slower
   (forwarding check overhead) and fragmentation recovery is measurably
   better

---

## Beads

Existing beads to close on completion:

- **eu-2ij** -- GC: Implement lazy sweeping optimisation (phase 2)
- **eu-5si** -- GC: Implement full reference updating system (phase 3)
- **eu-w68** -- Memory management unsafe code lacks safety documentation
  (address as part of phase 3, which adds new unsafe code for evacuation)

New beads to create for phase 1 (benchmarking infrastructure) as no
existing bead covers this work.
