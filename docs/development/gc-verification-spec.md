# Full GC Heap Verification Pass

**Status**: Spec  
**Bead**: eu-dsg2  
**Date**: 2026-04-18

## 1. Overview

Extend `EU_GC_VERIFY` with a level-2 mode that performs comprehensive
structural verification of the heap at multiple checkpoints during
garbage collection. Level 1 (existing) does post-mark re-traversal
only. Level 2 adds header validation, pointer validity checking, line
mark consistency, forwarding pointer lifecycle, and block list
integrity.

## 2. Gating

`EU_GC_VERIFY` becomes a level:

| Value | Behaviour |
|-------|-----------|
| unset / 0 | No verification |
| 1 | Existing: post-mark re-traversal (unchanged) |
| 2 | Full multi-checkpoint structural verification |

Level 2 implies level 1 — the mark completeness check is included in
checkpoint 1.

`verify_enabled()` in `gc_debug.rs` is replaced by `verify_level() -> u8`
returning 0, 1, or 2. Level 1 callers check `>= 1`, level 2 callers
check `>= 2`. Backward compatible.

## 3. HeapVerifier

New struct in `src/eval/memory/gc_verify.rs`:

```rust
struct HeapVerifier<'a> {
    heap: &'a HeapState,
    /// All block memory ranges: (start_ptr, end_ptr)
    block_ranges: Vec<(*const u8, *const u8)>,
    /// Valid allocation addresses found by walking headers
    valid_allocs: HashSet<usize>,
    /// Evacuation target ranges (checkpoint 2 only)
    evac_ranges: Vec<(*const u8, *const u8)>,
}
```

### Construction

Built fresh at each checkpoint:

1. Collect block ranges from all HeapState lists: `head`, `overflow`,
   `rest`, `unswept`, `recycled`, `evacuation_target`,
   `filled_evacuation_blocks`, and `lobs`
2. Walk each block to find allocations and build `valid_allocs`
3. At checkpoint 2, separately index evacuation target ranges

### Walking Allocations

Each block is walked using its LineMap to identify regions containing
live allocations. Within marked line regions, walk headers downward:
each AllocHeader's `alloc_length` gives the payload size, so the next
header is at `current - HEADER_SIZE - align_up(alloc_length)`.

Unmarked lines are holes (dead memory) — skipped during the walk.

During the walk, validate headers inline: reject entries with
`alloc_length == 0`, `alloc_length > BLOCK_SIZE_BYTES - HEADER_SIZE`,
or tag values outside 0-15. Report invalid headers immediately.

Large object blocks contain a single allocation each — validate the
one header directly.

### Pointer Containment Check

```rust
fn is_valid_heap_ptr(&self, ptr: *const u8) -> bool {
    let addr = ptr as usize;
    // Fast check: is it in any known block range?
    self.block_ranges.iter().any(|(lo, hi)|
        addr >= *lo as usize && addr < *hi as usize
    ) && self.valid_allocs.contains(&addr)
}
```

## 4. Checkpoints

### Checkpoint 1 — Post-mark

Runs after the mark phase, before sweep or evacuation. Called from
both the mark-sweep and evacuation paths in `collect.rs`.

Checks:
- **Mark completeness** (existing level 1): re-traverse roots, verify
  all reachable objects are marked
- **Header validity**: every marked object has tag 0-15 and plausible
  `alloc_length`
- **Pointer validity**: every heap pointer within a marked object
  (`Native::Str`, `Native::Set`, `Native::NdArray`, `Native::Vec`,
  all `RefPtr<HeapSyn>` in Case/Let/App/Ann/DeMeta) targets an address
  in `valid_allocs`
- **Line consistency**: every marked object's full allocation (header
  start through header + alloc_length, aligned) has all covering lines
  marked in the block's LineMap

### Checkpoint 2 — Post-evacuation, pre-update

Runs only during evacuation collections, after the evacuate loop,
before `scan_and_update`.

Checks:
- **Forwarding validity**: every object with the forwarded bit set
  has a forwarding pointer targeting an address within an evacuation
  target block (`evac_ranges`)
- **Evacuation copy validity**: every allocation in evacuation target
  blocks has a valid header (tag 0-15, plausible length)
- **Target line marking**: every allocation in evacuation target
  blocks has its covering lines marked

### Checkpoint 3 — Post-update

Runs after `scan_and_update` completes, before sweep. Evacuation path
only.

Checks:
- **No stale forwarding**: no live object has the forwarded bit set
  (all forwarding pointers should have been consumed during update)
- **Pointer liveness**: every pointer in every live object targets a
  marked, non-forwarded allocation
- **No dangling into source**: no live pointer targets an evacuation
  source block (those blocks are about to be recycled)

### Checkpoint 4 — Post-sweep

Runs after sweep / defer_sweep completes. Both paths.

Checks:
- **Block list disjointness**: no block appears in multiple HeapState
  lists (head, overflow, rest, unswept, recycled, evacuation_target,
  filled_evacuation_blocks)
- **Recycled cleanliness**: blocks in `recycled` have no marked lines
- **Rest block liveness**: every block in `rest` has at least one
  marked line

## 5. Integration

### Collector call sites

Mark-sweep path:
```
mark phase
  → if verify_level() >= 2: HeapVerifier::check_post_mark(roots)
  → elif verify_level() >= 1: verify_mark_integrity(roots) [existing]
defer_sweep / sweep
  → if verify_level() >= 2: HeapVerifier::check_post_sweep()
```

Evacuation path:
```
mark phase
  → if verify_level() >= 2: HeapVerifier::check_post_mark(roots)
  → elif verify_level() >= 1: verify_mark_integrity(roots) [existing]
evacuate loop
  → if verify_level() >= 2: HeapVerifier::check_post_evacuate()
scan_and_update
  → if verify_level() >= 2: HeapVerifier::check_post_update(roots)
finalise_evacuation + defer_sweep
  → if verify_level() >= 2: HeapVerifier::check_post_sweep()
```

### Failure behaviour

On any verification failure: `panic!` with a diagnostic message
including:
- Checkpoint name (e.g. "POST-MARK", "POST-EVACUATE")
- Failing check description
- Offending object/pointer address
- Expected vs actual state
- Block identity (which HeapState list it belongs to)

Same approach as existing poison/verify panics — debug assertions,
not recoverable errors.

## 6. Files

| File | Changes |
|------|---------|
| `src/eval/memory/gc_verify.rs` | **New** — HeapVerifier struct and all checkpoint logic |
| `src/eval/memory/gc_debug.rs` | Replace `verify_enabled()` with `verify_level()` |
| `src/eval/memory/collect.rs` | Insert checkpoint calls at four sites |
| `src/eval/memory/mod.rs` | Add `mod gc_verify` |
| `CLAUDE.md` | Update EU_GC_VERIFY documentation |

## 7. Testing

### Primary acceptance criterion

`EU_GC_VERIFY=2 cargo test --test harness_test` — all 50+ harness
tests pass without verification panics.

### Additional tests

- Run programs that triggered historical bugs 1-6 under level 2
- Unit tests for HeapVerifier: header walking on synthetic blocks,
  pointer containment checks, line consistency validation
- Injected faults (stretch goal, `#[cfg(test)]` only): deliberately
  skip a line mark or leave a stale forwarding pointer, verify the
  checker catches it

### CI integration

New job: `EU_GC_VERIFY=2 cargo test` alongside existing ASAN job.
`continue-on-error: true` initially until confident in no false
positives.

## 8. Performance

Level 2 builds a HashSet of all live allocations at each checkpoint,
then re-walks every pointer. For N live objects and M pointers, each
checkpoint is O(N + M). Four checkpoints per evacuation collection.

Acceptable because:
- Gated behind `EU_GC_VERIFY=2` — never in production
- Primary use: test harness and CI verification
- Cost is proportional to heap size, not program runtime
