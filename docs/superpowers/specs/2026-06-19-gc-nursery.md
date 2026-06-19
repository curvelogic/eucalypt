# eu-9tah.1 (W10): Sticky Immix Generational Nursery

**Date**: 2026-06-19
**Bead**: eu-9tah.1
**Agent**: Furnace
**Parent spec**: `2026-06-19-0.10.0-release-design.md`
**ROADMAP section**: W10 (lines 701-776)

## Motivation

The GC re-marks the entire live set every cycle — there is no notion of age,
no remembered set, no way to skip a long-lived object. On traversal-heavy
programs, mark time dominates (>95% of VM time on bench/008). The collector
is "Immix without the generational dimension".

### Profiling evidence (Stopwatch, 2026-06-19)

Under tight heap limits (32-80 MiB), only 2 of 19 AoC examples show
meaningful GC pressure:

- **day02-part-2**: 2.4× thunk turnover ratio at 32 MiB. Time explodes from
  0.06s (no GC) to 11.4s (1,615 GCs, 99.5% GC time). Blocks_recycled=0
  throughout — Immix hole reuse, not full-block recycling.
- **day05-part-2**: 2.0× turnover at 32 MiB. Same pattern.

The other 17 examples have monotonically growing live sets (lazy spine
construction) and hit a hard cliff: 0 GCs → immediate timeout with no
intermediate regime. These will benefit once demand analysis (W11) reduces
unnecessary thunk allocation, and once AoC algorithms are refactored to
avoid head-retention (eu-9tah.6).

The benchmark tests (`tests/harness/bench/`) provide synthetic controlled
cases: `008_long_lived_graph.eu` (long-lived objects), `007_short_lived.eu`
(short-lived thunks), `004_generations.eu` (generational behaviour),
`009_fragmentation.eu` (fragmentation).

## Design

Following the ROADMAP's four-part phased design, adapted for the Sticky Immix
(sticky-mark-bit) approach confirmed during brainstorming.

### Strategy: Sticky-mark-bit generational collection

Young objects are unmarked objects in existing Immix blocks. No separate
nursery space, no copying for promotion.

- **Minor collection**: trace from roots + remembered set WITHOUT flipping
  the mark state. Nursery objects that are reached are marked (tenured in
  place). Unmarked nursery objects are reclaimable.
- **Major collection**: today's full flip-and-trace. All objects are
  re-evaluated.
- **Promotion**: marking an object IS tenuring — no copy, no move, in-place.

This is a near-minimal delta from the existing collector, which already
flips a single mark bit per cycle (`flip_mark_state`, `heap.rs:1716`).

### Phase P0: Update GC documentation

The GC documentation is stale and wrong (claims "no evacuation / eager
sweep" but the collector already does opportunistic evacuation with
forwarding pointers and lazy deferred sweep). Update all doc comments in
`src/eval/memory/` to reflect the actual implementation.

### Phase P1: Allocation-rate collection trigger

**Problem**: `policy_requires_collection()` (`heap.rs:1771`) only fires when
`blocks_allocated >= limit` AND recycled ratio < 25%. With no `--heap-limit-mib`
set, it returns `false` always — allocation rate is ignored despite being
tracked.

**Fix**: Decouple "should we collect?" from the heap limit. Add an adaptive
nursery budget: track allocation volume since last collection; trigger a
minor collection when the nursery budget is exhausted (e.g. after N blocks
allocated since last collection, where N is tuneable, starting at 64 blocks
= 2 MiB).

This is a prerequisite for the nursery — without it, the nursery has no
trigger.

### Phase P2: Finish `DefragmentationSweep`

**Problem**: `DefragmentationSweep::candidates()` (`heap.rs:87`) returns
`Vec::new()` with a `// TODO: all blocks` comment. The worst-fragmentation
case silently degrades to non-compacting.

**Fix**: Replace the stub with the full set of unpinned fragmented block
indices. The enumeration already exists for the stress path — wire it in.

### Phase P3: Sticky-mark-bit minor collection

The core of the nursery. Add a `collect_minor()` function in
`src/eval/memory/collect.rs`:

1. **Roots**: GC roots + remembered set entries (old→young pointers)
2. **Mark phase**: trace from roots, marking reachable nursery objects.
   Do NOT flip the mark state — any nursery object that is marked is now
   tenured (it will survive the next minor collection because it carries
   the current mark bit).
3. **Sweep**: reclaim lines containing only unmarked objects (standard
   Immix line-based reclamation). Unmarked nursery objects in partially-
   occupied lines remain as holes.
4. **Statistics**: track minor collection count, minor mark time, minor
   sweep time separately from major collections.

The existing `collect()` function becomes `collect_major()` — it flips the
mark state and traces everything, as today.

### Phase P4: Write barrier with eager promotion

**The single mutation site**: `EnvFrame::update` (`env.rs`), reached when
an `Update` continuation overwrites the black hole. This is the only
in-place heap write in eucalypt's evaluation model.

**Eager promotion** (preferred): when a mature (marked) frame is updated
with a value pointing to a nursery (unmarked) object, immediately mark
(tenure) the nursery object. Thunk updates are write-once, so there is no
repeated cost — each thunk is updated exactly once.

```rust
// In EnvFrame::update (conceptual):
fn update(&mut self, value: SynClosure) {
    self.inner = value;
    // Eager promotion: if this frame is old (marked) and the
    // value points to a young (unmarked) object, mark it now.
    if self.is_marked() && !value.is_marked() {
        value.mark();  // tenure in place
    }
}
```

**Why not a card table**: the ROADMAP and brainstorming considered a card
table, but eager promotion is simpler and sufficient for eucalypt because:
- Thunk update is the only mutation site
- Updates are write-once (no repeated barrier cost)
- Eager promotion avoids the remembered set scan during minor collection

**Fallback — remembered set**: if any mutation sites beyond `EnvFrame::update`
are discovered (block merge, list cons), use a sequential store buffer as a
remembered set. These entries become additional roots during minor collection.

### Phase P5: Minor/major scheduling

Policy for when to do minor vs major collection:

- **Minor**: when the nursery budget is exhausted (from P1)
- **Major**: after N minor collections (tuneable, start with N=8), or when
  the minor collection fails to reclaim enough space (e.g. reclaimed < 25%
  of nursery), or when fragmentation exceeds the defragmentation threshold
- **Adaptive**: adjust N based on nursery survival rate — high survival
  means most objects are long-lived, so minors are wasted work → increase
  the threshold to trigger majors more often

### New `EU_GC_VERIFY` checkpoint

Add a verification checkpoint after every minor collection that asserts the
**no-untracked-old→young-edge** invariant: traverse all marked (old) objects
and verify that every pointer from an old object to a young (unmarked) object
is either:
- In the remembered set, OR
- The young object was eagerly promoted (marked) during the write barrier

This catches write barrier bugs. Active under `EU_GC_VERIFY=2`.

## Implementation Phasing

| Phase | Description | Risk | Dependencies |
|-------|-------------|------|-------------|
| P0 | Update GC docs | Trivial | None |
| P1 | Allocation-rate trigger | Small/low | None |
| P2 | Finish DefragmentationSweep | Small/low | None |
| P3 | Minor collection + nursery | Medium/medium-high | P1 |
| P4 | Write barrier (eager promotion) | Medium/**high** | P3 |
| P5 | Minor/major scheduling | Medium | P3, P4 |

P0-P2 are independently shippable wins. P3-P5 are the strategic core.

## Benchmark targets

### Primary (must improve)

- `tests/harness/bench/008_long_lived_graph.eu` — CollectorMark fraction
  must drop well below the >95% regime
- `tests/harness/bench/007_short_lived.eu` — net wall-clock gain
- `tests/harness/bench/004_generations.eu` — net wall-clock gain
- `examples/aoc25/day02.eu -t part-2` (at `--heap-limit-mib 32`) — total
  time must be ≤ 2× baseline (vs current 190× slowdown)

### Must not regress

- `tests/harness/bench/001_naive_fib.eu`
- `tests/harness/bench/002_thunk_updates.eu`
- Full harness test suite

### Secondary (should improve)

- `tests/harness/bench/009_fragmentation.eu` — should no longer degrade
  (P2 fixes DefragmentationSweep)
- `examples/aoc25/day05.eu -t part-2` (at `--heap-limit-mib 32`)

## Acceptance Criteria

1. Minor collection implemented (sticky-mark-bit, no mark-state flip)
2. Major collection is the existing full flip-and-trace
3. Write barrier on `EnvFrame::update` with eager promotion
4. Allocation-rate trigger decoupled from heap limit
5. `DefragmentationSweep::candidates()` returns actual block indices
6. `EU_GC_VERIFY=2` includes old→young invariant check after minor collections
7. `EU_GC_POISON=1` passes (use-after-free detection)
8. Statistics distinguish minor vs major collections (count, mark time, sweep time)
9. Primary benchmarks improve per targets above
10. No regression on `001_naive_fib` or `002_thunk_updates`
11. Full test suite passes: `cargo test`
12. `cargo clippy --all-targets -- -D warnings` clean
13. `cargo fmt --all` clean

## Risks

- **Write barrier cost > nursery savings**: a net regression on
  `002_thunk_updates`/`001_naive_fib` not recovered on `008`/`007` would
  falsify the approach. Mitigate: eager promotion behind one predictable
  branch (write-once thunks make this cheap).
- **Generational rewrites crash**: the evacuation path has had aarch64-specific
  bugs. Mitigate with `VERIFY`/`POISON` harness and the new old→young
  invariant check.
- **Most real programs don't benefit yet**: 17/19 AoC examples have
  monotonically growing live sets. The nursery's value compounds with W11
  (demand analysis reducing allocation) and eu-9tah.6 (head-retention
  refactoring).

## Out of Scope

- Concurrent collection — deferred to W21
- Separate nursery space / copying collector — sticky-mark-bit is sufficient
- Card table — eager promotion handles the single mutation site
- Nursery sizing heuristics beyond the simple block-count budget
