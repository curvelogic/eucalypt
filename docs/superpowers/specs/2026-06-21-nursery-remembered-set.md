# Nursery v3: Sticky-Mark-Bit with Remembered Set

**Bead**: eu-9tah.1
**Agent**: Furnace
**Date**: 2026-06-21
**Supersedes**: Previous nursery implementation (PR #875, HashSet full-trace approach)

## Problem

The current nursery (PR #875) traces the ENTIRE heap on every minor
collection using a HashSet for cycle detection. This makes minor = major
in cost. Combined with the allocation-rate trigger (every 8MB), programs
that previously needed 0-2 collections now get 100-200, causing 2-14x
time regressions across all AoC examples.

A genuine generational minor must skip old objects and only trace young
ones. This requires tracking old→young references via a write barrier.

## Previous Attempts and Why They Failed

1. **Sticky-mark-bit + shallow write barrier**: Minor used `is_marked()`
   to skip old objects. Write barrier marked closure's code+env as old.
   UNSOUND: promoting intermediate objects created new old→young edges
   within them — their young children were invisible to subsequent minors.

2. **HashSet full-trace** (current, PR #875): Minor traces all objects
   via HashSet. Correct but O(heap) per minor — no generational benefit.
   Every minor is as expensive as a major.

## Design: Remembered Set

**Write barrier**: When an old frame is updated with a new closure,
record the frame's address in a dirty list. Do NOT mark anything.

```rust
// vm.rs — at MachineState::update(), after cont_env.update():
if view.is_marked(environment) {
    heap.record_dirty_frame(environment);
}
```

**Minor collection**: Trace from roots + dirty frames as extra roots.
Use sticky-mark-bit (existing `mark()` machinery):
- Old objects: `is_marked() = YES` → `mark()` is a no-op → trace skips
- Young objects: `is_marked() = NO` → `mark()` tenures them
- Dirty frames' closures are scanned as additional roots → finds young
  objects reachable through updated old frames transitively

```rust
pub fn collect_minor(roots, heap, clock, dump_heap) {
    // DO NOT call reset() — preserve old objects' line marks
    // DO NOT call flip_mark_state()

    let mut heap_view = CollectorHeapView { heap };
    // NO HashSet — mark bit IS the visited set

    let mut queue = VecDeque::default();
    let mut scan_buffer = Vec::new();
    let scope = Scope();

    // 1. Scan regular roots
    roots.scan(&scope, &mut heap_view, &mut scan_buffer);
    queue.extend(scan_buffer.drain(..));

    // 2. Scan dirty frames as additional roots
    for frame_addr in heap.drain_dirty_frames() {
        // Scan the closure stored in the dirty frame
        // This finds young objects reachable through updated old frames
        if let Some(frame_ptr) = NonNull::new(frame_addr as *mut EnvFrame) {
            unsafe {
                frame_ptr.as_ref().scan(&scope, &mut heap_view, &mut scan_buffer);
            }
            queue.extend(scan_buffer.drain(..));
        }
    }

    // 3. Trace (mark bit provides cycle termination)
    while let Some(scanptr) = queue.pop_front() {
        scanptr.get().scan(&scope, &mut heap_view, &mut scan_buffer);
        queue.extend(scan_buffer.drain(..));
    }

    heap.record_minor_collection();
}
```

### Why this is sound (vs the shallow barrier that failed)

The shallow barrier marked code+env as OLD → minor skipped them →
their young children were invisible. The remembered set does NOT mark
anything. It records the frame address. During minor, the frame's
closure is scanned AS A ROOT — code and env are entered as young
objects (is_marked=NO) → trace follows through transitively → all
young children found and tenured.

No premature promotion. No new old→young edges created by the barrier.

### Why no HashSet is needed

Same reasoning as the original implementation guide: the mark bit IS
the visited set. Old objects are marked → `mark()` is a no-op. Young
objects start unmarked, get marked on first visit. Cycles among young
objects: first visit marks, second visit sees is_marked=YES → stops.

## Major Collection: Flip at START

```rust
pub fn collect_major(roots, heap, clock, dump_heap) {
    // 1. Preliminary minor — tenure all reachable young objects
    collect_minor(roots, heap, clock, false);

    // 2. Flip mark state — everything appears unmarked
    heap.flip_mark_state();

    // 3. Clear dirty frames (not needed — major retraces everything)
    heap.drain_dirty_frames();

    // 4. Reset line marks
    heap_view.reset();

    // 5. Full trace from roots (everything unmarked → full retrace)
    // ... existing trace logic ...

    // 6. Sweep
    // ... existing sweep logic ...

    // NO flip at end — flip was at start
}
```

### First post-major minor is a full trace

After major flip, all objects appear unmarked (mark_bit = old_ms,
is_marked(new_ms) = NO). The first minor after a major traces
everything (expected — standard generational GC behaviour). After
that minor tenures everything, subsequent minors only trace young
objects + dirty frames. O(young + dirty), not O(heap).

## Heap Changes

```rust
// In HeapState:
dirty_frames: Vec<usize>,

// Public API on Heap:
pub fn record_dirty_frame(&self, ptr: NonNull<EnvFrame>) {
    let heap_state = unsafe { &mut *self.state.get() };
    heap_state.dirty_frames.push(ptr.as_ptr() as usize);
}

pub fn drain_dirty_frames(&self) -> Vec<usize> {
    let heap_state = unsafe { &mut *self.state.get() };
    std::mem::take(&mut heap_state.dirty_frames)
}
```

## Scheduling

Fix the forced-major-after-every-minor bug:

```rust
// record_minor_collection should NOT use reclaim_ratio to escalate.
// Minor doesn't sweep, so it can never report a meaningful ratio.
// Instead, use a simple counter: major after N minors.
pub fn record_minor_collection(&self) {
    self.minor_collection_count.set(self.minor_collection_count.get() + 1);
    let minors = self.minors_since_major.get() + 1;
    self.minors_since_major.set(minors);

    let heap_state = unsafe { &mut *self.state.get() };
    heap_state.blocks_replaced_since_gc = 0;
    heap_state.record_collection();
    // NO reclaim_ratio check — don't force major escalation
}
```

Also review the allocation-rate trigger threshold. 256 blocks (8MB)
is too aggressive for programs that fit in RAM. Consider raising to
2048+ blocks (64MB+) or making it proportional to heap size.

## What to Change (summary)

Starting from current master (which has the HashSet minor from PR #875):

1. **collect.rs**: Replace HashSet minor with sticky-mark-bit minor.
   Remove `visited: Option<HashSet<usize>>` from CollectorHeapView.
   Add dirty frame scanning after root scanning. Remove HashSet import
   from collection path (keep in test code if needed).

2. **collect.rs**: `collect_major` — flip at START (preliminary minor +
   flip + drain dirty + reset + trace + sweep). Remove flip at END.

3. **heap.rs**: Add `dirty_frames: Vec<usize>` to HeapState. Add
   `record_dirty_frame()` and `drain_dirty_frames()` to Heap.
   Fix `record_minor_collection()` — remove reclaim_ratio escalation.
   Review allocation-rate trigger threshold.

4. **vm.rs**: Write barrier at `MachineState::update()` — record dirty
   frame, do NOT mark anything: `heap.record_dirty_frame(environment)`.

5. **cont.rs / vm.rs**: Remove `IoTransparentBranch` if present (from
   PR #882 which hasn't merged). Not needed for this change.

## Verification

### Correctness
```
cargo fmt --all
cargo clippy --all-targets -- -D warnings
cargo test
EU_GC_VERIFY=2 EU_GC_POISON=1 cargo test
EU_SOURCE_PRELUDE=1 cargo test --test harness_test
```

### Performance — MANDATORY

Run ALL AoC examples and report before/after stats. Use the baseline
at `examples/aoc25/stats/*.json`. For each example, report:
- Allocs (machine_allocs)
- Wall time
- Minor collections
- Major collections
- Mark time

Use appropriate timeouts based on baseline times (see baseline stats).
Programs that completed at baseline MUST complete now. Any new timeout
is a blocker.

```
cargo build --release
# For each day/part:
timeout <2x baseline time> ./target/release/eu --statistics-file /tmp/nursery-v3/<name>.json examples/aoc25/<day>.eu -t <part>
```

## DO NOT

- Use HashSet for collection traversal
- Mark anything in the write barrier (record only)
- Pass reclaim_ratio to force major escalation
- Defer this to 0.11 — this IS 0.10.0
