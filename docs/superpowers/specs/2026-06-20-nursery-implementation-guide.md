# Nursery P3+P4 Implementation Guide

**For**: Furnace
**Context**: Two previous attempts deviated from the spec by replacing
`flip_mark_state()` with a `HashSet`. This document proves the existing
mark convention supports sticky-mark-bit directly and provides exact
implementation logic. **Do not deviate from this.**

## Mark Convention (existing, unchanged)

```rust
// header.rs — DO NOT CHANGE
new_unmarked(ms):   mark_bit = !ms
mark_with_state(ms): mark_bit = ms
is_marked_with_state(ms): mark_bit == ms

// heap.rs — DO NOT CHANGE  
is_marked(obj):     header.is_marked_with_state(self.mark_state)
mark_object(obj):   header.mark_with_state(self.mark_state)
flip_mark_state():  self.mark_state = !self.mark_state
```

## State Transition Proof

### With mark_state = false:

| Object | mark_bit | is_marked? |
|--------|----------|------------|
| Newly allocated | true (!false) | true == false → NO |
| Marked (survived GC) | false (=ms) | false == false → YES |

### After flip → mark_state = true:

| Object | mark_bit | is_marked? |
|--------|----------|------------|
| Survived previous GC | false | false == true → NO (needs retrace) |
| Allocated after flip | false (!true) | false == true → NO (young) |

Both appear unmarked — correct. The first minor after a major is
effectively a full trace. This is expected.

### Full cycle walkthrough:

```
mark_state = false

MUTATOR: allocates A, B, C → mark_bit = true → is_marked = NO (young)

MINOR (no flip):
  Trace roots → A, B reachable
  mark_object(A) → mark_bit = false → is_marked = YES (tenured)
  mark_object(B) → mark_bit = false → is_marked = YES (tenured)
  C stays mark_bit = true → is_marked = NO (dead, reclaimable)

MUTATOR: allocates D → mark_bit = true → is_marked = NO (young)
  A, B: mark_bit = false → is_marked = YES (old)

MINOR (no flip):
  Trace roots → A, B, D reachable
  A: is_marked = YES → SKIP (old, don't retrace)
  B: is_marked = YES → SKIP
  mark_object(D) → mark_bit = false → tenured
  
MAJOR (flip at END):
  Reset line marks
  Trace ALL from roots → mark everything reachable with mark_state=false
  Sweep unreachable
  flip_mark_state() → mark_state = true
  
  All survivors: mark_bit = false, is_marked(true) = NO
  (Everything appears young — first post-major minor is full trace)

MUTATOR: allocates E → mark_bit = false (!true) → is_marked(true) = NO

MINOR (no flip):
  Everything unmarked → full trace (expected post-major)
  mark_object on reachables → mark_bit = true
  After: survivors is_marked(true) = YES (old)

MUTATOR: allocates F → mark_bit = false (!true) → is_marked(true) = NO
  Old objects: mark_bit = true → YES
  F: mark_bit = false → NO (young)
  
  Steady state restored. Minors skip old, trace young.
```

## P3: collect_minor() — exact logic

```rust
pub fn collect_minor(
    roots: &mut dyn GcScannable,
    heap: &mut Heap,
    clock: &mut Clock,
    dump_heap: bool,
) {
    clock.switch(ThreadOccupation::CollectorMark);

    let mut heap_view = CollectorHeapView {
        heap,
        minor_visited: None, // NOT used — no HashSet
    };

    // DO NOT call reset() — preserve old objects' line marks
    // DO NOT call flip_mark_state()

    // Trace from roots using the EXISTING mark machinery.
    // is_marked() returns true for old objects → mark() is a no-op
    // is_marked() returns false for young objects → mark() tenures them
    let mut queue = VecDeque::default();
    let mut scan_buffer = Vec::new();
    let scope = Scope();

    roots.scan(&scope, &mut heap_view, &mut scan_buffer);
    queue.extend(scan_buffer.drain(..));

    while let Some(scanptr) = queue.pop_front() {
        scanptr.get().scan(&scope, &mut heap_view, &mut scan_buffer);
        queue.extend(scan_buffer.drain(..));
    }

    // Post-minor verification (EU_GC_VERIFY >= 2)
    let verify = super::gc_debug::verify_level();
    if verify >= 2 {
        verify_post_minor(roots, heap_view.heap);
    }

    // Record minor collection (no flip, no sweep for now)
    heap_view.heap.record_minor_collection(0.0);
}
```

### Key points:
1. **NO reset()** — don't clear line marks (old objects need theirs)
2. **NO flip_mark_state()** — the whole point of sticky-mark-bit
3. **NO HashSet** — the existing `is_marked()` provides cycle
   termination. Old objects are marked → `mark()` is a no-op →
   the trace naturally stops at old object boundaries.
4. The existing `mark()` in `CollectorHeapView` already checks
   `!self.heap.is_marked(obj)` before marking. Old objects pass
   this check as YES → skip. Young objects pass as NO → mark.

### Why no HashSet is needed for cycles:
Old objects are already marked. If a young object points to an old
object, `is_marked(old) = YES` → the trace stops. If old objects form
a cycle, they're all marked → no infinite loop. The only concern is
cycles among YOUNG objects — but young objects start unmarked and get
marked on first visit, so the second visit sees `is_marked = YES` and
stops. Same as major collection — the mark bit IS the visited set.

### Minor sweep (deferred — not required for correctness):
Minor collections do NOT sweep. Dead young objects' memory is
reclaimed at the next major. This is simpler and avoids the complexity
of selectively resetting line marks for young-only lines. The adaptive
scheduler escalates to major when reclamation is needed.

## P4: Write barrier — exact logic

In `vm.rs`, at the thunk update site (`MachineState::update` or
wherever `EnvFrame::update` is called):

```rust
// After updating the env frame with the new closure value:
//
// If the frame is old (marked) and the closure's code or env
// points to young (unmarked) objects, tenure them immediately.
//
// Thunk updates are write-once, so this barrier fires at most
// once per thunk.

if heap.is_marked(environment) {
    // The frame is old. Check if it now points to young objects.
    // If so, mark them (tenure/promote).
    write_barrier_mark_young(heap, closure);
}
```

```rust
fn write_barrier_mark_young(heap: &Heap, closure: &SynClosure) {
    // Mark the closure's code pointer if young
    let code = closure.code();
    if code != NonNull::dangling() && !heap.is_marked(code) {
        heap.mark_object(code);
        heap.mark_line(code);
    }

    // Mark the closure's environment pointer if young
    if let Some(env) = closure.env() {
        if !heap.is_marked(env) {
            heap.mark_object(env);
            heap.mark_line(env);
        }
    }
}
```

### Key points:
1. **Shallow mark only** — we mark the immediate pointers (code, env)
   but not their transitive closure. This is safe because:
   - If a minor collection runs, it will trace from roots and discover
     anything reachable through the promoted objects
   - If a major collection runs, it traces everything anyway
2. The barrier only fires when writing to an OLD frame (is_marked=YES)
   with a YOUNG value (is_marked=NO). This is the only old→young edge.
3. Thunk updates are write-once — the barrier fires at most once per
   thunk in the program's lifetime.

## P5: Minor/major scheduling

```rust
pub fn collect(roots, heap, clock, dump_heap) {
    if heap.should_collect_major() {
        collect_major(roots, heap, clock, dump_heap);
    } else {
        collect_minor(roots, heap, clock, dump_heap);
    }
}
```

`should_collect_major()`:
- After N minor collections (start with N=8)
- When heap blocks exceed limit (existing policy)
- The adaptive escalation can be refined later

## verify_post_minor() — old→young invariant

```rust
fn verify_post_minor(roots: &mut dyn GcScannable, heap: &Heap) {
    // After a minor collection, ALL reachable objects must be marked.
    // Any unmarked reachable object means the write barrier or minor
    // trace missed it.
    
    // Re-traverse from roots using a separate visited set (HashSet
    // is fine HERE — this is verification, not collection).
    // For each reachable object:
    //   assert!(heap.is_marked(obj), "post-minor: unmarked reachable object");
}
```

## What NOT to change

1. `flip_mark_state()` — used by major GC, works correctly
2. `is_marked()` / `mark_object()` — the convention is correct
3. `new_unmarked()` — the `!mark_state` convention is correct
4. `collect_major()` — the existing major GC is correct
5. Do NOT introduce a `HashSet` for collection traversal
6. Do NOT add new mark bits or change the header layout
