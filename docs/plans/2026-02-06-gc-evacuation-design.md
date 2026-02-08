# GC Evacuation Design

Date: 2026-02-06

## 1. Overview and Goals

Implement opportunistic evacuation for the Immix garbage collector --
Phase 3 of the GC Immix completion plan. During collection, instead of
always marking objects in place, the collector can move objects out of
fragmented blocks into fresh blocks, compacting the heap.

This is the core Immix innovation: selective object movement during the
mark phase to reduce fragmentation and improve allocation throughput.

### Success Criteria

- Objects in fragmented blocks are evacuated to fresh blocks during
  collection
- All existing tests pass throughout (`cargo test`)
- `008_fragmentation.eu` stress test shows improved `blocks_recycled`
  and reduced `blocks_used`
- Non-fragmented workloads show no regression (evacuation not triggered)
- Mark phase overhead from forwarding checks is minimal (<5%)
- All new unsafe code has safety documentation

---

## 2. Current State

The codebase already has significant scaffolding for evacuation:

- **`AllocHeader`** (`src/eval/memory/header.rs`): Has `forwarded_to:
  Option<NonNull<()>>`, `is_forwarded()`, `set_forwarded()`, and
  `clear_forwarded()` methods -- all present but unused.

- **`CollectionStrategy`** (`src/eval/memory/heap.rs`): Enum with
  `MarkInPlace`, `SelectiveEvacuation(Vec<usize>)`, and
  `DefragmentationSweep` variants -- defined but unused.

- **`FragmentationAnalysis`** (`src/eval/memory/heap.rs`): Struct with
  block density categorisation and `analyze_fragmentation()` method --
  implemented but never drives decisions.

- **`GcScannable` trait** (`src/eval/memory/collect.rs`): Has `scan()`
  method that pushes discovered grey pointers but does not support
  reference rewriting. Needs a companion `scan_and_update()` method.

- **`collect()` function** (`src/eval/memory/collect.rs`): Takes
  `roots: &dyn GcScannable` (immutable borrow). Needs mutable roots
  for reference updating.

- **`Native::Str` and `Native::Set`**: Contain `RefPtr<HeapString>` and
  `RefPtr<HeapSet>` heap pointers that are not traced during GC scan.
  These must be traced for evacuation to correctly update all references.

### GcScannable Implementations

The following types implement `GcScannable` and will each need a
`scan_and_update()` method:

| Type | File | Heap Pointers |
|------|------|---------------|
| `LambdaForm` | `src/eval/memory/syntax.rs` | `body: NonNull<HeapSyn>` |
| `HeapSyn` | `src/eval/memory/syntax.rs` | Various `NonNull<HeapSyn>`, arrays |
| `SynClosure` | `src/eval/machine/env.rs` | `code: RefPtr<HeapSyn>`, `env: RefPtr<EnvFrame>` |
| `EnvFrame` | `src/eval/machine/env.rs` | `bindings: Array<SynClosure>`, `next: Option<NonNull<EnvFrame>>` |
| `Continuation` | `src/eval/machine/cont.rs` | Branch tables, environments, fallbacks |
| `MachineState` | `src/eval/machine/vm.rs` | `globals`, `closure`, `stack` (roots) |
| `Vec<NonNull<T>>` | `src/eval/memory/collect.rs` | Generic pointer vector |

---

## 3. Sub-bead 1: Trace Native::Str and Native::Set Heap Pointers

### Problem

`Native::Str(RefPtr<HeapString>)` and `Native::Set(RefPtr<HeapSet>)`
contain heap pointers but these are not traced during GC scanning. The
`HeapSyn::Atom { evaluand }` and `HeapSyn::Cons { args }` paths mark
only the `Ref` array, not the `Native` variants within `Ref::V(...)`.

For mark-in-place collection this is not a correctness bug (the objects
are still reachable via other paths, and line marking keeps them alive).
But for evacuation, every pointer that could reference a moved object
must be discoverable so it can be updated.

### Changes

**File: `src/eval/memory/syntax.rs`**

In the `GcScannable` impl for `HeapSyn`, when scanning `Ref` arrays
(in `Cons`, `App`, `Bif` arms and anywhere `Ref::V(Native::Str(ptr))`
or `Ref::V(Native::Set(ptr))` can appear), add marking calls:

```rust
// When iterating Ref values in arrays:
match ref_val {
    Ref::V(Native::Str(ptr)) => { marker.mark(*ptr); }
    Ref::V(Native::Set(ptr)) => { marker.mark(*ptr); }
    _ => {}
}
```

This does not change the scan output (strings and sets are leaf objects
with no further heap references), but it ensures their line marks are
set and, crucially, that evacuation can discover and update these
pointers.

### Validation

- All existing tests pass
- Add a unit test that allocates a `HeapString`, creates a `Native::Str`
  referencing it, collects, and verifies the string's line is marked

---

## 4. Sub-bead 2: Change collect() to Take Mutable Roots

### Problem

The current `collect()` signature is:

```rust
pub fn collect(roots: &dyn GcScannable, heap: &mut Heap, clock: &mut Clock, dump_heap: bool)
```

The `roots` parameter is an immutable borrow. For evacuation, the
collector must be able to update root pointers when a root's referent is
evacuated. This requires mutable access to the roots.

### Changes

**File: `src/eval/memory/collect.rs`**

Change the signature to:

```rust
pub fn collect(roots: &mut dyn GcScannableMut, heap: &mut Heap, clock: &mut Clock, dump_heap: bool)
```

Where `GcScannableMut` extends `GcScannable` with mutable scanning.
Alternatively, change `GcScannable::scan()` to take `&mut self`:

```rust
pub trait GcScannable {
    fn scan<'a>(
        &'a mut self,
        scope: &'a dyn CollectorScope,
        marker: &mut CollectorHeapView<'a>,
        out: &mut Vec<ScanPtr<'a>>,
    );
}
```

The simpler approach: just change the `roots` parameter to `&mut dyn
GcScannable` and update the trait to take `&mut self`. All current
implementations already have mutable access available at the call site
(the `MachineState` is owned by `Machine`).

**File: `src/eval/machine/vm.rs`**

Update the `collect()` call in `Machine::step()` or wherever GC is
triggered to pass `&mut self.state` instead of `&self.state`.

### Validation

- All existing tests pass
- The change is purely mechanical -- no behaviour change for
  mark-in-place collection

---

## 5. Sub-bead 3: Evacuation Target Block Management

### Problem

When evacuating, the collector needs fresh blocks to copy live objects
into. These "evacuation target blocks" must be managed separately from
normal allocation blocks to avoid interference with the mutator's bump
pointer.

### Changes

**File: `src/eval/memory/heap.rs`**

Add evacuation target management to `HeapState`:

```rust
/// Block currently being used as evacuation target during collection
evacuation_target: Option<BumpBlock>,
/// Blocks filled during evacuation (become part of rest after collection)
filled_evacuation_blocks: Vec<BumpBlock>,
```

Add methods to `Heap`:

- `acquire_evacuation_target() -> Option<&mut BumpBlock>`: Get or
  allocate an evacuation target block. Returns `None` if the heap limit
  would be exceeded (fall back to mark-in-place for remaining objects).

- `bump_allocate_in_target(size: usize) -> Option<NonNull<u8>>`: Bump
  allocate into the evacuation target. If the current target is full,
  move it to `filled_evacuation_blocks` and acquire a new one.

- `finalise_evacuation()`: After collection, move all evacuation blocks
  (target + filled) into `rest`. These blocks are freshly allocated and
  fully live -- no need to sweep them.

**File: `src/eval/memory/collect.rs`**

Add to `CollectorHeapView`:

- `evacuate_alloc(size: usize) -> Option<NonNull<u8>>`: Delegate to
  `heap.bump_allocate_in_target(size)`.

### Validation

- Unit test: acquire target, allocate into it, finalise, verify blocks
  appear in rest
- Verify heap limit is respected (no unbounded allocation during
  evacuation)

---

## 6. Sub-bead 4: Object Evacuation (memcpy + Forwarding)

### Problem

The core evacuation operation: copy an object from a fragmented block to
an evacuation target block, set the forwarding pointer in the old
location, and mark the new copy.

### Changes

**File: `src/eval/memory/collect.rs`**

Add `evacuate()` method to `CollectorHeapView`:

```rust
/// Evacuate an object from a candidate block to the evacuation target.
///
/// # Safety
///
/// - `obj` must point to a valid, live heap object with an AllocHeader
///   prefix
/// - The object must be in a candidate (fragmented) block
/// - The object must not already be forwarded
/// - The evacuation target must have sufficient space
///
/// Returns the new location of the object, or None if evacuation
/// failed (e.g. target block full and no replacement available).
pub fn evacuate<T>(&mut self, obj: NonNull<T>) -> Option<NonNull<T>> {
    let header = self.heap.get_header_mut(obj);
    let size = header.length() as usize + size_of::<AllocHeader>();

    // Allocate space in evacuation target
    let new_raw = self.heap.bump_allocate_in_target(size)?;

    // Copy object (header + payload) to new location
    // SAFETY: Both source and destination are valid, non-overlapping,
    // properly aligned heap memory. Size is the full allocation
    // including header.
    unsafe {
        let src = (obj.as_ptr() as *const u8)
            .sub(size_of::<AllocHeader>());
        std::ptr::copy_nonoverlapping(src, new_raw.as_ptr(), size);
    }

    // Compute new object pointer (after header)
    let new_obj = unsafe {
        NonNull::new_unchecked(
            new_raw.as_ptr().add(size_of::<AllocHeader>()) as *mut T
        )
    };

    // Set forwarding pointer in old header
    header.set_forwarded(new_obj.cast());

    // Mark the new copy
    self.heap.mark_object(new_obj);
    self.heap.mark_line(new_obj);

    Some(new_obj)
}
```

**File: `src/eval/memory/heap.rs`**

Add `get_header_mut()` method to `Heap`:

```rust
/// Get mutable access to an object's AllocHeader.
///
/// # Safety
///
/// The pointer must be to a valid heap object allocated with an
/// AllocHeader prefix.
pub fn get_header_mut<T>(&mut self, obj: NonNull<T>) -> &mut AllocHeader {
    unsafe {
        let header_ptr = (obj.as_ptr() as *mut AllocHeader).sub(1);
        &mut *header_ptr
    }
}
```

Add `is_in_candidate_block()` method to check whether an object resides
in a block marked for evacuation.

### Validation

- Unit test: allocate object, evacuate it, verify forwarding pointer set
  and new copy is accessible
- Verify old object's header shows `is_forwarded() == true`
- Verify new object has correct data (byte-for-byte comparison)

---

## 7. Sub-bead 5: Implement scan_and_update for All GcScannable Types

### Problem

During an evacuating collection, after objects are copied and forwarding
pointers set, all live objects must have their internal pointers updated
to reflect the new locations. The existing `scan()` method only reads
pointers -- it cannot rewrite them.

### Design

Add a `scan_and_update()` method to `GcScannable`:

```rust
pub trait GcScannable {
    fn scan<'a>(
        &'a self,
        scope: &'a dyn CollectorScope,
        marker: &mut CollectorHeapView<'a>,
        out: &mut Vec<ScanPtr<'a>>,
    );

    /// Scan this object and update any forwarded pointers in place.
    /// Called during the update phase of an evacuating collection.
    /// Default implementation does nothing (for types with no
    /// rewritable pointers).
    fn scan_and_update<'a>(
        &'a mut self,
        _heap: &'a CollectorHeapView<'a>,
    ) {}
}
```

### Changes per Type

**`LambdaForm`** (`src/eval/memory/syntax.rs`):
- Check if `body` pointer is forwarded; if so, update to new location

**`HeapSyn`** (`src/eval/memory/syntax.rs`):
- For each `NonNull<HeapSyn>` field (in `Case`, `Let`, `LetRec`, `Ann`,
  `DeMeta`): check forwarding, update pointer
- For each `Array<Ref>` (in `Cons`, `App`, `Bif`): iterate refs, check
  `Native::Str`/`Native::Set` pointers for forwarding, update
- For `Array<LambdaForm>` (in `Let`, `LetRec`): iterate, check body
  pointers
- For `Array<Option<NonNull<HeapSyn>>>` (branch tables in `Case`):
  iterate, check forwarding, update

**`SynClosure`** (`src/eval/machine/env.rs`):
- Check `code` and `env` pointers for forwarding, update

**`EnvFrame`** (`src/eval/machine/env.rs`):
- Check `next` pointer for forwarding, update
- Iterate `bindings` array, update each closure's pointers

**`Continuation`** (`src/eval/machine/cont.rs`):
- For each variant: check all `NonNull` fields for forwarding, update
- Branch tables: iterate and update forwarded entries

**`MachineState`** (`src/eval/machine/vm.rs`):
- Check `globals`, `closure.code`, `closure.env` for forwarding, update
- Iterate `stack`, update forwarded continuation pointers

**`Vec<NonNull<T>>`** (`src/eval/memory/collect.rs`):
- Iterate, check each pointer for forwarding, update in place

### Forwarding Check Helper

Add to `CollectorHeapView`:

```rust
/// If the object at `ptr` has been forwarded, return the new location.
/// Otherwise return None.
pub fn forwarded_to<T>(&self, ptr: NonNull<T>) -> Option<NonNull<T>> {
    let header = self.heap.get_header(ptr);
    if header.is_forwarded() {
        header.forwarded_to.map(|p| p.cast())
    } else {
        None
    }
}
```

### Validation

- Unit test per type: create object with pointers to evacuated objects,
  call `scan_and_update()`, verify pointers updated
- Integration test: full collection cycle with evacuation, verify all
  references resolve correctly

---

## 8. Sub-bead 6: Wire Evacuation into collect() Cycle

### Problem

Integrate the evacuation components into the main `collect()` function.

### Design

The evacuating collection cycle proceeds as:

1. **Analyse fragmentation**: Call `analyze_fragmentation()` on the heap
   to identify candidate blocks (occupancy below threshold, e.g. <50%
   of lines marked in previous cycle)

2. **Decide strategy**: If candidate blocks exist and evacuation target
   space is available, run an evacuating cycle. Otherwise, fall back to
   mark-in-place.

3. **Mark phase (with evacuation)**: During the normal mark-and-trace
   loop, when scanning reaches an object in a candidate block:
   - Instead of marking in place, call `evacuate()` to copy to target
   - Set forwarding pointer in old location
   - Push the new copy onto the scan queue (to trace its children)

4. **Update phase**: After marking completes, traverse all live objects
   and roots calling `scan_and_update()` to rewrite any pointers to
   forwarded objects.

5. **Finalise**: Call `finalise_evacuation()` to integrate target blocks
   into the heap. Candidate blocks are now fully dead (all live objects
   moved out) and can be returned to the free list without sweeping.

### Changes

**File: `src/eval/memory/collect.rs`**

```rust
pub fn collect(
    roots: &mut dyn GcScannable,
    heap: &mut Heap,
    clock: &mut Clock,
    dump_heap: bool,
) {
    clock.switch(ThreadOccupation::CollectorMark);

    // Decide strategy
    let strategy = heap.choose_collection_strategy();

    let mut heap_view = CollectorHeapView { heap };
    heap_view.reset();

    let mut queue = VecDeque::default();
    let mut scan_buffer = Vec::new();
    let scope = Scope();

    // Mark roots
    roots.scan(&scope, &mut heap_view, &mut scan_buffer);
    queue.extend(scan_buffer.drain(..));

    // Trace
    while let Some(scanptr) = queue.pop_front() {
        // If evacuating and object is in candidate block,
        // evacuate instead of scanning in place
        if strategy.is_evacuating() {
            if let Some(new_ptr) = maybe_evacuate(&mut heap_view, &scanptr) {
                // Push new location for scanning
                queue.push_back(new_ptr);
                continue;
            }
        }
        scanptr.get().scan(&scope, &mut heap_view, &mut scan_buffer);
        queue.extend(scan_buffer.drain(..));
    }

    // Update phase (only if evacuation occurred)
    if strategy.is_evacuating() {
        // Update root pointers
        roots.scan_and_update(&heap_view);
        // Update all live objects (traverse marked objects)
        heap_view.update_all_forwarded_refs();
    }

    clock.switch(ThreadOccupation::CollectorSweep);
    heap_view.defer_sweep();

    // Finalise evacuation blocks
    if strategy.is_evacuating() {
        heap.finalise_evacuation();
    }

    heap.record_collection();
    heap.flip_mark_state();
}
```

**File: `src/eval/memory/heap.rs`**

Add `choose_collection_strategy() -> CollectionStrategy`:

```rust
pub fn choose_collection_strategy(&self) -> CollectionStrategy {
    let analysis = self.analyze_fragmentation();

    // Only evacuate if fragmentation is significant
    if analysis.fragmentation_ratio < 0.2 {
        return CollectionStrategy::MarkInPlace;
    }

    // Need at least one free block for evacuation target
    if !self.can_acquire_evacuation_target() {
        return CollectionStrategy::MarkInPlace;
    }

    // Collect indices of candidate blocks (sparse + fragmented)
    let candidates = self.candidate_block_indices(&analysis);
    if candidates.is_empty() {
        CollectionStrategy::MarkInPlace
    } else {
        CollectionStrategy::SelectiveEvacuation(candidates)
    }
}
```

### Interaction with Lazy Sweeping

Evacuated candidate blocks become fully dead after evacuation (all live
objects moved out). They bypass the lazy sweep queue and go directly to
the free list. Non-candidate blocks follow the normal lazy sweep path.

### Validation

- All existing tests pass
- `008_fragmentation.eu` shows measurably better `blocks_recycled`
- Non-fragmented benchmarks show no regression (strategy remains
  `MarkInPlace`)
- Benchmark comparison confirms mark phase is only slightly slower
  (forwarding check overhead)

---

## 9. Sub-bead 7: Evacuation Testing and Validation

### Test Plan

**Unit Tests** (`src/eval/memory/collect.rs::tests`):

1. **`test_evacuate_single_object`**: Allocate one object in a block,
   mark block as candidate, collect with evacuation, verify object
   accessible at new location via forwarding pointer.

2. **`test_evacuate_with_internal_refs`**: Allocate a Let expression
   whose body points to another HeapSyn. Evacuate the body. Verify the
   Let's body pointer is updated to the new location.

3. **`test_evacuate_preserves_data`**: Evacuate various object types
   (LambdaForm, HeapSyn variants, Continuation). Verify data integrity
   by comparing field values before and after evacuation.

4. **`test_no_evacuation_when_not_fragmented`**: Create a non-fragmented
   heap, run collection, verify `CollectionStrategy::MarkInPlace` is
   chosen and no forwarding pointers are set.

5. **`test_evacuation_target_exhaustion`**: Fill the heap near its
   limit, trigger evacuating collection, verify graceful fallback to
   mark-in-place when no evacuation target can be acquired.

6. **`test_native_str_pointer_updated`**: Allocate a `HeapString`,
   reference it via `Native::Str`, evacuate the string, verify the
   `Native::Str` pointer is updated.

7. **`test_native_set_pointer_updated`**: Same as above for
   `Native::Set` / `HeapSet`.

**Stress Tests** (`harness/test/bench/`):

- Run `008_fragmentation.eu` with `--statistics-file` before and after
- Compare `blocks_recycled`, `blocks_used`, mark phase timing
- Run non-fragmented benchmarks to verify no regression

**Integration**:

- Full `cargo test` suite passes
- `./scripts/gc-bench.sh compare` shows improvement in fragmentation
  metrics without regression elsewhere

---

## 10. Sub-bead 8: Safety Documentation for Evacuation Unsafe Code

### Scope

All `unsafe` blocks introduced by evacuation must have safety comments
documenting:

1. **What invariant the unsafe code relies on**
2. **Why the invariant holds at this call site**
3. **What could go wrong if the invariant is violated**

### Locations Requiring Documentation

- `evacuate()` in `CollectorHeapView`: `ptr::copy_nonoverlapping`,
  pointer arithmetic, `NonNull::new_unchecked`
- `get_header_mut()` in `Heap`: raw pointer dereference
- `bump_allocate_in_target()`: raw pointer arithmetic for bump
  allocation
- `scan_and_update()` implementations: mutable pointer rewriting in
  each `GcScannable` type
- `forwarded_to()`: reading header through raw pointer

### Documentation Standard

Each unsafe block should follow the existing project pattern (visible
in `ScanPtr::from_non_null` and the heap safety model comment at the
top of `heap.rs`):

```rust
// SAFETY: <what invariant holds>
// - <first reason it holds>
// - <second reason it holds>
// - <what the caller guarantees>
unsafe {
    // ...
}
```

### Existing Unsafe to Audit

Review and update safety comments for existing forwarding-related code
in `header.rs` that is currently unused but will become live:

- `AllocHeader::set_forwarded()`
- `AllocHeader::clear_forwarded()`
- `AllocHeader::is_forwarded()`

Also address **eu-w68** (Memory management unsafe code lacks safety
documentation) by auditing all existing unsafe blocks in
`src/eval/memory/` and adding missing safety comments.

---

## Appendix A: Dependency Graph

```
Sub-bead 1: Trace Native::Str/Set pointers
Sub-bead 2: Mutable roots
Sub-bead 3: Evacuation target blocks
Sub-bead 4: Object evacuation (memcpy + forwarding)  ← depends on 3
Sub-bead 5: scan_and_update for all types             ← depends on 1, 2, 4
Sub-bead 6: Wire into collect() cycle                 ← depends on 2, 3, 4, 5
Sub-bead 7: Testing and validation                    ← depends on 6
Sub-bead 8: Safety documentation                      ← depends on 4, 5, 6
```

Sub-beads 1, 2, and 3 can proceed in parallel. Sub-bead 4 requires 3.
Sub-bead 5 requires 1, 2, and 4. Sub-bead 6 brings everything together.
Sub-beads 7 and 8 follow.

## Appendix B: Files Modified

| File | Changes |
|------|---------|
| `src/eval/memory/collect.rs` | `GcScannable` trait extension, `collect()` signature, `CollectorHeapView` methods |
| `src/eval/memory/header.rs` | No structural changes (existing forwarding API used) |
| `src/eval/memory/heap.rs` | Evacuation target management, strategy selection, `get_header_mut()` |
| `src/eval/memory/syntax.rs` | `Native::Str`/`Set` tracing, `scan_and_update()` for `LambdaForm`, `HeapSyn` |
| `src/eval/machine/env.rs` | `scan_and_update()` for `SynClosure`, `EnvFrame` |
| `src/eval/machine/cont.rs` | `scan_and_update()` for `Continuation` |
| `src/eval/machine/vm.rs` | `scan_and_update()` for `MachineState`, mutable root passing |

## Appendix C: Risk Assessment

| Risk | Severity | Mitigation |
|------|----------|------------|
| Missed pointer update (use-after-move) | Critical | Comprehensive `scan_and_update()` coverage + stress tests |
| `Native::Str`/`Set` pointers not updated | High | Sub-bead 1 ensures these are traced; sub-bead 5 ensures they are updated |
| Evacuation target exhaustion | Medium | Graceful fallback to mark-in-place |
| Mark phase overhead from forwarding checks | Low | Check is a single bit test; benchmark validates |
| Interaction with lazy sweep | Medium | Evacuated blocks bypass sweep queue; tested explicitly |
| Mutable root borrow conflicts | Low | Stop-the-world GC ensures exclusive access |

## Appendix D: Related Beads

- **eu-2ij** -- GC: Implement lazy sweeping optimisation (Phase 2, prerequisite, completed)
- **eu-5si** -- GC: Implement full reference updating system (Phase 3, this work)
- **eu-w68** -- Memory management unsafe code lacks safety documentation (addressed by sub-bead 8)
