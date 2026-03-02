# Block Pinning API Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Add a `pin(ref) -> PinGuard` API to `MutatorHeapView` so
that heap memory blocks can be marked as non-evacuatable during GC.
This enables zero-copy string borrowing from the heap -- a `&str` can
be safely held across allocations because the pinned block will not be
moved by the collector's evacuation phase.

**Architecture:** The pinning mechanism is a ref-counted per-block pin
stored in a `HashMap<usize, usize>` (base address to pin count) on
the `Heap`. A `PinGuard` RAII type increments the pin count on
creation and decrements on drop. During evacuating collection, the
collector queries the pin map and skips any object whose block is
pinned. The design is single-threaded (matching the existing heap
model) and uses `UnsafeCell` interior mutability consistent with the
rest of the heap.

**Tech Stack:** Rust, existing Immix GC infrastructure in
`src/eval/memory/`.

**Prerequisites:** The evacuation infrastructure (eu-5si / GC Immix
Phase 3) is already implemented in the codebase.

---

## Background

Read the design doc at
`docs/plans/2026-02-04-string-intrinsics-design.md` (Phase 2 section)
and the evacuation design at
`docs/plans/2026-02-06-gc-evacuation-design.md` before starting.

The current evacuation logic in `collect.rs` calls `try_evacuate()`
for each live heap object during the mark phase. If the object resides
in a candidate block (fragmented / sparse), it is copied to an
evacuation target block and a forwarding pointer is set. The update
phase then rewrites all pointers to forwarded objects.

Pinning prevents evacuation of specific blocks. When a mutator holds a
`&str` borrowed directly from a `HeapString`, the block containing
that string must not be evacuated -- otherwise the `&str` would become
a dangling pointer.

Key files:

- `src/eval/memory/heap.rs` -- `Heap` struct, `HeapState`,
  `is_in_candidate_block()`
- `src/eval/memory/mutator.rs` -- `MutatorHeapView`
- `src/eval/memory/collect.rs` -- `try_evacuate()`,
  `collect_with_evacuation()`
- `src/eval/memory/bump.rs` -- `block_base_of()`, `BumpBlock`
- `src/eval/memory/header.rs` -- `AllocHeader` (notes pinning bit as
  unsupported)

---

## Task 1: Add Pin Count Map to Heap

**Files:**
- Modify: `src/eval/memory/heap.rs`

**Step 1: Add the pin map field to `Heap`**

In the `Heap` struct (around line 1022), add a new field:

```rust
/// Pin counts by block base address.
///
/// A block with a non-zero pin count must not be evacuated.
/// Managed via `pin_block()` and `unpin_block()` methods.
pin_counts: UnsafeCell<HashMap<usize, usize>>,
```

Add `use std::collections::HashMap;` to the imports at the top of the
file (it already imports `VecDeque` from `std::collections`).

**Step 2: Initialise the pin map in constructors**

In `Heap::new()` and `Heap::with_block_limit()`, add:

```rust
pin_counts: UnsafeCell::new(HashMap::new()),
```

**Step 3: Add pin/unpin methods to `Heap`**

Add these methods to the `impl Heap` block:

```rust
/// Increment the pin count for the block containing `ptr`.
///
/// While pinned, the block will not be selected for evacuation.
pub fn pin_block<T>(&self, ptr: NonNull<T>) {
    let base = bump::block_base_of(ptr);
    // SAFETY: Single-threaded access. Pin/unpin is called from the
    // mutator, never concurrently with collection.
    let pin_counts = unsafe { &mut *self.pin_counts.get() };
    *pin_counts.entry(base).or_insert(0) += 1;
}

/// Decrement the pin count for the block at `base_address`.
///
/// When the count reaches zero, the entry is removed and the block
/// becomes eligible for evacuation again.
pub fn unpin_block(&self, base_address: usize) {
    // SAFETY: Single-threaded access. Same invariant as pin_block.
    let pin_counts = unsafe { &mut *self.pin_counts.get() };
    if let Some(count) = pin_counts.get_mut(&base_address) {
        *count -= 1;
        if *count == 0 {
            pin_counts.remove(&base_address);
        }
    }
}

/// Check whether a block is currently pinned.
pub fn is_block_pinned(&self, base_address: usize) -> bool {
    // SAFETY: Read-only access. Single-threaded.
    let pin_counts = unsafe { &*self.pin_counts.get() };
    pin_counts.contains_key(&base_address)
}
```

**Step 4: Verify it compiles**

```bash
cargo build --lib 2>&1 | head -20
```

---

## Task 2: Add PinGuard RAII Type

**Files:**
- Modify: `src/eval/memory/mutator.rs`

**Step 1: Define the PinGuard struct**

Add above the `MutatorHeapView` impl block:

```rust
/// RAII guard that keeps a heap block pinned (non-evacuatable) while
/// held.
///
/// Created by `MutatorHeapView::pin()`. When dropped, the pin count
/// for the block is decremented. If the count reaches zero, the block
/// becomes eligible for evacuation again.
///
/// # Safety
///
/// `PinGuard` borrows from the `Heap` via a raw pointer to avoid
/// lifetime entanglement with `MutatorHeapView`. The heap must
/// outlive all `PinGuard` instances -- which is guaranteed because
/// the heap owns all memory and `PinGuard` is not `Send`/`Sync`.
pub struct PinGuard {
    /// The base address of the pinned block.
    base_address: usize,
    /// Raw pointer to the owning heap (for unpin on drop).
    heap: *const super::heap::Heap,
}

impl Drop for PinGuard {
    fn drop(&mut self) {
        // SAFETY: The heap outlives PinGuard (heap owns all memory,
        // PinGuard is created from a MutatorHeapView which borrows
        // the heap). Single-threaded, no concurrent access.
        let heap = unsafe { &*self.heap };
        heap.unpin_block(self.base_address);
    }
}

impl std::fmt::Debug for PinGuard {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "PinGuard(block @ {:#x})", self.base_address)
    }
}
```

**Step 2: Add `pin()` method to `MutatorHeapView`**

Add to the `impl<'guard> MutatorHeapView<'guard>` block:

```rust
/// Pin the block containing `ptr`, preventing it from being
/// evacuated during garbage collection.
///
/// Returns a `PinGuard` that unpins the block when dropped.
/// Multiple pins on the same block are ref-counted.
pub fn pin<T>(&self, ptr: NonNull<T>) -> PinGuard {
    self.heap.pin_block(ptr);
    PinGuard {
        base_address: super::bump::block_base_of(ptr),
        heap: self.heap as *const super::heap::Heap,
    }
}
```

**Step 3: Update imports**

Add `use std::ptr::NonNull;` to the imports in `mutator.rs` if not
already present (it is used via `RefPtr` but the bare `NonNull` may
need importing).

**Step 4: Verify it compiles**

```bash
cargo build --lib 2>&1 | head -20
```

---

## Task 3: Unit Tests for Pin/Unpin Basics

**Files:**
- Modify: `src/eval/memory/heap.rs` (tests module)

**Step 1: Add basic pin/unpin test**

In the `#[cfg(test)] pub mod tests` block at the bottom of `heap.rs`,
add:

```rust
#[test]
pub fn test_pin_and_unpin_block() {
    let heap = Heap::new();
    let view = MutatorHeapView::new(&heap);

    // Allocate an object
    let ptr = view.atom(Ref::L(0)).unwrap().as_ptr();
    let base = crate::eval::memory::bump::block_base_of(ptr);

    // Block should not be pinned initially
    assert!(!heap.is_block_pinned(base));

    // Pin the block
    let guard = view.pin(ptr);
    assert!(heap.is_block_pinned(base));

    // Drop the guard -- block should be unpinned
    drop(guard);
    assert!(!heap.is_block_pinned(base));
}

#[test]
pub fn test_pin_ref_counting() {
    let heap = Heap::new();
    let view = MutatorHeapView::new(&heap);

    let ptr = view.atom(Ref::L(0)).unwrap().as_ptr();
    let base = crate::eval::memory::bump::block_base_of(ptr);

    // Pin twice
    let guard1 = view.pin(ptr);
    let guard2 = view.pin(ptr);
    assert!(heap.is_block_pinned(base));

    // Drop one -- still pinned
    drop(guard1);
    assert!(heap.is_block_pinned(base));

    // Drop second -- now unpinned
    drop(guard2);
    assert!(!heap.is_block_pinned(base));
}
```

**Step 2: Run the tests**

```bash
cargo test test_pin_and_unpin_block -- --nocapture 2>&1 | tail -5
cargo test test_pin_ref_counting -- --nocapture 2>&1 | tail -5
```

---

## Task 4: Evacuation Skips Pinned Blocks

**Files:**
- Modify: `src/eval/memory/collect.rs`
- Modify: `src/eval/memory/heap.rs` (add `is_block_pinned_by_ptr` helper)

**Step 1: Add pointer-level pinned check to `Heap`**

In `impl Heap`, add:

```rust
/// Check whether the block containing `ptr` is pinned.
pub fn is_ptr_in_pinned_block<T>(&self, ptr: NonNull<T>) -> bool {
    let base = bump::block_base_of(ptr);
    self.is_block_pinned(base)
}
```

**Step 2: Expose pinned check on `CollectorHeapView`**

In `impl CollectorHeapView<'_>` in `collect.rs`, add:

```rust
/// Check whether the block containing `ptr` is currently pinned.
///
/// Pinned blocks must not be evacuated.
pub fn is_pinned<T>(&self, ptr: NonNull<T>) -> bool {
    self.heap.is_ptr_in_pinned_block(ptr)
}
```

**Step 3: Guard `try_evacuate` against pinned blocks**

In the `try_evacuate` function in `collect.rs`, add a pinned check
before the evacuation call. The function currently reads:

```rust
fn try_evacuate(heap_view: &mut CollectorHeapView<'_>, data_ptr: *const u8, candidates: &[usize]) {
    if candidates.is_empty() {
        return;
    }
    let Some(ptr) = NonNull::new(data_ptr as *mut u8) else {
        return;
    };
    if !heap_view.is_in_candidate_block(ptr, candidates) {
        return;
    }
    // Evacuate: copy header + payload, set forwarding pointer in old header
    let _ = heap_view.evacuate(ptr);
}
```

Add the pinned check after the candidate check:

```rust
fn try_evacuate(heap_view: &mut CollectorHeapView<'_>, data_ptr: *const u8, candidates: &[usize]) {
    if candidates.is_empty() {
        return;
    }
    let Some(ptr) = NonNull::new(data_ptr as *mut u8) else {
        return;
    };
    if !heap_view.is_in_candidate_block(ptr, candidates) {
        return;
    }
    // Skip evacuation for pinned blocks
    if heap_view.is_pinned(ptr) {
        return;
    }
    // Evacuate: copy header + payload, set forwarding pointer in old header
    let _ = heap_view.evacuate(ptr);
}
```

**Step 4: Verify it compiles**

```bash
cargo build --lib 2>&1 | head -20
```

---

## Task 5: Test That Pinned Blocks Survive Evacuation

**Files:**
- Modify: `src/eval/memory/collect.rs` (tests module)

**Step 1: Add evacuation-with-pinning test**

In the `#[cfg(test)] pub mod tests` block in `collect.rs`, add:

```rust
/// Test that a pinned block is NOT evacuated during collection.
///
/// Allocates objects, pins one block, forces evacuating collection,
/// and verifies the pinned object stays at its original address.
#[test]
pub fn test_pinned_block_not_evacuated() {
    let mut heap = Heap::new();
    let mut clock = Clock::default();
    let mut pool = crate::eval::memory::symbol::SymbolPool::new();
    clock.switch(ThreadOccupation::Mutator);

    // Fill up blocks to create fragmentation
    let (pinned_ptr, _other_ptrs) = {
        let view = MutatorHeapView::new(&heap);

        // Create garbage to fill initial blocks
        let _garbage: Vec<_> = repeat_with(|| -> LambdaForm {
            LambdaForm::new(1, view.atom(Ref::L(0)).unwrap().as_ptr(), Smid::default())
        })
        .take(512)
        .collect();

        // Create the object we want to pin
        let pinned = view.atom(Ref::L(42)).unwrap().as_ptr();

        // Create more objects in different blocks
        let others: Vec<_> = repeat_with(|| -> LambdaForm {
            LambdaForm::new(1, view.atom(Ref::L(0)).unwrap().as_ptr(), Smid::default())
        })
        .take(256)
        .collect();

        (pinned, others)
    };

    let original_addr = pinned_ptr.as_ptr() as usize;

    // Pin the block containing our object
    let _pin_guard = {
        let view = MutatorHeapView::new(&heap);
        view.pin(pinned_ptr)
    };

    // Do a normal collection first
    collect(&mut vec![pinned_ptr], &mut heap, &mut clock, false);
    heap.flush_unswept();

    // Force evacuating collection if there are rest blocks
    let rest_count = heap.rest_block_count();
    if rest_count > 0 {
        let candidates: Vec<usize> = (0..rest_count + 2).collect();
        collect_with_evacuation(
            &mut vec![pinned_ptr],
            &mut heap,
            &mut clock,
            &candidates,
            false,
        );
    }

    // The pinned object should NOT have been moved
    let final_addr = pinned_ptr.as_ptr() as usize;
    assert_eq!(
        original_addr, final_addr,
        "pinned object should not have been evacuated"
    );

    // Verify the object is still accessible and intact
    let obj: &HeapSyn = unsafe { &*pinned_ptr.as_ptr() };
    match obj {
        HeapSyn::Atom {
            evaluand: Ref::L(42),
        } => {} // correct
        other => panic!("pinned object has wrong data: {:?}", other),
    }
}
```

**Step 2: Run the test**

```bash
cargo test test_pinned_block_not_evacuated -- --nocapture 2>&1 | tail -10
```

---

## Task 6: Filter Pinned Blocks from Candidate Selection

**Files:**
- Modify: `src/eval/memory/heap.rs`

**Step 1: Filter pinned blocks in `analyze_collection_strategy`**

In the `analyze_collection_strategy` method, pinned blocks should be
excluded from the candidate list. After the existing loop that
collects `fragmented_blocks`, add filtering:

Find the line (around line 1639):
```rust
                CollectionStrategy::SelectiveEvacuation(fragmented_blocks)
```

Replace the return of `fragmented_blocks` with a filtered version:

```rust
                // Exclude pinned blocks from evacuation candidates
                let unpinned_candidates: Vec<usize> = fragmented_blocks
                    .into_iter()
                    .filter(|&idx| {
                        let base = match idx {
                            0 => heap_state.head.as_ref().map(|b| b.base_address()),
                            1 => heap_state.overflow.as_ref().map(|b| b.base_address()),
                            n => heap_state.rest.get(n - 2).map(|b| b.base_address()),
                        };
                        base.map_or(true, |addr| !self.is_block_pinned(addr))
                    })
                    .collect();
                if unpinned_candidates.is_empty() {
                    CollectionStrategy::MarkInPlace
                } else {
                    CollectionStrategy::SelectiveEvacuation(unpinned_candidates)
                }
```

**Step 2: Verify it compiles and tests pass**

```bash
cargo build --lib 2>&1 | head -20
cargo test test_pin -- --nocapture 2>&1 | tail -10
```

---

## Task 7: Update Header Comment About Pinning

**Files:**
- Modify: `src/eval/memory/header.rs`

**Step 1: Update the doc comment on `AllocHeader`**

The comment currently states:

```rust
///  - optionally a pinning bit (which we don't support)
```

Change this to:

```rust
///  - pinning (supported at the block level via Heap::pin_block /
///    Heap::unpin_block, not per-object)
```

This reflects the design choice: pinning is at the block granularity
(via a ref-counted map on the heap) rather than a per-object bit in
the header.

**Step 2: Verify it compiles**

```bash
cargo build --lib 2>&1 | head -20
```

---

## Task 8: Run Full Test Suite and Clippy

**Files:**
- None (validation only)

**Step 1: Run clippy**

```bash
cargo clippy --all-targets -- -D warnings 2>&1 | tail -20
```

Fix any warnings. Common issues to watch for:
- Unused imports (e.g. `HashMap` if not used elsewhere)
- Unnecessary `&*` dereferences
- Redundant clones

**Step 2: Run rustfmt**

```bash
cargo fmt --all
```

**Step 3: Run the full test suite**

```bash
cargo test 2>&1 | tail -20
```

All tests must pass, including the existing evacuation tests.

**Step 4: Run pinning-specific tests**

```bash
cargo test test_pin -- --nocapture 2>&1
cargo test test_pinned_block_not_evacuated -- --nocapture 2>&1
```

---

## Appendix A: Design Decisions

### Block-Level vs Object-Level Pinning

Immix papers describe per-object pin bits in the header. We use
block-level pinning (a `HashMap<usize, usize>` on the heap) instead
because:

1. **Simpler**: No change to `AllocHeader` layout or size
2. **Sufficient**: The use case (string borrowing during intrinsics) pins
   one block per borrowed string, and intrinsics are short-lived
3. **No header bloat**: The `AllocHeader` is currently 16 bytes. Adding
   a pin bit would require either stealing from `alloc_length` or
   expanding the bitmap, both of which affect all objects for a feature
   used by few
4. **Ref-counted**: Multiple borrows from the same block naturally
   stack via the count

### PinGuard Lifetime

`PinGuard` deliberately does NOT borrow `MutatorHeapView` or carry a
lifetime parameter. This is because the primary use case
(`str_arg_ref`) needs to hold a `PinGuard` and a `&str` simultaneously
while performing allocations -- which requires the `MutatorHeapView`
to be usable. A lifetime-bound guard would create a borrow conflict.

Instead, `PinGuard` stores a raw `*const Heap` pointer and relies on
the structural invariant that the heap outlives all guards (the heap
is owned by the `Machine` and lives for the entire evaluation).

### Interaction with Emergency Collection

Emergency collections perform a sweep (not evacuation), so pinning
does not affect them. If future emergency collections incorporate
evacuation, the same `is_block_pinned` check will apply.

---

## Appendix B: Files Modified

| File | Changes |
|------|---------|
| `src/eval/memory/heap.rs` | `pin_counts` field, `pin_block()`, `unpin_block()`, `is_block_pinned()`, `is_ptr_in_pinned_block()`, candidate filtering, tests |
| `src/eval/memory/mutator.rs` | `PinGuard` struct, `MutatorHeapView::pin()` method |
| `src/eval/memory/collect.rs` | `CollectorHeapView::is_pinned()`, `try_evacuate()` guard, tests |
| `src/eval/memory/header.rs` | Doc comment update |

---

## Appendix C: Future Work (Out of Scope)

- **`str_arg_ref()` helper**: Returns `&str` borrowed directly from
  heap with automatic pinning. Depends on this plan. Will be a separate
  task (Phase 2b of string intrinsics optimisation).
- **Pin diagnostics**: Count / log pinned blocks during GC for
  debugging. Low priority.
- **Concurrent pinning**: If the heap becomes multi-threaded, the pin
  map will need atomic operations or a lock. Currently unnecessary
  (single-threaded).
