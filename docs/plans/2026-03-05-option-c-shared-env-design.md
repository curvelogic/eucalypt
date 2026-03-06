# Option C: Shared Environment for Data Constructor Destructuring

## Problem Statement

When case-matching on a data constructor, `env_from_data_args` (vm.rs:492-522)
copies closures from the constructor's environment into a fresh `Array` in a new
frame. This severs the link between the copy and the original: when a copied
thunk is later forced, the `Update` continuation writes the computed value back
to the *copy's* frame slot, not the original. Any other reference to the
original thunk still sees the unevaluated version, defeating memoisation.

This affects all data constructors (cons cells, blocks, pairs) — any thunk
stored in a constructor field loses sharing when pattern-matched.

## Root Cause Analysis

### Array Sharing Semantics

`Array<T>` (memory/array.rs) wraps a `RawArray<T>` which stores a raw
`RefPtr<T>` to heap-allocated backing storage. `RawArray` is `Copy`. When an
`Array` is cloned, the clone **shares the same backing memory** — both instances
point to the same heap allocation. Mutations via `Array::set()` write through
the shared pointer and are visible through all clones.

The existing `EnvFrame::cell()` method returns `(self.bindings.clone(), idx)`,
and `update()` calls `arr.set(i, closure)` on that clone. **Because Array clone
shares storage, updates through `cell()` DO modify the original frame's data.**

### Where Sharing Breaks

The problem is specifically in `env_from_data_args` (vm.rs:501):

```rust
let mut array = Array::with_capacity(&view, args.len());  // NEW backing storage
for r in args {
    let closure = match r {
        Ref::L(index) => (*local_env).get(&view, *index)  // READS from original
            .ok_or(...)?,
        // ...
    };
    array.push(&view, closure);  // COPIES into new storage
}
view.from_saturation(array, next, self.annotation)  // new frame with new array
```

`Array::with_capacity` allocates **fresh heap memory**. The closures are *read*
from the constructor's env and *pushed* into the new array. The new frame has
completely separate backing storage. The link is severed.

### The Update Path

Understanding the update path is critical for the design. When a thunk at
logical index `i` is entered:

1. VM reads `self.closure = env.get(i)` — gets the closure from the frame
2. If updatable, VM calls `env.update(i, black_hole)` — writes BlackHole
3. VM pushes `Update { environment: env_ptr, index: i }` — stores **logical** `i`
4. VM evaluates the thunk to a value
5. Update fires: calls `env.update(i, value_closure)`
6. `update()` internally calls `cell(i)` which returns `(arr_clone, physical_idx)`
7. `arr_clone.set(physical_idx, value)` writes to shared backing

**Key insight**: The Update continuation stores the **logical index**, not the
physical index. The logical→physical translation happens inside `cell()` every
time. This means we can add index remapping to `cell()` and the entire update
mechanism works automatically.

---

## Comprehensive Catalogue of Data Constructor Arg Patterns

All `data(...)` calls across `src/eval/stg/`:

### Category 1: Sequential from zero — `[L(0), ..., L(n-1)]`

| File | Line | Constructor | Args |
|------|------|-------------|------|
| list.rs | 44 | ListCons | `[lref(0), lref(1)]` |
| block.rs | 211 | ListCons | `[lref(0), lref(1)]` |
| block.rs | 390 | BlockPair | `[lref(0), lref(1)]` |
| embed.rs | 309 | (tag 0) | `[lref(0), lref(1)]` |
| wrap.rs | 87 | BoxedNumber | `[lref(0)]` |
| wrap.rs | 93 | BoxedString | `[lref(0)]` |
| wrap.rs | 99 | BoxedSymbol | `[lref(0)]` |
| wrap.rs | 105 | BoxedZdt | `[lref(0)]` |
| wrap.rs | 184 | BoxedNumber | `[lref(0)]` |
| array.rs | 203 | BoxedNumber | `[lref(0)]` |
| string.rs | 109,163,398 | BoxedString | `[lref(0)]` |

### Category 2: Reversed — `[L(1), L(0)]`

| File | Line | Constructor | Args | Context |
|------|------|-------------|------|---------|
| force.rs | 42 | ListCons | `[lref(1), lref(0)]` | `force_list_spine` — eagerly forces |
| force.rs | 82 | ListCons | `[lref(1), lref(0)]` | `force_list` — eagerly forces |
| block.rs | 71 | ListCons | `[lref(1), lref(0)]` | `to_kv_list` |
| block.rs | 1106 | ListCons | `[lref(1), lref(0)]` | `MERGE_WITH` |
| block.rs | 1210 | ListCons | `[lref(1), lref(0)]` | `KV_LIST` |

### Category 3: Offset sequential — `[L(base), L(base+1), ...]`, base > 0

| File | Line | Constructor | Args |
|------|------|-------------|------|
| block.rs | 168 | ListCons | `[lref(1), lref(2)]` |

### Category 4: Single non-zero — `[L(k)]`, k > 0

| File | Line | Constructor | Args |
|------|------|-------------|------|
| block.rs | 122 | BlockKvList | `[lref(2)]` |
| block.rs | 162 | BoxedSymbol | `[lref(3)]` (inside thunk) |

### Category 5: Mixed locals + values — `[L(k), V(_)]`

| File | Line | Constructor | Args |
|------|------|-------------|------|
| constant.rs | 59 | Block | `[lref(0), no_index()]` |
| block.rs | 88 | Block | `[lref(1), no_index()]` |

(`no_index()` = `Ref::V(Native::Num(0))` — a literal value, not a local ref)

### Category 6: Pure values — `[V(_)]`

| File | Line | Constructor | Args |
|------|------|-------------|------|
| compiler.rs | 1443 | BoxedNumber | `[num(99)]` |

No sharing concern — these are literal constants with no thunks.

---

## Unified Design: Index Map on EnvironmentFrame

### Core Idea

Add a small inline index map to `EnvironmentFrame` that translates logical
indices (as used by branch bodies and Update continuations) to physical indices
(positions in the shared backing array). For normal frames and identity-shared
frames, the map is unused and indices pass through directly.

### Data Structure Change

```rust
pub struct EnvironmentFrame<C: Clone> {
    /// Indexed bindings (may share backing storage with another frame)
    bindings: Array<C>,
    /// Logical-to-physical index remapping for shared-backing frames.
    /// When active (map_len > 0), logical index i maps to physical
    /// index remap[i]. When inactive (map_len == 0), logical == physical.
    remap: [u8; 4],
    /// Number of active remap entries. 0 = no remapping.
    remap_len: u8,
    /// Source code annotation
    annotation: Smid,
    /// Reference to next environment
    next: Option<RefPtr<EnvironmentFrame<C>>>,
}
```

**Size impact**: 5 bytes added (`[u8; 4]` + `u8`). With struct alignment, the
frame grows from 24 bytes to 32 bytes (one extra 8-byte word). This is
acceptable — frames are heap-allocated and the GC manages thousands of them.

**Maximum arg count**: 4 is sufficient. The maximum data constructor arity in
the codebase is 2 (ListCons, BlockPair, Block). The theoretical max from STG is
bounded by the `DataConstructor` enum. If a future constructor exceeds 4 args,
it falls back to the copy path.

### Modified `cell()`

```rust
fn cell(&self, guard: &dyn MutatorScope, idx: usize) -> Option<(Array<C>, usize)> {
    let logical_len = self.logical_len();
    if idx < logical_len {
        let physical = self.physical_index(idx);
        Some((self.bindings.clone(), physical))
    } else {
        match self.next {
            Some(ref env) => {
                (*ScopedPtr::from_non_null(guard, *env)).cell(guard, idx - logical_len)
            }
            None => None,
        }
    }
}

/// Logical length of this frame (for cactus-stack chaining)
#[inline]
fn logical_len(&self) -> usize {
    if self.remap_len > 0 {
        self.remap_len as usize
    } else {
        self.bindings.len()
    }
}

/// Translate logical index to physical index in backing array
#[inline]
fn physical_index(&self, logical: usize) -> usize {
    if self.remap_len > 0 {
        self.remap[logical] as usize
    } else {
        logical
    }
}
```

**Performance impact**: One branch (`remap_len > 0`) on every frame access.
For normal frames `remap_len == 0`, so the branch is highly predictable (always
false). On modern CPUs this is effectively zero cost.

### Modified constructors

```rust
impl<C: Clone> EnvironmentFrame<C> {
    pub fn new(bindings: Array<C>, annotation: Smid, next: Option<RefPtr<Self>>) -> Self {
        debug_assert!(next.is_none() || (next.unwrap() != RefPtr::dangling()));
        Self {
            bindings,
            remap: [0; 4],
            remap_len: 0,
            annotation,
            next,
        }
    }

    /// Construct a frame that shares backing storage with an existing Array,
    /// using an explicit index map for logical-to-physical translation.
    pub fn new_remapped(
        bindings: Array<C>,
        remap: &[u8],
        annotation: Smid,
        next: Option<RefPtr<Self>>,
    ) -> Self {
        debug_assert!(remap.len() <= 4);
        debug_assert!(next.is_none() || (next.unwrap() != RefPtr::dangling()));
        let mut map = [0u8; 4];
        map[..remap.len()].copy_from_slice(remap);
        Self {
            bindings,
            remap: map,
            remap_len: remap.len() as u8,
            annotation,
            next,
        }
    }
}
```

### Modified `Default`

```rust
impl<C: Clone> Default for EnvironmentFrame<C> {
    fn default() -> Self {
        Self {
            bindings: Default::default(),
            remap: [0; 4],
            remap_len: 0,
            annotation: Default::default(),
            next: Default::default(),
        }
    }
}
```

### GC: No Changes Needed

The `remap` and `remap_len` fields are plain `u8` values — not GC-managed
pointers. The existing `scan()` and `scan_and_update()` methods only need to
trace `bindings` (via `mark_array`) and `next` (via `mark`). The shared backing
Array's `RefPtr` keeps the allocation alive regardless of which frame the GC
reaches first. **No GC code changes required.**

### Array::clone_with_length

One helper on Array for the identity-shared case:

```rust
impl<T: Sized + Clone> Array<T> {
    /// Clone sharing backing storage, with a different logical length.
    /// The physical capacity is unchanged; only the logical bound moves.
    pub fn clone_with_length(&self, n: usize) -> Self {
        Array {
            length: n.min(self.length),
            data: self.data,  // Copy — shares backing storage
        }
    }
}
```

---

## Modified `env_from_data_args`

The core change — a single function that handles all categories:

```rust
fn env_from_data_args(
    &self,
    view: MutatorHeapView<'_>,
    args: &[Ref],
    next: RefPtr<EnvFrame>,
) -> Result<RefPtr<EnvFrame>, ExecutionError> {
    // Analyse the arg pattern
    match classify_args(args) {
        ArgPattern::SequentialFromZero(n) => {
            // Phase 1: Clone backing, identity mapping
            let constructor_env = view.scoped(self.closure.env());
            let shared = constructor_env.bindings.clone_with_length(n);
            Ok(view.alloc(EnvFrame::new(shared, self.annotation, Some(next)))?.as_ptr())
        }
        ArgPattern::Remapped { remap, len, max_physical } => {
            // Phases 2-3: Clone backing, use index map
            let constructor_env = view.scoped(self.closure.env());
            let shared = constructor_env.bindings.clone_with_length(max_physical + 1);
            Ok(view.alloc(
                EnvFrame::new_remapped(shared, &remap[..len], self.annotation, Some(next))
            )?.as_ptr())
        }
        ArgPattern::RequiresCopy => {
            // Phase 4 / fallback: copy into fresh array (current behaviour)
            self.env_from_data_args_copy(view, args, next)
        }
    }
}
```

With the classifier:

```rust
enum ArgPattern {
    /// All args are L(0), L(1), ..., L(n-1)
    SequentialFromZero(usize),
    /// All args are L(_) and fit in a 4-entry remap table
    Remapped {
        remap: [u8; 4],
        len: usize,
        max_physical: usize,
    },
    /// Contains V(_) or G(_) args, or > 4 args — must copy
    RequiresCopy,
}

fn classify_args(args: &[Ref]) -> ArgPattern {
    if args.len() > 4 {
        return ArgPattern::RequiresCopy;
    }

    // Check if all args are local refs
    let mut remap = [0u8; 4];
    let mut max_physical: usize = 0;
    let mut is_sequential_from_zero = true;

    for (i, r) in args.iter().enumerate() {
        match r {
            Ref::L(idx) => {
                remap[i] = *idx as u8;
                max_physical = max_physical.max(*idx);
                if *idx != i {
                    is_sequential_from_zero = false;
                }
            }
            _ => return ArgPattern::RequiresCopy,
        }
    }

    if is_sequential_from_zero {
        ArgPattern::SequentialFromZero(args.len())
    } else {
        ArgPattern::Remapped {
            remap,
            len: args.len(),
            max_physical,
        }
    }
}
```

---

## Phase-by-Phase Breakdown

### Phase 1: Sequential from Zero (Category 1)

**What**: `[L(0), ..., L(n-1)]` — identity mapping, no remap needed.

**Mechanism**: `Array::clone_with_length(n)` + `EnvFrame::new(...)`.
`remap_len = 0`, so `logical_len()` returns `bindings.len()` (= n), and
`physical_index(i)` returns `i`. Identical to current behaviour except the
backing array is shared.

**Impact**: Fixes sharing for CONS cells, BlockPair, all Boxed* wrappers — the
critical path for lazy list memoisation. This is where ~80% of the sharing
benefit lies.

**Implementation**: Add `clone_with_length` to Array, add `remap`/`remap_len`
fields to EnvFrame (initialised to zero in `new()`), modify `env_from_data_args`
with identity fast path.

### Phase 2: Offset Sequential (Categories 3, 4)

**What**: `[L(base), L(base+1), ...]` where base > 0. Also single non-zero
locals like `[L(2)]`.

**Mechanism**: Shares the constructor's backing array. The remap table maps
logical indices to physical indices: `remap = [base, base+1, ...]`.
`remap_len = n`. The backing Array needs `length >= base + n` (achieved by
`clone_with_length(max_physical + 1)`).

**Correctness detail**: `Array::get(physical)` checks `physical < length`.
With `clone_with_length(max_physical + 1)`, the length is `max_physical + 1`
which satisfies the bounds check. Similarly, `Array::set(physical, _)` in the
update path checks the same bounds. The physical indices are within the
original frame's allocation since they came from valid `Ref::L(idx)` values.

**Impact**: Fixes sharing for `block.rs:168` (ListCons `[lref(1), lref(2)]`),
`block.rs:122` (BlockKvList `[lref(2)]`), `block.rs:162` (BoxedSymbol
`[lref(3)]`). These are block construction helpers — moderate benefit.

**Implementation**: Add `new_remapped()` constructor to EnvFrame, add
`classify_args()` helper, extend `env_from_data_args` with the `Remapped` path.

### Phase 3: Arbitrary Permutations (Category 2)

**What**: `[L(1), L(0)]` — args reference locals in a different order than their
position in the branch frame.

**Mechanism**: Same as Phase 2 — the remap table handles arbitrary permutations.
`remap = [1, 0]`, `remap_len = 2`. Logical index 0 maps to physical index 1 and
vice versa. The backing array is shared.

**Correctness detail**: When the branch enters logical index 0, `cell(0)`
returns `(bindings.clone(), 1)`. The Update continuation stores
`(env_ptr, logical_index=0)`. When the update fires, it calls `cell(0)` again,
gets physical index 1, and writes to `bindings[1]`. The constructor's original
frame also has `bindings[1]` pointing to the same backing location — so the
update is visible through the constructor's frame. **Sharing is preserved.**

The same applies to the BlackHole write: `update(0, black_hole)` writes to
physical index 1 in the shared backing, preventing cyclic re-entry from any
frame that shares the storage.

**Analysis of call sites**: All 5 reversed-arg sites are in `force.rs` (which
eagerly evaluates, so sharing has no memoisation benefit) and `block.rs` (block
manipulation — bounded intermediate structures). The sharing fix is technically
correct but the practical benefit is limited compared to Phase 1.

**Impact**: Fixes sharing for block-manipulation cons cells. Moderate benefit
for workloads heavy on block operations (e.g., deeply nested merges).

**Implementation**: Already handled by the `Remapped` path from Phase 2 — no
additional code needed. The `classify_args()` function already produces the
correct remap for permuted args.

**Phases 2 and 3 are implemented together** — they use the same mechanism. The
only distinction is conceptual (sequential-with-offset vs. permuted), but the
remap table handles both uniformly.

### Phase 4: Mixed Locals + Values (Category 5)

**What**: `[L(k), V(_)]` — some args are local refs (need sharing), others are
literal values (need fresh closures).

**Why it's harder**: The `V(_)` args create new `SynClosure` wrappers around
`HeapSyn::Atom` nodes. These don't exist in the constructor's environment —
they're synthesised during destructuring. We cannot store them in the shared
backing array without overwriting whatever occupies that physical slot in the
constructor's frame.

**Concrete example**: `data(Block, [lref(0), no_index()])` in `constant.rs:59`.

The constructor's env frame was created by a lambda with (at least) 1 arg. The
backing array has the lambda's bindings. Position 0 holds the list (a thunk we
want to share). Position 1 may hold another lambda argument, or may be beyond
the frame's bounds.

If we share the backing and write the `no_index()` closure to position 1, we
corrupt whatever the constructor's frame had there. If position 1 is out of
bounds, the write fails with `ArrayBoundsError`.

**Design: Copy-on-write for value slots**

Share the backing array for the local-ref args, but allocate a *new* backing
array only when value args are present, copying the shared data and inserting
value closures at the appropriate positions.

```rust
ArgPattern::Mixed { locals, values, max_physical } => {
    let constructor_env = view.scoped(self.closure.env());
    let n = args.len();

    // Allocate new array sized for the branch frame
    let mut array = Array::with_capacity(&view, n);

    for (i, r) in args.iter().enumerate() {
        let closure = match r {
            Ref::L(idx) => {
                // Read from constructor env (still a copy, but see below)
                (*constructor_env).get(&view, *idx)
                    .ok_or(ExecutionError::BadEnvironmentIndex(*idx))?
            }
            Ref::V(_) => SynClosure::new(
                view.alloc(HeapSyn::Atom { evaluand: r.clone() })?.as_ptr(),
                self.closure.env(),
            ),
            Ref::G(idx) => {
                let global_env = view.scoped(self.globals);
                (*global_env).get(&view, *idx)
                    .ok_or(ExecutionError::BadGlobalIndex(*idx))?
            }
        };
        array.push(&view, closure);
    }

    view.from_saturation(array, next, self.annotation)
}
```

This is the **current behaviour** — no sharing for mixed args. The question is
whether we can do better.

**Alternative: Partial sharing via selective overwrite**

If the value arg's target position is known to be "dead" in the constructor's
frame (i.e., not referenced by any live closure), we could overwrite it safely.
But proving liveness at runtime is impractical.

**Alternative: Two-array frame**

Add an auxiliary array to the frame for value-arg slots:

```rust
pub struct EnvironmentFrame<C: Clone> {
    bindings: Array<C>,
    aux: Array<C>,         // auxiliary storage for value-arg slots
    remap: [u8; 4],
    remap_len: u8,
    remap_aux: [bool; 4],  // true = read from aux instead of bindings
    annotation: Smid,
    next: Option<RefPtr<Self>>,
}
```

`get(logical_i)` checks `remap_aux[i]`: if true, reads from `aux`; otherwise
reads from `bindings` at `remap[i]`. Updates to shared slots write through to
the shared backing; updates to aux slots are local.

**Cost**: Extra `Array<C>` (16 bytes) + `[bool; 4]` (4 bytes) on every frame.
For the vast majority of frames that don't use this, it's dead weight. The aux
array is default-empty (no allocation), so the heap cost is minimal, but the
struct size increase affects all frames.

**Evaluation**: Category 5 has exactly 2 call sites, both constructing `Block`
values (wrapping a list with a `no_index` sentinel). Block construction is not a
lazy-list path. The `no_index()` value arg is a constant — it's never a thunk,
never updated, never benefits from sharing. The only sharing loss is for the
`L(k)` arg (the block's content list), which is typically consumed once during
block evaluation.

**Recommendation**: Use the copy fallback for mixed args. The two-array
approach adds ~20 bytes to every frame for a benefit that applies to 2 call
sites, both in block construction. The engineering cost is disproportionate.

However, if mixed patterns become more common in future (e.g., new data
constructors with embedded constants), the two-array approach is a clean
extension point. The remap mechanism from Phases 1-3 is the foundation;
aux storage slots on top of the existing remap.

**If we want to eliminate the mixed pattern entirely**: Restructure the STG
code for `Block` construction to separate the list and sentinel into distinct
operations. For example, `Block` could carry only `[lref(0)]` (the list), with
the no-index sentinel handled by the case-match branch that inspects the block
index separately. This eliminates the mixed pattern at the source, making all
Block constructors fall into Category 1 or 3. This is a compiler-level change,
not a VM change.

---

## Correctness Properties (All Phases)

### Sharing Preservation

After the fix, for any data constructor whose args are all `Ref::L(_)`:
1. Branch frame's `bindings` shares backing storage with constructor env frame
2. `cell(logical_i)` returns `(bindings.clone(), physical_i)` — the clone
   shares the same backing
3. `Update { environment, index: logical_i }` stores the logical index
4. Update fires → `cell(logical_i)` → `(clone, physical_i)` →
   `clone.set(physical_i, value)` → writes to shared backing
5. All frames sharing the backing see the update

### BlackHole Propagation

When entering a thunk at logical index `i`:
1. VM calls `env.update(i, black_hole)` (vm.rs:260)
2. `update()` calls `cell(i)` → `(clone, physical_i)`
3. `clone.set(physical_i, black_hole)` writes to shared backing
4. Any other frame sharing the backing that reads `physical_i` sees the
   BlackHole, preventing cyclic re-entry

### GC Safety

- `remap`/`remap_len` are plain `u8` values — no GC tracing needed
- The shared Array's `RefPtr` keeps backing alive through any frame that
  references it — standard GC pointer tracing via `Array::allocated_data()`
- `scan()` iterates `bindings` (marking all closures in the backing) —
  correct regardless of whether the frame owns or shares the backing
- `scan_and_update()` updates closure pointers in the backing — writes through
  the shared pointer, visible to all sharing frames

### Frame Lifetime

Even if the constructor env frame is collected, the backing data persists as
long as any `Array` handle references it. The `Array` holds the `RefPtr`
directly (not through the frame), so frame collection doesn't free the backing.

### Cactus Stack Correctness

`logical_len()` returns `remap_len` (when > 0) or `bindings.len()`. For
remapped frames, `remap_len` is the number of args exposed by this frame —
correct for index chaining to `next`. Indices `>= remap_len` chain through as
before.

**Subtlety**: With remapping, `bindings.len()` may be larger than `remap_len`
(because the backing array includes slots beyond what this frame exposes). The
`logical_len()` method ensures the cactus stack uses the correct logical length,
not the physical backing length.

### Thread Safety

Not a concern — eucalypt is single-threaded.

### Update Suppression

vm.rs:557-563 suppresses updates when a case branch directly enters a local
ref. This optimisation is orthogonal — it prevents unnecessary Update
continuations for tail-recursive patterns. With shared backing, suppressing
an update is still correct (the value is already in the shared backing from
the original evaluation).

---

## Implementation Plan

All phases are implemented as a single PR. The remap mechanism is added in full
from the start — phasing is conceptual (for understanding), not a delivery
boundary.

| Phase | Categories | Mechanism | New code | Depends on |
|-------|-----------|-----------|----------|------------|
| 1 | 1, 6 | `clone_with_length` + identity map | ~35 lines | — |
| 2+3 | 2, 3, 4 | `new_remapped` + remap table | ~25 lines | Phase 1 |
| 4 | 5 | Copy fallback (current behaviour) | 0 lines | — |

---

### Step 1: Add `Array::clone_with_length`

**File**: `src/eval/memory/array.rs` (after line 175, in the `impl Array<T>` block)

```rust
/// Clone sharing backing storage, with a different logical length.
pub fn clone_with_length(&self, n: usize) -> Self {
    Array {
        length: n.min(self.length),
        data: self.data,
    }
}
```

**Test**: Unit test in same file confirming shared backing mutation visibility.

---

### Step 2: Add remap fields to `EnvironmentFrame`

**File**: `src/eval/machine/env.rs`

**2a. Struct definition** (line 181-191):

```rust
pub struct EnvironmentFrame<C>
where
    C: Clone,
{
    /// Indexed bindings (may share backing with another frame)
    bindings: Array<C>,
    /// Logical-to-physical index remap for shared-backing frames.
    /// When remap_len > 0, logical index i maps to physical remap[i].
    /// When remap_len == 0, logical index == physical index.
    remap: [u8; 4],
    /// Number of active remap entries. 0 = identity mapping.
    remap_len: u8,
    /// Source code annotation
    annotation: Smid,
    /// Reference to next environment
    next: Option<RefPtr<EnvironmentFrame<C>>>,
}
```

**2b. Default** (line 193-204): Add `remap: [0; 4], remap_len: 0`.

**2c. `new()`** (line 210-217): Add `remap: [0; 4], remap_len: 0`.

**2d. Add `new_remapped()` constructor**:

```rust
/// Construct a shared-backing frame with an explicit index remap.
pub fn new_remapped(
    bindings: Array<C>,
    remap: &[u8],
    annotation: Smid,
    next: Option<RefPtr<Self>>,
) -> Self {
    debug_assert!(remap.len() <= 4);
    debug_assert!(next.is_none() || (next.unwrap() != RefPtr::dangling()));
    let mut map = [0u8; 4];
    map[..remap.len()].copy_from_slice(remap);
    Self {
        bindings,
        remap: map,
        remap_len: remap.len() as u8,
        annotation,
        next,
    }
}
```

**2e. Add helper methods**:

```rust
/// Logical length of this frame for cactus-stack index chaining.
#[inline]
fn logical_len(&self) -> usize {
    if self.remap_len > 0 {
        self.remap_len as usize
    } else {
        self.bindings.len()
    }
}

/// Translate logical index to physical index in backing array.
#[inline]
fn physical_index(&self, logical: usize) -> usize {
    if self.remap_len > 0 {
        self.remap[logical] as usize
    } else {
        logical
    }
}
```

**2f. Modify `cell()`** (line 220-230):

Replace `self.bindings.len()` with `self.logical_len()` and `idx` with
`self.physical_index(idx)`:

```rust
fn cell(&self, guard: &dyn MutatorScope, idx: usize) -> Option<(Array<C>, usize)> {
    let len = self.logical_len();
    if idx < len {
        Some((self.bindings.clone(), self.physical_index(idx)))
    } else {
        match self.next {
            Some(ref env) => {
                (*ScopedPtr::from_non_null(guard, *env)).cell(guard, idx - len)
            }
            None => None,
        }
    }
}
```

Note: `get()` and `update()` are unchanged — they delegate to `cell()`.

**2g. Update `Display` and `Debug` impls** (lines 286-326):

Replace `self.bindings.len()` with `self.logical_len()` in the Display impl
(line 291) and Debug impl (line 318). Optionally annotate remapped frames with
`R` in debug output.

**2h. GC impls** (lines 363-395):

No changes needed. `scan()` iterates `bindings` which traces the shared backing.
`scan_and_update()` updates pointers in the backing. `remap`/`remap_len` are
plain bytes — no GC tracing required.

---

### Step 3: Add `classify_args` and modify `env_from_data_args`

**File**: `src/eval/machine/vm.rs`

**3a. Add `ArgPattern` enum and `classify_args` function** (above `env_from_data_args`,
around line 490):

```rust
/// Classification of data constructor arg patterns for shared-env optimisation.
enum ArgPattern {
    /// All args are L(0), L(1), ..., L(n-1) — identity mapping
    SequentialFromZero(usize),
    /// All args are L(_) with an arbitrary mapping that fits in 4 entries
    Remapped {
        remap: [u8; 4],
        len: usize,
        max_physical: usize,
    },
    /// Contains V(_) or G(_), or > 4 args — must copy
    RequiresCopy,
}

fn classify_args(args: &[Ref]) -> ArgPattern {
    if args.is_empty() {
        return ArgPattern::SequentialFromZero(0);
    }
    if args.len() > 4 {
        return ArgPattern::RequiresCopy;
    }

    let mut remap = [0u8; 4];
    let mut max_physical: usize = 0;
    let mut is_sequential = true;

    for (i, r) in args.iter().enumerate() {
        match r {
            Ref::L(idx) => {
                remap[i] = *idx as u8;
                max_physical = max_physical.max(*idx);
                if *idx != i {
                    is_sequential = false;
                }
            }
            _ => return ArgPattern::RequiresCopy,
        }
    }

    if is_sequential {
        ArgPattern::SequentialFromZero(args.len())
    } else {
        ArgPattern::Remapped {
            remap,
            len: args.len(),
            max_physical,
        }
    }
}
```

**3b. Rename current `env_from_data_args` to `env_from_data_args_copy`**
(line 492-522):

Keep the existing implementation as the fallback path. Change `fn env_from_data_args`
to `fn env_from_data_args_copy` with the same signature and body.

**3c. Add new `env_from_data_args`** that dispatches:

```rust
fn env_from_data_args(
    &self,
    view: MutatorHeapView<'_>,
    args: &[Ref],
    next: RefPtr<EnvFrame>,
) -> Result<RefPtr<EnvFrame>, ExecutionError> {
    match classify_args(args) {
        ArgPattern::SequentialFromZero(n) => {
            let constructor_env = view.scoped(self.closure.env());
            let shared = constructor_env.bindings.clone_with_length(n);
            Ok(view
                .alloc(EnvFrame::new(shared, self.annotation, Some(next)))?
                .as_ptr())
        }
        ArgPattern::Remapped {
            remap,
            len,
            max_physical,
        } => {
            let constructor_env = view.scoped(self.closure.env());
            let shared = constructor_env.bindings.clone_with_length(max_physical + 1);
            Ok(view
                .alloc(EnvFrame::new_remapped(
                    shared,
                    &remap[..len],
                    self.annotation,
                    Some(next),
                ))?
                .as_ptr())
        }
        ArgPattern::RequiresCopy => self.env_from_data_args_copy(view, args, next),
    }
}
```

**Note**: `constructor_env.bindings` is a private field. We need to either:
- Add a `pub fn bindings(&self) -> &Array<C>` accessor to `EnvironmentFrame`, or
- Add a `pub fn shared_bindings(&self, n: usize) -> Array<C>` method that
  returns `self.bindings.clone_with_length(n)`, or
- Make the field `pub(crate)`

Recommended: Add a method to avoid exposing the field directly:

```rust
/// Return a shared view of the bindings array with logical length n.
/// The returned Array shares backing storage with this frame.
pub fn shared_bindings(&self, n: usize) -> Array<C> {
    self.bindings.clone_with_length(n)
}
```

Then the VM code becomes:
```rust
let shared = constructor_env.shared_bindings(n);       // Phase 1
let shared = constructor_env.shared_bindings(max + 1); // Phase 2+3
```

---

### Step 4: Harness Tests

**File**: `harness/test/089_sharing.eu` (new file)

Test that thunk memoisation works across multiple traversals of the same
cons-cell structure. The test should:

1. Build a lazy list where each element has a visible side effect or is
   expensive to compute
2. Traverse the list twice (e.g., `sum` then `length`, or two `map` passes)
3. Verify both traversals produce correct results
4. Verify the total evaluation count matches single-traversal expectation

Since eucalypt doesn't have mutable state for counting evaluations, the test
approach should focus on correctness (both traversals yield the same result)
and performance (the test completes in bounded time — a sharing bug would cause
exponential blowup on recursive structures).

Concrete test ideas:
- `iterate(f, x)` producing an infinite list, `take(n, ...)` twice — with
  sharing, the second take reuses forced thunks
- Deeply nested `map` over a shared list — linear with sharing, quadratic
  without
- A list where each element depends on the previous (fibonacci-style) —
  sharing means O(n), no sharing means O(2^n)

**File**: `tests/harness_test.rs` — add `test_harness_089`.

---

### Step 5: Verification

1. `cargo fmt --all`
2. `cargo clippy --all-targets -- -D warnings`
3. `cargo test` — all existing tests + new test 089
4. Manual smoke test: `echo 'fib(n): __IF(n < 2, n, fib(n - 1) + fib(n - 2))  x: take(20, iterate((+ 1), 0) map(fib)) sum' | cargo run -- -` — should complete quickly with sharing

---

### Step 6: Display and Debug polish

Update the `Display` and `Debug` impls for `EnvironmentFrame` to show remap
info for debugging:

```rust
// In Display
if self.remap_len > 0 {
    write!(f, "[×{}R]→...", self.remap_len)
} else {
    write!(f, "[×{}]→...", self.bindings.len())
}
```

---

## File Change Summary

| File | Changes |
|------|---------|
| `src/eval/memory/array.rs` | Add `clone_with_length` method (~6 lines) |
| `src/eval/machine/env.rs` | Add `remap`/`remap_len` fields, `new_remapped()`, `logical_len()`, `physical_index()`, `shared_bindings()`, modify `cell()`, update `Default`/`new`/`Display`/`Debug` (~50 lines changed) |
| `src/eval/machine/vm.rs` | Add `ArgPattern`, `classify_args()`, new `env_from_data_args`, rename old to `_copy` (~60 lines) |
| `harness/test/089_sharing.eu` | New test file (~20-30 lines) |
| `tests/harness_test.rs` | Add `test_harness_089` (1 line) |

**Total**: ~140 lines new/changed across 5 files.

---

## Bead Structure

- **Parent bead**: eu-xxrx (lazy streams) — this fix is a prerequisite
- **Implementation bead**: Create a single bead for the full shared-env fix
  (phases 1-3 delivered together)
- **Branch**: `feat/furnace-shared-env` targeting `0.4.0`
- **Agent**: Furnace (backend/VM work)
- **PR**: Single PR with all phases, reviewed by Wicket

---

## Relation to Lazy Streams (eu-xxrx)

This fix is a **prerequisite** for lazy streams. Without it, `StreamNext`
producing lazy cons cells has broken memoisation — every traversal re-evaluates
every element.

Once merged, the path to lazy streams opens: `StreamNext` can produce lazy cons
cells, and thunk sharing through those cells will be preserved across multiple
traversals. The remaining stream work (eu-uihm, eu-qg0j) can proceed.
