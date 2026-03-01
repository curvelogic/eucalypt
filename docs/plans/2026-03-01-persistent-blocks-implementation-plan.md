# Persistent Blocks Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Replace cons-list block representation with `im::OrdMap` for
O(log n) lookup, structural sharing on merge, and no index
invalidation after merge.

**Architecture:** Phased migration — introduce `HeapBlock` alongside
the existing cons-list representation, add new intrinsics, switch the
compiler, then remove the old representation. Both representations
coexist during the transition. All work on a feature branch with
benchmarking before merge.

**Tech Stack:** Rust, `im` crate (with `rc` feature), existing STG
machine and GC infrastructure.

**Prerequisites:** eu-hmji (persistent sets) must land first to
introduce the `im` crate dependency.

---

## CRITICAL: Feature Branch Requirement

**All work MUST happen on a feature branch** (`persistent-blocks` or
similar). Before merging to master, run comparative benchmarks against
the current implementation. If performance regresses unacceptably,
the branch is not merged.

Create the branch before starting Task 1:

```
git checkout -b feat/persistent-blocks
```

---

## Background

Read the design doc at
`docs/plans/2026-03-01-persistent-blocks-design.md` before starting.

The current block representation is a cons-list of `BlockPair` cells
with a lazily-cached `HashMap` index. Merged blocks lose their index
and regress to O(n) lookup. This plan replaces the cons-list with
`im::OrdMap<SymbolId, BlockEntry>` stored in a `HeapBlock` heap
object.

Key files:
- `src/eval/stg/block.rs` — block intrinsics (BLOCK, LOOKUPOR, MERGE, etc.)
- `src/eval/stg/compiler.rs:1049-1064` — `compile_block()`
- `src/eval/stg/tags.rs` — DataConstructor::Block (8), BlockPair (9), BlockKvList (10)
- `src/eval/stg/syntax.rs:408-423` — `dsl::pair()`, `dsl::block()`
- `src/eval/memory/syntax.rs:35-48` — `Native` enum
- `src/eval/memory/syntax.rs:274-308` — GC mark/update functions
- `src/eval/memory/set.rs` — reference pattern for heap-allocated types
- `src/eval/stg/render.rs:248-276` — `RenderBlockItems` (iterates block list)
- `src/eval/stg/mod.rs:74-88` — intrinsic registration

---

## Phase 1: Infrastructure

### Task 1: Add `im` Crate Dependency

**Files:**
- Modify: `Cargo.toml`

**Step 1: Add the dependency**

In `Cargo.toml` under `[dependencies]`, add:

```toml
im = { version = "15", features = ["rc"] }
```

The `rc` feature uses `Rc` instead of `Arc` for internal sharing,
matching eucalypt's single-threaded execution model.

**Step 2: Verify it compiles**

Run: `cargo build`

**Step 3: Commit**

```
git commit -m "feat: add im crate dependency for persistent data structures"
```

**Note:** If eu-hmji has already landed, the `im` dependency already
exists. In that case, just verify the `rc` feature is enabled and
skip this task.

---

### Task 2: Create HeapBlock and BlockEntry Types

**Files:**
- Create: `src/eval/memory/block.rs`
- Modify: `src/eval/memory/mod.rs`

**Step 1: Create the module**

Create `src/eval/memory/block.rs`:

```rust
use std::cell::Cell;
use std::fmt;

use im::OrdMap;

use crate::common::sourcemap::Smid;
use crate::eval::memory::syntax::Ref;
use crate::eval::stg::tags::DataConstructor;

use super::alloc::StgObject;
use super::string::SymbolId;

/// A single entry in a persistent block.
#[derive(Clone)]
pub struct BlockEntry {
    /// Insertion order for rendering (declaration order).
    pub order: usize,
    /// Value thunk pointer (into GC heap). Uses Cell for
    /// interior mutability so GC can update forwarding pointers
    /// through structurally-shared im::OrdMap nodes.
    pub value: Cell<Ref>,
}

impl BlockEntry {
    pub fn new(order: usize, value: Ref) -> Self {
        Self {
            order,
            value: Cell::new(value),
        }
    }
}

/// A persistent block storing key-value pairs with O(log n) lookup
/// and structural sharing on merge.
pub struct HeapBlock {
    /// Persistent ordered map: SymbolId → BlockEntry.
    /// Sorted by SymbolId for O(log n) lookup. Declaration order
    /// is recovered via BlockEntry::order at render time.
    pub entries: OrdMap<SymbolId, BlockEntry>,
    /// Next insertion order index (monotonically increasing).
    pub next_order: usize,
}

impl HeapBlock {
    /// Create an empty block.
    pub fn empty() -> Self {
        Self {
            entries: OrdMap::new(),
            next_order: 0,
        }
    }

    /// Create a block from an iterator of (key, value) pairs.
    /// Insertion order follows iterator order.
    pub fn from_pairs(pairs: impl Iterator<Item = (SymbolId, Ref)>) -> Self {
        let mut entries = OrdMap::new();
        let mut next_order = 0;
        for (key, value) in pairs {
            entries.insert(key, BlockEntry::new(next_order, value));
            next_order += 1;
        }
        Self {
            entries,
            next_order,
        }
    }

    /// Look up a value by symbol key. O(log n).
    pub fn lookup(&self, key: &SymbolId) -> Option<Ref> {
        self.entries.get(key).map(|e| e.value.get())
    }

    /// Number of entries.
    pub fn len(&self) -> usize {
        self.entries.len()
    }

    /// Whether the block is empty.
    pub fn is_empty(&self) -> bool {
        self.entries.is_empty()
    }

    /// Iterate entries in declaration (insertion) order.
    /// Returns (SymbolId, Ref) pairs sorted by insertion order.
    pub fn ordered_entries(&self) -> Vec<(SymbolId, Ref)> {
        let mut entries: Vec<_> = self
            .entries
            .iter()
            .map(|(k, e)| (e.order, *k, e.value.get()))
            .collect();
        entries.sort_by_key(|(order, _, _)| *order);
        entries
            .into_iter()
            .map(|(_, k, v)| (k, v))
            .collect()
    }

    /// Shallow merge: right overrides left for duplicate keys.
    /// New right-side keys are appended after all left-side keys.
    /// Overridden keys keep their left-side insertion position.
    pub fn merge(&self, right: &HeapBlock) -> HeapBlock {
        let mut result = self.entries.clone();
        let mut next_order = self.next_order;
        for (key, entry) in right.entries.iter() {
            let order = if let Some(existing) = result.get(key) {
                existing.order
            } else {
                let o = next_order;
                next_order += 1;
                o
            };
            result.insert(*key, BlockEntry::new(order, entry.value.get()));
        }
        HeapBlock {
            entries: result,
            next_order,
        }
    }

    /// Check whether the block contains a given key.
    pub fn contains_key(&self, key: &SymbolId) -> bool {
        self.entries.contains_key(key)
    }
}

impl StgObject for HeapBlock {
    fn as_any(&self) -> &dyn std::any::Any {
        self
    }
}

impl Clone for HeapBlock {
    fn clone(&self) -> Self {
        Self {
            entries: self.entries.clone(),
            next_order: self.next_order,
        }
    }
}

impl PartialEq for HeapBlock {
    fn eq(&self, other: &Self) -> bool {
        // Structural equality: same keys, same insertion orders
        // (value equality requires forcing thunks — not done here)
        self.entries.len() == other.entries.len()
            && self
                .entries
                .keys()
                .zip(other.entries.keys())
                .all(|(a, b)| a == b)
    }
}

impl Eq for HeapBlock {}

impl fmt::Debug for HeapBlock {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "HeapBlock({} entries)", self.entries.len())
    }
}
```

**Step 2: Add the module declaration**

In `src/eval/memory/mod.rs`, add:

```rust
pub mod block;
```

**Step 3: Verify it compiles**

Run: `cargo build`

**Step 4: Commit**

```
git commit -m "feat: add HeapBlock and BlockEntry types for persistent blocks"
```

---

### Task 3: Add Native::Block Variant and GC Support

**Files:**
- Modify: `src/eval/memory/syntax.rs`

**Step 1: Add the Native variant**

In the `Native` enum (line 35), add a new variant after `Set`:

```rust
pub enum Native {
    Sym(SymbolId),
    Str(RefPtr<HeapString>),
    Num(Number),
    Zdt(DateTime<FixedOffset>),
    Index(Rc<BlockIndex>),
    Set(RefPtr<HeapSet>),
    Block(RefPtr<HeapBlock>),  // NEW
}
```

Add the import at the top of the file:

```rust
use super::block::HeapBlock;
```

**Step 2: Update GC mark function**

In `mark_ref_heap_pointers()` (line 274), add handling for the new
variant. `HeapBlock` contains `Cell<Ref>` values that may be heap
pointers — these must be traced:

```rust
Native::Block(ptr) => {
    heap_view.mark(*ptr);
    // Also trace Ref values inside the block's entries
    let block = unsafe { ptr.as_ref() };
    for (_, entry) in block.entries.iter() {
        mark_ref_heap_pointers(heap_view, &entry.value.get());
    }
}
```

**Step 3: Update GC update function**

In `update_ref_heap_pointers()` (line 294), add handling. The
`Cell<Ref>` interior mutability allows in-place updates through
structurally-shared OrdMap nodes:

```rust
Native::Block(ptr) => {
    if let Some(new_ptr) = mapping.get(ptr) {
        *current = Ref::V(Native::Block(*new_ptr));
    }
    // Update Ref values inside the block's entries
    let block = unsafe { ptr.as_ref() };
    for (_, entry) in block.entries.iter() {
        let r = entry.value.get();
        let mut r_mut = r;
        update_ref_heap_pointers(heap_view, &mut r_mut, mapping);
        if r != r_mut {
            entry.value.set(r_mut);
        }
    }
}
```

**Step 4: Update Clone, PartialEq, Debug for Native**

Ensure the `Native` enum's derived or manual trait impls handle the
new `Block` variant. Follow the same pattern as `Set`:

- `Clone`: `Block(ptr) => Block(*ptr)` (copy the RefPtr)
- `PartialEq`: `(Block(a), Block(b)) => a == b` (pointer equality)
- `Debug`: `Block(ptr) => write!(f, "Block({:?})", ptr)`

**Step 5: Verify it compiles**

Run: `cargo build`

**Step 6: Run tests to check for regressions**

Run: `cargo test --lib`

**Step 7: Commit**

```
git commit -m "feat: add Native::Block variant with GC mark/update support"
```

---

## Phase 2: New Intrinsics

### Task 4: PBLOCK_FROM_PAIRS Construction Intrinsic

**Files:**
- Modify: `src/eval/stg/block.rs`
- Modify: `src/eval/stg/mod.rs`
- Modify: `src/eval/stg/support.rs`

**Step 1: Add HeapBlock allocation helper**

In `src/eval/stg/support.rs`, add a helper to allocate and return a
HeapBlock (follow the `machine_return_set` pattern):

```rust
pub fn machine_return_block(
    machine: &dyn IntrinsicMachine,
    view: MutatorHeapView<'_>,
    block: HeapBlock,
) -> Result<(), ExecutionError> {
    let ptr = view.alloc_blob(block)?;
    let block_ref = Ref::V(Native::Block(ptr));
    let data = view.data(
        DataConstructor::Block.tag(),
        Array::from_slice(&view, &[block_ref]),
    )?;
    machine.set_closure(SynClosure::new(data.as_ptr(), machine.env(view)));
    Ok(())
}
```

Note: This creates a `DataConstructor::Block` with a single arg
(the `Native::Block` pointer) instead of the old two-arg form
`[list, index]`. The case-analysis tag is the same (8), but the arg
structure differs. During the transition, code must detect which form
it's dealing with.

**Step 2: Add the PBLOCK_FROM_PAIRS intrinsic**

In `src/eval/stg/block.rs`, add:

```rust
/// Construct a HeapBlock from a list of BlockPair cons cells.
/// Takes a single argument: the cons-list of pairs.
pub struct PBlockFromPairs;

impl StgIntrinsic for PBlockFromPairs {
    fn name(&self) -> &str {
        "PBLOCK_FROM_PAIRS"
    }

    fn execute(
        &self,
        machine: &dyn IntrinsicMachine,
        view: MutatorHeapView<'_>,
        _emitter: &mut dyn Emitter,
        args: &[Ref],
    ) -> Result<(), ExecutionError> {
        let list = data_list_arg(machine, view, args[0].clone())?;
        let mut entries = Vec::new();
        for item in list {
            let code = view.scoped(item.code());
            match &*code {
                HeapSyn::Cons { tag, args: pair_args }
                    if *tag == DataConstructor::BlockPair.tag() =>
                {
                    let key_ref = pair_args.get(0).unwrap();
                    if let Ref::V(Native::Sym(sym_id)) = key_ref {
                        let value_ref = pair_args.get(1).unwrap();
                        let value_closure = item.navigate_local(&view, value_ref);
                        // Store the value's code pointer as the Ref
                        entries.push((*sym_id, value_ref.clone()));
                    }
                }
                _ => {}
            }
        }
        let block = HeapBlock::from_pairs(entries.into_iter());
        machine_return_block(machine, view, block)
    }
}
```

**Note:** The exact implementation of value storage will need
refinement during implementation. The key challenge is converting
from `SynClosure` (environment + code pointer) to a `Ref` that the
HeapBlock can store. Examine how the existing `MERGE` intrinsic's
`deconstruct()` function (line 988) extracts values from BlockPairs,
and how `machine_return_closure_list()` packages them back. The
persistent block needs to store enough information to reconstruct the
closure when looked up.

**Step 3: Register the intrinsic**

In `src/eval/stg/mod.rs`, in `make_standard_runtime()`, add:

```rust
rt.add(Box::new(block::PBlockFromPairs));
```

**Step 4: Verify it compiles**

Run: `cargo build`

**Step 5: Commit**

```
git commit -m "feat: add PBLOCK_FROM_PAIRS construction intrinsic"
```

---

### Task 5: PBLOCK_LOOKUP Intrinsic

**Files:**
- Modify: `src/eval/stg/block.rs`
- Modify: `src/eval/stg/mod.rs`

**Step 1: Add the lookup intrinsic**

```rust
/// O(log n) lookup in a HeapBlock. Returns the value if found,
/// or a default if not.
pub struct PBlockLookup;

impl StgIntrinsic for PBlockLookup {
    fn name(&self) -> &str {
        "PBLOCK_LOOKUP"
    }

    fn execute(
        &self,
        machine: &dyn IntrinsicMachine,
        view: MutatorHeapView<'_>,
        _emitter: &mut dyn Emitter,
        args: &[Ref],
    ) -> Result<(), ExecutionError> {
        // args: [key_symbol, default, block_ref]
        let key = match &args[0] {
            Ref::V(Native::Sym(sym_id)) => *sym_id,
            _ => return Err(ExecutionError::TypeMismatch(
                Smid::default(),
                IntrinsicType::Symbol,
                IntrinsicType::from(&args[0]),
            )),
        };

        let block_ptr = match &args[2] {
            Ref::V(Native::Block(ptr)) => ptr,
            _ => return Err(ExecutionError::TypeMismatch(
                Smid::default(),
                IntrinsicType::Block,
                IntrinsicType::from(&args[2]),
            )),
        };

        let block = unsafe { block_ptr.as_ref() };
        match block.lookup(&key) {
            Some(value) => {
                machine.set_closure(SynClosure::new(/* reconstruct from value */));
            }
            None => {
                // Return default (args[1])
                machine.set_closure(SynClosure::new(/* default closure */));
            }
        }
        Ok(())
    }
}
```

**Note:** The exact closure reconstruction from stored `Ref` values
will need refinement. Study how the existing `LOOKUPOR` (line 579)
returns values after index lookup — it calls `walk_list_to_position`
and `extract_value_from_pair`. With HeapBlock, we skip the position
walk and return the stored value directly.

**Step 2: Register**

In `make_standard_runtime()`:

```rust
rt.add(Box::new(block::PBlockLookup));
```

**Step 3: Commit**

```
git commit -m "feat: add PBLOCK_LOOKUP intrinsic for O(log n) block lookup"
```

---

### Task 6: PBLOCK_MERGE and PBLOCK_MERGEWITH Intrinsics

**Files:**
- Modify: `src/eval/stg/block.rs`
- Modify: `src/eval/stg/mod.rs`

**Step 1: Add PBLOCK_MERGE**

Merge two HeapBlocks with structural sharing. Right overrides left:

```rust
pub struct PBlockMerge;

impl StgIntrinsic for PBlockMerge {
    fn name(&self) -> &str {
        "PBLOCK_MERGE"
    }

    fn execute(
        &self,
        machine: &dyn IntrinsicMachine,
        view: MutatorHeapView<'_>,
        _emitter: &mut dyn Emitter,
        args: &[Ref],
    ) -> Result<(), ExecutionError> {
        let left = block_arg(machine, view, &args[0])?;
        let right = block_arg(machine, view, &args[1])?;
        let merged = left.merge(right);
        machine_return_block(machine, view, merged)
    }
}
```

Add a `block_arg()` helper in `support.rs` (following the `set_arg`
pattern at line 388):

```rust
pub fn block_arg<'a>(
    machine: &dyn IntrinsicMachine,
    view: MutatorHeapView<'_>,
    r: &Ref,
) -> Result<&'a HeapBlock, ExecutionError> {
    match r {
        Ref::V(Native::Block(ptr)) => Ok(unsafe { ptr.as_ref() }),
        _ => Err(ExecutionError::TypeMismatch(
            Smid::default(),
            IntrinsicType::Block,
            IntrinsicType::from(r),
        )),
    }
}
```

**Step 2: Add PBLOCK_MERGEWITH**

Merge with a combine function for duplicate keys. This is more
complex — for overlapping keys, the combine function must be applied
at runtime:

```rust
pub struct PBlockMergeWith;
```

Model this on the existing `MergeWith` (line 1120). The key
difference: instead of traversing cons-lists and rebuilding via
`IndexMap`, iterate the OrdMaps directly. For duplicate keys, apply
the combine function (third argument) and store the result.

**Step 3: Register both**

```rust
rt.add(Box::new(block::PBlockMerge));
rt.add(Box::new(block::PBlockMergeWith));
```

**Step 4: Commit**

```
git commit -m "feat: add PBLOCK_MERGE and PBLOCK_MERGEWITH intrinsics"
```

---

### Task 7: PBLOCK_TO_LIST Conversion Intrinsic

**Files:**
- Modify: `src/eval/stg/block.rs`
- Modify: `src/eval/stg/mod.rs`

During the transition, the rendering pipeline
(`RenderBlockItems` in `render.rs:248`) expects blocks to contain a
cons-list of BlockPair cells. Rather than rewriting the entire render
path immediately, add a conversion intrinsic:

**Step 1: Add PBLOCK_TO_LIST**

```rust
/// Convert a HeapBlock to a cons-list of BlockPair cells in
/// insertion (declaration) order. Used by the rendering pipeline
/// during the transition period.
pub struct PBlockToList;
```

This intrinsic:
1. Calls `block.ordered_entries()` to get entries in insertion order
2. For each `(SymbolId, Ref)`, creates a `BlockPair` cons cell
3. Chains them into a `ListCons` list
4. Returns the list

This allows existing `RenderBlockItems`, `Elements`, `Keys`, `Values`
to work unchanged with HeapBlock.

**Step 2: Register**

```rust
rt.add(Box::new(block::PBlockToList));
```

**Step 3: Commit**

```
git commit -m "feat: add PBLOCK_TO_LIST for rendering compatibility"
```

---

### Task 8: Write Harness Tests for Persistent Blocks

**Files:**
- Create: `harness/test/095_persistent_block.eu`
- Modify: `tests/harness_test.rs`

**Step 1: Create the test file**

```eucalypt
# Test persistent block operations
# These exercise the same block features but ensure the persistent
# representation produces identical results.

tests: {
  ` "Basic block construction and lookup"
  basic: {
    b: {x: 1 y: 2 z: 3}
    α: b.x //= 1
    β: b.y //= 2
    γ: b.z //= 3
  }

  ` "Block merge — right overrides left"
  merge: {
    l: {x: 1 y: 2}
    r: {y: 3 z: 4}
    m: l r
    α: m.x //= 1
    β: m.y //= 3
    γ: m.z //= 4
  }

  ` "Deep merge"
  deep-merge: {
    l: {a: {x: 1 y: 2}}
    r: {a: {y: 3 z: 4}}
    m: l << r
    α: m.a.x //= 1
    β: m.a.y //= 3
    γ: m.a.z //= 4
  }

  ` "Block keys preserve declaration order"
  order: {
    b: {z: 3 a: 1 m: 2}
    α: b keys //= [:z, :a, :m]
  }

  ` "Merge preserves left-side key order"
  merge-order: {
    l: {b: 2 a: 1}
    r: {c: 3 a: 10}
    m: l r
    α: m keys //= [:b, :a, :c]
  }

  ` "Block as function (merge application)"
  block-fn: {
    base: {x: 1 y: 2}
    override: {y: 3}
    result: base override
    α: result.y //= 3
  }

  ` "Large block lookup efficiency"
  large: {
    b: {
      k00: 0  k01: 1  k02: 2  k03: 3  k04: 4
      k05: 5  k06: 6  k07: 7  k08: 8  k09: 9
      k10: 10 k11: 11 k12: 12 k13: 13 k14: 14
      k15: 15 k16: 16 k17: 17 k18: 18 k19: 19
    }
    α: b.k00 //= 0
    β: b.k19 //= 19
    γ: b.k10 //= 10
  }

  ` "Has key check"
  has-key: {
    b: {x: 1 y: 2}
    α: b has(:x) //= true
    β: b has(:z) //= false
  }

  ` "Elements iteration"
  elements: {
    b: {x: 1 y: 2}
    α: b elements map(value) //= [1, 2]
  }
}

RESULT: tests values map(values) map(all-true?) all-true? then(:PASS, :FAIL)
```

**Step 2: Register the test**

In `tests/harness_test.rs`, add:

```rust
harness_test!(test_harness_095, "095_persistent_block");
```

**Step 3: Run the test**

Run: `cargo test test_harness_095`

This should pass with the current (cons-list) implementation,
confirming the test exercises standard block behaviour. It will
continue to pass after switching to persistent blocks.

**Step 4: Commit**

```
git commit -m "test: harness test for persistent block operations"
```

---

## Phase 3: Switch Compiler

### Task 9: Update Block Data Constructor Args

**Files:**
- Modify: `src/eval/stg/tags.rs`
- Modify: `src/eval/stg/block.rs`
- Modify: `src/eval/stg/render.rs`
- Modify: `src/eval/machine/vm.rs`

This is the critical switchover. Change the `DataConstructor::Block`
representation from `[list_ref, index_ref]` (two args) to
`[block_ref]` (single arg containing `Native::Block`).

**Step 1: Understand what cases on Block tag**

Search the codebase for code that pattern-matches on
`DataConstructor::Block.tag()`. Key locations:

- `block.rs` — LOOKUPOR wrapper, MERGE wrapper, DEEPMERGE wrapper
- `render.rs:143` — RENDER cases on Block to extract items list
- `vm.rs:539` — Block application delegates to MERGE
- `compiler.rs` — `compile_lookup` emits code that eventually
  evaluates blocks

Each of these currently expects `args[0]` = items list and
`args[1]` = index. They must be updated to expect `args[0]` =
`Native::Block(ptr)`.

**Step 2: Update LOOKUPOR to handle HeapBlock**

In the `LOOKUPOR` execute method (line 579), detect whether the block
arg is old-style (cons-list) or new-style (HeapBlock):

```rust
match &block_args[0] {
    Ref::V(Native::Block(ptr)) => {
        // New persistent block — O(log n) lookup
        let block = unsafe { ptr.as_ref() };
        match block.lookup(&key_sym) {
            Some(value) => { /* return value */ }
            None => { /* return default */ }
        }
    }
    _ => {
        // Old cons-list block — existing logic
        // (kept during transition)
    }
}
```

**Step 3: Update MERGE to handle HeapBlock**

Similarly, `Merge::execute()` (line 1086) should detect HeapBlock
args and use `HeapBlock::merge()` directly.

**Step 4: Update RENDER to handle HeapBlock**

In `render.rs:143`, the Block case currently extracts `args[0]` as a
list and passes to `RenderBlockItems`. For HeapBlock, call
`PBLOCK_TO_LIST` first to convert to the expected list form, or
rewrite `RenderBlockItems` to iterate HeapBlock directly.

**Step 5: Update compile_block()**

In `src/eval/stg/compiler.rs:1049-1064`, change `compile_block()` to
emit a call to `PBLOCK_FROM_PAIRS` instead of `dsl::block()`:

```rust
pub fn compile_block(
    &self,
    binder: &mut LetBinder,
    smid: Smid,
    block_map: &BlockMap<RcExpr>,
) -> Result<Holder, CompileError> {
    binder.ensure_recursive();
    // Build pair list as before (temporary, consumed by intrinsic)
    let mut index = KEmptyList.gref();
    for (k, v) in block_map.iter().rev() {
        let v_index = self.compile_binding(binder, v.clone(), smid, false)?;
        let kv_index = binder.add(dsl::pair(k, v_index))?;
        index = binder.add(dsl::cons(kv_index, index))?;
    }
    // Call PBLOCK_FROM_PAIRS instead of dsl::block()
    Ok(Holder::new(Rc::new(StgSyn::App {
        callable: Ref::G(intrinsics::index("PBLOCK_FROM_PAIRS")
            .expect("PBLOCK_FROM_PAIRS must be registered")),
        args: Array::from_slice(&[], &[index]),
    })))
}
```

**Step 6: Run full test suite**

Run: `cargo test`

Fix any failures — this is the most likely task to surface issues
since it changes the core representation.

**Step 7: Commit**

```
git commit -m "feat: switch compiler and intrinsics to persistent HeapBlock"
```

---

### Task 10: Update ELEMENTS, KEYS, VALUES for HeapBlock

**Files:**
- Modify: `src/eval/stg/block.rs`

**Step 1: Update the intrinsics**

The `Elements`, `Keys`, `Values` intrinsics currently iterate the
cons-list. Update them to detect HeapBlock and use
`ordered_entries()`:

For `Elements`: convert `HeapBlock::ordered_entries()` to a cons-list
of BlockPairs.

For `Keys`: extract just the SymbolIds in insertion order.

For `Values`: extract just the value Refs in insertion order.

**Step 2: Run tests**

Run: `cargo test`

**Step 3: Commit**

```
git commit -m "feat: update Elements/Keys/Values for persistent blocks"
```

---

### Task 11: Benchmark Comparison

**Files:**
- Possibly modify: `benches/` (add block-specific benchmarks if needed)

**Step 1: Run existing benchmarks on master**

```
git stash
git checkout master
cargo bench -- --save-baseline master
git checkout feat/persistent-blocks
git stash pop
```

**Step 2: Run benchmarks on the feature branch**

```
cargo bench -- --save-baseline persistent-blocks
```

**Step 3: Compare results**

```
cargo bench -- --baseline master
```

**Step 4: Add block-specific benchmarks if needed**

If existing benchmarks don't adequately exercise blocks, add targeted
benchmarks for:
- Small block lookup (5 keys)
- Large block lookup (100 keys)
- Post-merge block lookup
- Repeated merge chains
- Block construction

**Step 5: Document results**

Record benchmark results in a comment or file. If performance is
acceptable, proceed to Phase 4. If not, investigate and optimise, or
fall back to the simpler "eager index on merge" fix.

**Step 6: Commit**

```
git commit -m "bench: block benchmark comparison for persistent blocks"
```

---

## Phase 4: Remove Old Representation

### Task 12: Remove Cons-List Block Code

Only proceed if benchmarks are acceptable.

**Files:**
- Modify: `src/eval/stg/block.rs`
- Modify: `src/eval/stg/tags.rs`
- Modify: `src/eval/stg/syntax.rs`
- Modify: `src/eval/memory/syntax.rs`

**Step 1: Remove deprecated data constructors**

In `src/eval/stg/tags.rs`, mark `BlockPair` (9) and `BlockKvList`
(10) as unused. Do not change their tag numbers (to avoid shifting
other tags), but remove their use from code.

**Step 2: Remove cons-list block helpers**

In `src/eval/stg/block.rs`, remove:
- `build_index()` (line 749)
- `store_index_in_block()` (line 804)
- `BLOCK_INDEX_THRESHOLD` (line 450)
- `walk_list_to_position()` (line 693)
- `extract_value_from_pair()` (line 713)
- `count_list()` (line 731)
- `pair_key_symbol_id()` (line 777)
- `BlockListIterator` (line 658)
- Old-style branches in LOOKUPOR, MERGE, MERGEWITH, DEEPMERGE

**Step 3: Remove Native::Index**

In `src/eval/memory/syntax.rs`, remove:
- `Index(Rc<BlockIndex>)` from the `Native` enum
- Any GC handling for `Index`

**Step 4: Remove dsl helpers**

In `src/eval/stg/syntax.rs`, remove:
- `dsl::pair()` (line 408)
- `dsl::block()` (line 421)
- `no_index()` (line 417)

**Step 5: Remove PBLOCK_TO_LIST**

If the rendering pipeline has been updated to iterate HeapBlock
directly (rather than converting to list), remove the transitional
`PBlockToList` intrinsic.

**Step 6: Remove transitional P-prefix intrinsics**

Rename `PBlockFromPairs` → `Block` (or similar), `PBlockLookup` →
`LookupOr`, etc. — the P-prefix was only for coexistence.

**Step 7: Run full test suite and clippy**

```
cargo test
cargo clippy --all-targets -- -D warnings
cargo fmt --all
```

**Step 8: Commit**

```
git commit -m "chore: remove deprecated cons-list block representation"
```

---

### Task 13: Close Bead and Merge

**Step 1: Close the bead**

```
bd close eu-m59i --reason="Persistent blocks implemented with im::OrdMap"
```

**Step 2: Sync**

```
bd sync
```

**Step 3: Merge to master**

Only after benchmarks pass and all tests are green:

```
git checkout master
git merge feat/persistent-blocks
git push
```

**Step 4: Clean up branch**

```
git branch -d feat/persistent-blocks
```

---

## Summary of Deliverables

| Task | Phase | Feature | Key Files |
|------|-------|---------|-----------|
| 1 | 1 | im dependency | `Cargo.toml` |
| 2 | 1 | HeapBlock type | `memory/block.rs` |
| 3 | 1 | Native::Block + GC | `memory/syntax.rs` |
| 4 | 2 | PBLOCK_FROM_PAIRS | `stg/block.rs` |
| 5 | 2 | PBLOCK_LOOKUP | `stg/block.rs` |
| 6 | 2 | PBLOCK_MERGE/WITH | `stg/block.rs` |
| 7 | 2 | PBLOCK_TO_LIST | `stg/block.rs` |
| 8 | 2 | Harness tests | `harness/test/095` |
| 9 | 3 | Switch compiler + intrinsics | `compiler.rs`, `block.rs`, `render.rs` |
| 10 | 3 | ELEMENTS/KEYS/VALUES | `stg/block.rs` |
| 11 | 3 | Benchmarks | `benches/` |
| 12 | 4 | Remove old representation | Multiple files |
| 13 | 4 | Close bead + merge | — |

## Task Dependencies

```
Phase 1:
  Task 1 (im dep)
    └─→ Task 2 (HeapBlock type)
          └─→ Task 3 (Native::Block + GC)

Phase 2 (all depend on Task 3):
  Task 4 (PBLOCK_FROM_PAIRS)
  Task 5 (PBLOCK_LOOKUP)
  Task 6 (PBLOCK_MERGE/WITH)
  Task 7 (PBLOCK_TO_LIST)
  Task 8 (harness tests) — depends on Tasks 4-7

Phase 3 (depends on Phase 2):
  Task 9 (switch compiler) — depends on Tasks 4-7
    └─→ Task 10 (ELEMENTS/KEYS/VALUES)
          └─→ Task 11 (benchmarks)

Phase 4 (depends on Phase 3 + benchmark approval):
  Task 12 (remove old) — depends on Task 11
    └─→ Task 13 (close bead + merge)
```

Tasks 4-7 within Phase 2 can proceed in parallel.
