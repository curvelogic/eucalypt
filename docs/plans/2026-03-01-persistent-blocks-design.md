# Persistent Blocks Design

**Bead:** eu-m59i
**Status:** Design approved, implementation plan pending
**Depends on:** eu-hmji (persistent sets — introduces `im` crate)

## Overview

Replace the cons-list block representation with `im::OrdMap` for
key-value storage, giving O(log n) lookup without a separate lazy
index, and structural sharing on merge. This solves the block index
invalidation problem: merged blocks currently lose their index and
regress to O(n) lookup.

**Branch and benchmark requirement:** This is a fundamental change to
the core data structure. All work must happen on a feature branch with
comparative benchmarking against the current implementation before
merging to master. If performance regresses unacceptably, the branch
is not merged and we fall back to a simpler "eager index on merge"
fix.

---

## Section 1: Data Representation

The new block representation replaces the cons-list + lazy HashMap
with a single `im::OrdMap`:

```rust
pub struct HeapBlock {
    entries: im::OrdMap<SymbolId, BlockEntry>,
    next_order: usize,
}

pub struct BlockEntry {
    order: usize,
    value: Cell<Ref>,
}
```

- `entries`: persistent ordered map keyed by `SymbolId` for O(log n)
  lookup and structural sharing on merge
- `next_order`: counter for insertion-order tracking (monotonically
  increasing)
- `order`: per-entry insertion position, used to reconstruct
  declaration order at render time
- `value`: `Cell<Ref>` — the thunk pointer into the GC heap, using
  interior mutability so the GC can update forwarding pointers
  in-place through structurally-shared nodes

A new `Native` variant holds the heap pointer:

```rust
pub enum Native {
    // ... existing variants ...
    Block(RefPtr<HeapBlock>),
}
```

`HeapBlock` lives on the GC heap as an opaque blob (same pattern as
`HeapSet`, `HeapString`), allocated via `alloc_blob` and implementing
`StgObject`.

The `im` crate is used with the `rc` feature (single-threaded `Rc`
sharing, not `Arc`) since eucalypt is single-threaded.

---

## Section 2: Block Construction

The STG compiler's `compile_block()` currently builds a cons-list of
`BlockPair` cells in reverse order, then wraps in `dsl::block()`.
With persistent blocks:

1. **Compile-time**: The compiler emits code to create block pairs as
   before, but calls a new intrinsic (`PBLOCK_FROM_PAIRS`) that
   builds a `HeapBlock` directly from the pair list.

2. **Runtime construction**: The intrinsic takes a list of key-value
   pairs (as now) and inserts them into an `im::OrdMap` one by one,
   tracking insertion order via `next_order`.

3. **Block literals**: For statically-known blocks, the compiler could
   potentially emit a single `HeapBlock` allocation with all entries
   pre-populated, avoiding the intermediate pair list entirely.

The `DataConstructor::Block` tag (8) is still used for case-analysis
(is this a block?), but the args hold a single
`Ref::V(Native::Block(ptr))` rather than `[list_ref, index_ref]`.

**Deprecation:** `BlockPair` (tag 9) and `BlockKvList` (tag 10) are
retained during migration as intermediate forms consumed by the
construction intrinsic. They are deprecated and scheduled for removal
once all block construction goes through `HeapBlock` directly.

---

## Section 3: Lookup

Both existing lookup paths (STG find-loop and LOOKUPOR intrinsic with
lazy indexing) collapse into a single O(log n) operation:

```rust
fn lookup(block: &HeapBlock, key: SymbolId) -> Option<Ref> {
    block.entries.get(&key).map(|entry| entry.value.get())
}
```

Simplifications:
- `LOOKUPOR` / `LOOKUPOR#` intrinsics reduce to `entries.get(&key)`,
  returning value if found, default if not
- The STG find-loop wrapper is no longer needed
- `BLOCK_INDEX_THRESHOLD` (16) and all index-building/caching code
  deleted
- `Native::Index` variant removed
- `build_index`, `store_index_in_block`, and the unsafe mutation code
  deleted

---

## Section 4: Merge

With `im::OrdMap`, merge becomes structural sharing:

```rust
fn merge(left: &HeapBlock, right: &HeapBlock) -> HeapBlock {
    let mut result = left.entries.clone();  // O(1) structural sharing
    let mut next_order = left.next_order;
    for (key, entry) in right.entries.iter() {
        let order = if result.contains_key(key) {
            result.get(key).unwrap().order  // keep left-side position
        } else {
            let o = next_order;
            next_order += 1;
            o
        };
        result.insert(*key, BlockEntry {
            order,
            value: Cell::new(entry.value.get()),
        });
    }
    HeapBlock { entries: result, next_order }
}
```

Key properties:
- **O(m log n)** where m = right block size, n = left block size
- **Result has instant lookup** — no index regression (the main
  problem this bead solves)
- **Insertion order preserved** — overridden keys keep their original
  position, new keys append
- **No intermediate `IndexMap`** — no String extraction, no closure
  list conversion
- **Deep merge** (`MERGEWITH`) works similarly but calls the combine
  function for duplicate keys instead of overriding

The `deconstruct()` helper that extracts keys as `String` via the
symbol pool is no longer needed for merge — we work with `SymbolId`
throughout.

---

## Section 5: Rendering and Export

`im::OrdMap` is sorted by `SymbolId`, not declaration order. At
render time, we restore declaration order using the `order` field:

```rust
fn ordered_entries(block: &HeapBlock) -> Vec<(SymbolId, Ref)> {
    let mut entries: Vec<_> = block.entries.iter()
        .map(|(k, e)| (e.order, *k, e.value.get()))
        .collect();
    entries.sort_by_key(|(order, _, _)| *order);
    entries.into_iter().map(|(_, k, v)| (k, v)).collect()
}
```

This is O(n log n) for the sort, but only happens at render time.
`ELEMENTS`, `KEYS`, `VALUES` intrinsics also use ordered iteration.

**Performance concern:** The ordering overhead applies to every block
iteration, not just rendering. Some uses (e.g. merge, which doesn't
care about order) can skip it, but consumers that expect declaration
order (export, `keys`, `values`, `elements`) pay the cost.
Benchmarking must verify this is acceptable in practice.

---

## Section 6: GC Integration

With `Cell<Ref>` interior mutability, GC integration is
straightforward.

**Mark phase** — iterate all entries, mark any heap pointers:

```rust
fn mark_block_pointers(heap_view: &dyn HeapView, block: &HeapBlock) {
    for (_, entry) in block.entries.iter() {
        mark_ref_heap_pointers(heap_view, &entry.value.get());
    }
}
```

**Update phase** — iterate all entries, update forwarded pointers
in-place via `Cell::set()`:

```rust
fn update_block_pointers(
    block: &HeapBlock,
    mapping: &HashMap<RefPtr<HeapSyn>, RefPtr<HeapSyn>>,
) {
    for (_, entry) in block.entries.iter() {
        let r = entry.value.get();
        if let Some(updated) = update_ref(&r, mapping) {
            entry.value.set(updated);
        }
    }
}
```

Because structurally-shared nodes are the same `Rc<Node>` internally,
updating a `Cell` in one block's traversal automatically fixes all
blocks sharing that node. GC update work is proportional to the number
of *unique* entries across all live blocks, not the total — structural
sharing reduces GC work.

---

## Section 7: Migration Strategy

This is a big change touching the compiler, runtime, GC, merge,
lookup, export, and intrinsics. The migration is staged in four
phases, all on a feature branch.

**Phase 1: Introduce HeapBlock alongside existing representation**
- Add `im` dependency (after eu-hmji lands persistent sets)
- Add `HeapBlock`, `BlockEntry`, `Native::Block` variant
- Add GC mark/update support for the new variant
- Existing block construction, lookup, merge unchanged

**Phase 2: New intrinsics for persistent blocks**
- `PBLOCK_FROM_PAIRS` — construct HeapBlock from pair list
- `PBLOCK_LOOKUP` — O(log n) lookup
- `PBLOCK_MERGE` — structural sharing merge
- These coexist with old intrinsics — both representations work

**Phase 3: Switch compiler to emit persistent blocks**
- `compile_block()` emits `PBLOCK_FROM_PAIRS` instead of cons-list
- Runtime `BLOCK` intrinsic builds HeapBlock
- Old lookup/merge intrinsics detect both representations during
  transition

**Phase 4: Remove old representation**
- Delete cons-list block construction
- Delete `BlockPair`, `BlockKvList` data constructors (deprecated)
- Delete `Native::Index`, `build_index`, `store_index_in_block`
- Delete STG find-loop wrapper
- Simplify lookup/merge intrinsics (only HeapBlock path)

**Benchmark requirements before merge:**
- Lookup performance (small blocks, large blocks, post-merge blocks)
- Merge performance (shallow, deep, repeated)
- Memory usage (structural sharing effectiveness)
- GC pause time impact
- Full harness test suite (`cargo test`)
- Benchmarks (`cargo bench`)

If performance regresses unacceptably, the branch is not merged and
the fallback is the simpler "eager index on merge" fix (build HashMap
index immediately after merge, no persistent data structures needed).

---

## Dependencies

- **eu-hmji** (persistent sets using `im::HashSet`) — introduces the
  `im` crate dependency; must land first

## Out of Scope

- Persistent sets (tracked in eu-hmji)
- Changes to block syntax or semantics
- Bytecode VM changes (eu-b4c) — persistent blocks work with either
  interpreter
