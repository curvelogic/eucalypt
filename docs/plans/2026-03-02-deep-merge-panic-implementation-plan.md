# Deep Merge Panic Fix Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Fix the panic in `<<` (DEEPMERGE) when either operand is a dynamically-constructed block (via `block()`). The `<<` operator should handle dynamic blocks identically to static blocks.

**Bug:** `{x: 1} << block([[:y, 2]])` panics with "could not navigate to native". The equivalent `merge({x: 1}, block([[:y, 2]]))` works correctly.

**Architecture:**

The eucalypt STG machine represents block key-value pairs in two forms:

- **Static blocks** (compiled from syntax): `BlockPair(Ref::V(Native::Sym(id)), value)` — the key is a raw native symbol, directly navigable.
- **Dynamic blocks** (from `block()` intrinsic): The input list `[[:y, 2]]` passes through `Kv` which wraps it as `BlockKvList([:y, 2])`. The key `:y` remains a `BoxedSymbol` (a `Cons { tag: BoxedSymbol, args: [Ref::V(Native::Sym)] }` node).

The `MERGE` intrinsic uses `PackPair` to prepare items, which calls `ExtractKey` — this properly unboxes the key symbol via `unbox_sym`. However, `MERGEWITH` (called by `DEEPMERGE`) uses `BlockPair` instead, which extracts `k` from the list head without unboxing. The resulting `BlockPair(BoxedSymbol(...), v)` has a boxed key that `deconstruct()` cannot navigate via `navigate_local_native()`, causing a panic.

The fix is in `BlockPair`'s wrapper: when extracting from `BlockKvList`, unbox the key symbol before creating the `BlockPair`. This ensures all `BlockPair` nodes have raw sym keys regardless of whether the block was constructed statically or dynamically.

A secondary defensive fix is needed in `deconstruct()` and `pair_key_symbol_id()`, which call `navigate_local_native` (which panics). These should be made robust against boxed symbols by handling the `Cons { tag: BoxedSymbol, ... }` case, falling back gracefully rather than panicking.

**Tech Stack:** Rust, STG intrinsics

**Key Files:**
- `src/eval/stg/block.rs` — `BlockPair`, `deconstruct`, `pair_key_symbol_id`, `DeepMerge`
- `src/eval/machine/env.rs` — `navigate_local_native`
- `harness/test/015_block_fns.eu` — existing merge/deep-merge tests
- `tests/harness_test.rs` — harness test registration

---

## Root Cause Analysis

1. `BlockPair` intrinsic (line ~420 of `block.rs`) handles `BlockKvList` by extracting list elements `[k, v]` and creating `data(BlockPair, [k, v])`. For dynamic blocks, `k` is a `BoxedSymbol`, not a raw sym.

2. `MERGEWITH`'s wrapper uses `BlockPair` (not `PackPair`) to convert kvs, so it passes boxed keys to the BIF.

3. `MERGEWITH::execute()` calls `deconstruct()`, which calls `pair_closure.navigate_local_native(&view, k)`. This function only follows `Atom { evaluand }` chains; encountering a `Cons { tag: BoxedSymbol, ... }` node, it panics.

4. `pair_key_symbol_id()` has the same vulnerability — it also calls `navigate_local_native` on the key and would panic on boxed keys.

---

### Task 1: Add Harness Test for Deep Merge with Dynamic Blocks

Write a failing test that reproduces the bug before fixing it.

**Files:**
- Create: `harness/test/085_deep_merge_dynamic.eu`
- Modify: `tests/harness_test.rs`

**Step 1: Create the test file**

Create `harness/test/085_deep_merge_dynamic.eu`:

```eucalypt
# Deep merge (<<) with dynamically-constructed blocks
#
# The << operator should handle blocks built via block()
# identically to static blocks.

# --- Static deep merge (already works) ---
static-checks: {
  trues: [
    ({x: 1} << {y: 2}) = {x: 1 y: 2},
    ({} << {a: 1}) = {a: 1},
    ({a: 1} << {}) = {a: 1},
    ({a: 1} << {a: 2}) = {a: 2}
  ]
}

# --- Dynamic deep merge (previously panicked) ---
dynamic-checks: {
  trues: [
    ({x: 1} << block([[:y, 2]])) = {x: 1 y: 2},
    ({} << block([[:x, 1]])) = {x: 1},
    (block([[:a, 1]]) << {b: 2}) = {a: 1 b: 2},
    (block([[:a, 1]]) << block([[:b, 2]])) = {a: 1 b: 2}
  ]
}

# --- Dynamic deep merge with sym() ---
sym-checks: {
  trues: [
    ({} << block([[sym("x"), 1]])) = {x: 1},
    ({a: 1} << block([[sym("b"), 2]])) = {a: 1 b: 2}
  ]
}

# --- Deep merge recurses into nested blocks ---
nested-checks: {
  trues: [
    ({a: {x: 1}} << block([[:a, {y: 2}]])) = {a: {x: 1 y: 2}},
    (block([[:a, {x: 1}]]) << {a: {y: 2}}) = {a: {x: 1 y: 2}}
  ]
}

# --- Shallow merge with dynamic blocks (already works, regression guard) ---
merge-checks: {
  trues: [
    merge({x: 1}, block([[:y, 2]])) = {x: 1 y: 2},
    merge(block([[:a, 1]]), {b: 2}) = {a: 1 b: 2}
  ]
}

pass: [
  static-checks.trues all-true?,
  dynamic-checks.trues all-true?,
  sym-checks.trues all-true?,
  nested-checks.trues all-true?,
  merge-checks.trues all-true?
] all-true?

RESULT: if(pass, :PASS, :FAIL)
```

**Step 2: Register the test in `tests/harness_test.rs`**

Add after the `test_harness_084` function:

```rust
#[test]
pub fn test_harness_085() {
    run_test(&opts("085_deep_merge_dynamic.eu"));
}
```

**Verify (expect FAILURE):**
```bash
cargo test test_harness_085 2>&1 | tail -20
```

---

### Task 2: Fix BlockPair to Unbox Keys from BlockKvList

The `BlockPair` intrinsic must unbox the key symbol when extracting from a `BlockKvList`. This is the primary fix — it ensures that `BlockPair` always produces pairs with raw native sym keys.

**Files:**
- Modify: `src/eval/stg/block.rs` (the `BlockPair` wrapper)

**Step 1: Add `unbox_sym` to the BlockPair wrapper's BlockKvList branch**

In the `BlockPair::wrapper` method, the `BlockKvList` branch currently creates:
```rust
data(DataConstructor::BlockPair.tag(), vec![lref(2), lref(0)])
```
where `lref(2)` is the raw list head `k` — which may be a `BoxedSymbol`.

Change this to unbox the key first:

```rust
// Before (line ~420-437 of block.rs):
(
    DataConstructor::BlockKvList.tag(), // [lcons] [kv]
    switch(
        local(0),
        vec![(
            DataConstructor::ListCons.tag(), // [k t] [lcons] [kv]
            switch(
                local(1),
                vec![(
                    DataConstructor::ListCons.tag(), // [v .] [k t] [lcons] [kv]
                    data(
                        DataConstructor::BlockPair.tag(),
                        vec![lref(2), lref(0)],
                    ),
                )],
            ),
        )],
    ),
),

// After:
(
    DataConstructor::BlockKvList.tag(), // [lcons] [kv]
    switch(
        local(0),
        vec![(
            DataConstructor::ListCons.tag(), // [k t] [lcons] [kv]
            switch(
                local(1),
                vec![(
                    DataConstructor::ListCons.tag(), // [v .] [k t] [lcons] [kv]
                    unbox_sym(
                        local(2), // force unbox k
                        // [sym] [v .] [k t] [lcons] [kv]
                        data(
                            DataConstructor::BlockPair.tag(),
                            vec![lref(0), lref(1)],
                        ),
                    ),
                )],
            ),
        )],
    ),
),
```

The `unbox_sym(local(2), ...)` destructures the head element (`k`) as a `BoxedSymbol`, binding the inner raw sym at position 0. The `data(BlockPair, [lref(0), lref(1)])` then creates the pair with the raw sym key and the value `v` (shifted from `lref(0)` to `lref(1)` due to the new binding).

**Verify:**
```bash
cargo test test_harness_085
```

---

### Task 3: Make `deconstruct` Defensive Against Boxed Symbols

Even with `BlockPair` fixed, `deconstruct` should not panic on unexpected key representations. Replace the `navigate_local_native` call with navigation that handles `BoxedSymbol` gracefully.

**Files:**
- Modify: `src/eval/stg/block.rs` (the `deconstruct` function)

**Step 1: Replace `navigate_local_native` with a robust key extraction**

The current `deconstruct` function (around line 988-1016) calls:
```rust
let sym = if let syntax::Native::Sym(id) = pair_closure.navigate_local_native(&view, k) {
    pool.resolve(id).to_string()
} else {
    panic!("bad block_pair passed to merge intrinsic: non-symbolic key")
};
```

Replace with a helper that handles both raw sym refs and boxed symbols:

```rust
let sym = extract_sym_from_key(view, pair_closure, k, pool)
    .unwrap_or_else(|| {
        panic!("bad block_pair passed to merge intrinsic: non-symbolic key")
    });
```

**Step 2: Add the `extract_sym_from_key` helper function**

Add before the `deconstruct` function:

```rust
/// Extract a symbol string from a block pair key.
///
/// Handles both raw `Ref::V(Native::Sym)` keys (from static blocks)
/// and `BoxedSymbol` keys (from dynamically-constructed blocks).
fn extract_sym_from_key(
    view: MutatorHeapView,
    closure: &SynClosure,
    key_ref: crate::eval::memory::syntax::Ref,
    pool: &crate::eval::memory::symbol::SymbolPool,
) -> Option<String> {
    use crate::eval::memory::syntax::{self, HeapSyn};

    match &key_ref {
        syntax::Ref::V(Native::Sym(id)) => {
            return Some(pool.resolve(*id).to_string());
        }
        syntax::Ref::V(_) => return None,
        _ => {}
    }

    let key_closure = closure.navigate_local(&view, key_ref);
    let code = view.scoped(key_closure.code());
    match &*code {
        HeapSyn::Atom { evaluand: syntax::Ref::V(Native::Sym(id)) } => {
            Some(pool.resolve(*id).to_string())
        }
        HeapSyn::Cons { tag, args } if *tag == DataConstructor::BoxedSymbol.tag() => {
            if let Some(inner) = args.get(0) {
                if let Native::Sym(id) = key_closure.navigate_local_native(&view, inner) {
                    Some(pool.resolve(id).to_string())
                } else {
                    None
                }
            } else {
                None
            }
        }
        _ => None,
    }
}
```

**Verify:**
```bash
cargo test test_harness_085
cargo test test_harness_015
```

---

### Task 4: Make `pair_key_symbol_id` Defensive Against Boxed Symbols

The block indexing function `pair_key_symbol_id` also calls `navigate_local_native` and would panic on boxed symbol keys.

**Files:**
- Modify: `src/eval/stg/block.rs` (the `pair_key_symbol_id` function)

**Step 1: Replace `navigate_local_native` with robust extraction**

The current code (around line 777-796):
```rust
fn pair_key_symbol_id(
    view: MutatorHeapView<'_>,
    pair: &SynClosure,
) -> Option<crate::eval::memory::symbol::SymbolId> {
    use crate::eval::memory::syntax::HeapSyn;

    let code = view.scoped(pair.code());
    match &*code {
        HeapSyn::Cons { tag, args } if *tag == DataConstructor::BlockPair.tag() => {
            let k = args.get(0)?;
            let native = pair.navigate_local_native(&view, k);
            if let Native::Sym(id) = native {
                Some(id)
            } else {
                None
            }
        }
        _ => None,
    }
}
```

Replace with:
```rust
fn pair_key_symbol_id(
    view: MutatorHeapView<'_>,
    pair: &SynClosure,
) -> Option<crate::eval::memory::symbol::SymbolId> {
    use crate::eval::memory::syntax::HeapSyn;

    let code = view.scoped(pair.code());
    match &*code {
        HeapSyn::Cons { tag, args } if *tag == DataConstructor::BlockPair.tag() => {
            let k = args.get(0)?;
            extract_sym_id_from_key(view, pair, k)
        }
        _ => None,
    }
}

/// Extract a symbol ID from a block pair key ref.
///
/// Handles raw `Ref::V(Native::Sym)` (static blocks) and
/// `BoxedSymbol` constructors (dynamic blocks).
fn extract_sym_id_from_key(
    view: MutatorHeapView<'_>,
    closure: &SynClosure,
    key_ref: crate::eval::memory::syntax::Ref,
) -> Option<crate::eval::memory::symbol::SymbolId> {
    use crate::eval::memory::syntax::{self, HeapSyn};

    // Fast path: raw native sym
    if let syntax::Ref::V(Native::Sym(id)) = &key_ref {
        return Some(*id);
    }

    // Navigate to the key closure
    if let syntax::Ref::L(_) = &key_ref {
        let key_closure = closure.navigate_local(&view, key_ref);
        let code = view.scoped(key_closure.code());
        match &*code {
            HeapSyn::Atom { evaluand: syntax::Ref::V(Native::Sym(id)) } => Some(*id),
            HeapSyn::Cons { tag, args }
                if *tag == DataConstructor::BoxedSymbol.tag() =>
            {
                let inner = args.get(0)?;
                if let Native::Sym(id) = key_closure.navigate_local_native(&view, inner) {
                    Some(id)
                } else {
                    None
                }
            }
            _ => None,
        }
    } else {
        None
    }
}
```

**Verify:**
```bash
cargo test test_harness_085
cargo test test_harness_073
cargo test test_harness_015
```

---

### Task 5: Run Full Test Suite and Clippy

Verify nothing is broken and code quality is maintained.

**Files:** None (verification only)

**Step 1: Run clippy**
```bash
cargo clippy --all-targets -- -D warnings
```

**Step 2: Run formatting**
```bash
cargo fmt --all
```

**Step 3: Run all harness tests**
```bash
cargo test --test harness_test
```

**Step 4: Run full test suite**
```bash
cargo test
```

All tests must pass with no clippy warnings before committing.

---

### Task 6: Commit the Fix

**Step 1: Commit all changes**

Stage and commit with an appropriate message:

```bash
git add harness/test/085_deep_merge_dynamic.eu tests/harness_test.rs src/eval/stg/block.rs
git commit -m "fix: deep merge (<<) panic on dynamically-constructed blocks

BlockPair intrinsic now unboxes the key symbol when extracting from
BlockKvList, ensuring BlockPair always has raw native sym keys.

Also hardens deconstruct() and pair_key_symbol_id() against boxed
symbol keys as a defensive measure.

Adds harness test 085 covering deep merge with block(), sym(),
nested blocks, and mixed static/dynamic operands.

Closes eu-nqde"
```

---

## Summary of Changes

| File | Change | Purpose |
|------|--------|---------|
| `src/eval/stg/block.rs` | Add `unbox_sym` in `BlockPair::wrapper` | Primary fix: ensure raw sym keys |
| `src/eval/stg/block.rs` | Add `extract_sym_from_key` helper | Defensive: robust key extraction |
| `src/eval/stg/block.rs` | Fix `deconstruct` to use helper | Defensive: no panic on boxed keys |
| `src/eval/stg/block.rs` | Add `extract_sym_id_from_key` helper | Defensive: robust symbol ID extraction |
| `src/eval/stg/block.rs` | Fix `pair_key_symbol_id` to use helper | Defensive: no panic on boxed keys |
| `harness/test/085_deep_merge_dynamic.eu` | New test file | Regression tests for the bug |
| `tests/harness_test.rs` | Register test 085 | Wire up the new test |
