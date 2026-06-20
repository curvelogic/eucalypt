# eu-9tah.11: Core Pass — Re-flatten Nested Let Scopes After Demand Analysis

**Date**: 2026-06-20
**Bead**: eu-9tah.11
**Agent**: Quill (core pass territory)
**Depends on**: eu-9tah.10 (SCC splitting — currently disabled)
**Unblocks**: Re-enabling SCC splitting for effective demand analysis

## Principle

Core passes handle transformation. STG compile should be simple.

## Motivation

SCC splitting (eu-9tah.10) correctly decomposes `LetRec` scopes into
nested `Let` + `LetRec` chains based on dependency analysis. This enables
the demand analysis pass to assign precise `AtMostOnce` cardinality to
non-recursive single-use bindings — allocations dropped 31% on day12.

But at runtime, each `Let` scope creates a separate `EnvFrame` linked
to its parent. `Ref::L(i)` traverses `i` frames in the chain. The deeper
nesting from SCC splitting caused catastrophic O(depth) regressions:

| Example | Before | After SCC | Change |
|---------|--------|-----------|--------|
| day01-p2 | 9.7s | 39.2s | 4× |
| day09-p1 | 8.3s | 66.7s | 8× |
| day12-p1 | 0.3s | 3.4s | 12× |
| day04/06/07/08/10/11 | completed | TIMEOUT | — |

The pass is disabled pending this fix.

## Design

### New core pass: `src/core/reflatten.rs`

A pass that runs **after demand analysis, before STG compile**. It walks
the expression tree and merges chains of nested `Let`/`LetRec` scopes
back into a single `LetRec` — but with the per-binding demand annotations
preserved from the analysis.

### Pipeline placement

```
desugar → cook → SCC split → hoist → inline → prune
        → demand analysis → RE-FLATTEN → STG compile
```

### Algorithm

Walk the expression tree. At each `Let` node:

1. Check if the body is another `Let` or `LetRec`
2. If so, collect all bindings from the chain into a flat list
3. Merge into a single `LetRec` scope with the innermost body
4. Adjust de Bruijn indices as bindings are combined

Each `CoreBinding` in the merged scope retains its `demand` field
exactly as set by the analysis pass. The STG compiler then sees a
normal `LetRec` and consults `CoreBinding.demand` to decide `Value`
vs `Thunk` per binding — no STG compiler changes needed.

### Example

After SCC splitting + demand analysis:

```
Let { a = 1 (demand: Strict, AtMostOnce) } in
Let { b = a + 1 (demand: Strict, AtMostOnce) } in
LetRec { f = \n -> f(n-1) (demand: Lazy, Multi) } in
body
```

After re-flattening:

```
LetRec {
  a = 1       (demand: Strict, AtMostOnce → Value)
  b = a + 1   (demand: Strict, AtMostOnce → Value)
  f = \n -> f(n-1)  (demand: Lazy, Multi → Thunk)
} in body
```

Single `LetRec`, single `EnvFrame` at runtime, but `a` and `b` are
compiled as `Value` (no Update frame) while `f` is compiled as `Thunk`.

### De Bruijn index adjustment

When merging `Let { [a] } in Let { [b] } in body`, the inner scope's
bindings reference `a` as `Var::Bound(scope=1, binder=0)`. After
flattening into `LetRec { [a, b] } in body`, the reference becomes
`Var::Bound(scope=0, binder=0)`.

The codebase has `open_let_scope` / `close_let_scope` machinery for
exactly this kind of scope manipulation. The pass should:

1. Open each scope in the chain (converting bound vars to free)
2. Concatenate binding lists
3. Close as a single scope (converting free vars back to bound)

### What NOT to flatten

- **`DefaultBlockLet`** scopes — these represent block values, not
  function bindings. Never merge across block boundaries.
- **Scopes separated by non-Let expressions** — only merge chains of
  directly nested `Let`/`LetRec` nodes.
- **Scopes from different compilation units** — in practice this doesn't
  arise since the pass runs on the merged core expression.

### LetType of the merged scope

The merged scope should be `LetType::LetRec` if ANY constituent scope
was `LetRec` (recursive bindings need the recursive environment).
If all constituent scopes were non-recursive `Let`, the merged scope
can remain `Let` — though in practice this is unlikely since the SCC
split keeps truly recursive bindings in `LetRec`.

## Acceptance Criteria

1. New pass in `src/core/reflatten.rs` merges nested Let/LetRec chains
2. Per-binding demand annotations are preserved through the merge
3. De Bruijn indices are correctly adjusted
4. `DefaultBlockLet` scopes are never merged
5. SCC splitting is re-enabled (`loader.split_letrecs()` uncommented)
6. No wall-clock time regressions on AoC examples (compare against
   baseline with SCC splitting disabled)
7. Allocation count improvements from SCC splitting are preserved
   (day12: -31% or better)
8. `eu dump split` still shows the split structure (runs before flatten)
9. A new `eu dump reflatten` (or similar) shows the flattened result
10. Full test suite passes: `cargo test`
11. `cargo clippy --all-targets -- -D warnings` clean
12. `cargo fmt --all` clean

## Performance constraint

Eucalypt compiles on every execution. The pass is O(n) in the number
of Let/LetRec scopes — negligible for typical programs. Verify with
`--statistics` that the pass stays sub-millisecond.

## Risks

- **De Bruijn index errors**: scope manipulation is error-prone. The
  open/close machinery mitigates this, but thorough testing is essential.
  Run the full harness suite and verify with `eu dump stg` that variable
  references are correct.
- **Interaction with other passes**: the re-flatten runs after all
  analysis passes, so it only affects STG compilation. No upstream
  pass is affected.

## Out of Scope

- Changing the STG compiler's scope handling
- Changing the runtime `EnvFrame` representation
- Optimising `Ref::L` traversal to O(1)
