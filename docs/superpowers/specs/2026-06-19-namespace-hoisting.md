# eu-398r: Namespace Lambda Hoisting

**Date**: 2026-06-19
**Bead**: eu-398r
**Agent**: Quill
**Parent spec**: `2026-06-19-0.10.0-release-design.md`

## Motivation

Namespace functions (`str.upper`, `cal.now`, `vec.len`, etc.) are the most
common function call pattern in eucalypt programs. Currently they compile to
runtime `LookupOr` calls — a native function that searches the namespace block
at runtime for each invocation. The inline pass (`distribute()` in
`src/core/inline/reduce.rs`) cannot inline these because it only sees
`Lookup(Var("str"), "upper")`, not the bare `Lam` or `Intrinsic` that the
inliner requires.

## Background: Namespace Block Structure

### Regular namespaces (the primary target)

Defined in `lib/prelude.eu` as plain blocks of lambda/intrinsic wrappers:

- **str** (~line 963): `str: { of: __STR, split: __SPLIT, upper: __STR_UPPER, ... }`
- **cal** (~line 1852): `cal: { zdt: __ZDT, parse: '__ZDT.PARSE', ... }`
- **vec** (~line 1965): `vec: { of: '__VEC.OF', len: '__VEC.LEN', ... }`
- Also: `arr`, `set`, `bit`, `ch`, `graph`

These are NOT monads. They are plain blocks where every member is either a
`Lam` wrapping an intrinsic or a direct `Intrinsic` reference.

### Monadic namespaces (secondary target)

- **io** (line 34): `io: monad{...} { shell: ..., exec: ..., ... }`
- **random** (line 162): `random: monad{...} { ... }`

These have the same block-of-lambdas pattern for their members but also carry
monad metadata. Members can be self-referential (e.g. `map` depends on `bind`
and `return`).

### Core representation after desugaring

A namespace block desugars to:

```
Let(
  [("upper", Intrinsic("__STR_UPPER")), ("split", Lam(...)), ...],
  Block({"upper" → Var("upper"), "split" → Var("split"), ...}),
  DefaultBlockLet
)
```

A use site `"hello" str.upper` desugars to:

```
App(Lookup(Var("str"), "upper"), "hello")
```

### STG compilation

`Lookup` compiles to `LookupOr(key_symbol, fallback, object)` — a runtime
native function call (`src/eval/stg/compiler.rs:1411-1460`).

## Design

### New core pass: `src/core/hoist.rs`

A transformation pass that:

1. **Identifies namespace blocks**: walks the core expression tree looking for
   `Let` bindings whose RHS is a `DefaultBlockLet` block where all (or most)
   members are `Lam(_, true, _)` or `Intrinsic(_, _)`.

2. **Extracts hoistable members**: for each namespace block, extracts members
   that are:
   - `Lam(smid, true, scope)` — inlinable lambdas
   - `Intrinsic(smid, name)` — direct intrinsic references
   - NOT self-referential (do not reference other members of the same block)

3. **Creates top-level bindings**: hoisted members become new `Let` bindings at
   the same scope level as the namespace block, with mangled names using the
   `__<namespace>_<member>` convention (e.g. `__str_upper`, `__cal_zdt`). This
   convention is already established in the prelude (`__IO_BIND`,
   `__STR_UPPER`, etc.).

4. **Rewrites Lookup nodes**: `Lookup(Var("str"), "upper")` becomes
   `Var("__str_upper")` where the member was successfully hoisted.

5. **Preserves namespace blocks**: the original namespace block remains in scope
   — it may be passed as a value, used in dynamic lookups, or accessed via
   members that were not hoisted. Hoisting creates copies, not moves.

### Pipeline placement

Insert between **cook** and **inline**, so that `distribute()` sees the hoisted
bindings in its `Let` binding pool:

```
desugar → cook → **hoist** → inline → eliminate/prune → verify
```

In `src/driver/eval.rs`, add the hoisting pass call in the pipeline sequence.

### What `distribute()` does with the hoisted bindings

`distribute()` (`src/core/inline/reduce.rs:91-104`) collects `Let` bindings
where `inlinable(expr)` returns true:

```rust
fn inlinable(expr: &RcExpr) -> bool {
    matches!(&*expr.inner, Expr::Lam(_, true, _) | Expr::Intrinsic(_, _))
}
```

Hoisted namespace members are exactly `Lam(_, true, _)` or `Intrinsic(_, _)`,
so they pass this filter. `distribute()` then performs beta reduction at call
sites.

### Self-referential members

Members that reference other members of the same namespace (e.g. a monadic
`map` that calls `bind` and `return`) should NOT be hoisted in isolation — they
would lose access to sibling bindings. Strategy:

- For regular namespaces (str, cal, vec): members are typically independent
  intrinsic wrappers — all hoistable.
- For monadic namespaces (io, random): hoist only members that do not reference
  siblings. Leave self-referential members in the block.
- Detection: check whether the member's body contains `Var` references to other
  bindings in the same `Let` scope.

### Dump support

`eu dump hoisted <file>` should show the expression after the hoisting pass,
making it easy to verify which members were hoisted.

## Acceptance Criteria

1. Namespace functions appear in `eu dump inlined` output as inlined expressions
   (not wrapped in `Lookup`)
2. `eu dump hoisted` shows the new pass output with hoisted bindings visible
3. Measurable reduction in `machine_ticks` or `stg-eval` time on AoC examples
   that use namespace functions heavily
4. No behavioural change: full test suite passes (`cargo test`)
5. No change to user-visible syntax or semantics
6. `cargo clippy --all-targets -- -D warnings` clean
7. `cargo fmt --all` clean

## Out of Scope

- Hoisting from user-defined blocks (only prelude namespaces)
- Changing the namespace access syntax
- Removing the `LookupOr` infrastructure (still needed for dynamic lookups)
