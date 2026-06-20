# eu-9tah.9: Wire Demand Signature Table into STG Compiler for User Functions

**Date**: 2026-06-20
**Bead**: eu-9tah.9
**Agent**: Furnace
**Parent spec**: `2026-06-19-0.10.0-release-design.md`
**Depends on**: eu-9tah.10 (SCC splitting — greatly increases the number of
bindings that get precise demands)

## Motivation

The demand analysis pass computes per-function demand signatures (a vector of
`Demand` per argument) but the signature table is **discarded** in `eval.rs`
(`_signatures`). The STG compiler only uses the intrinsic signature table
(populated from `build_intrinsic_signatures()`) — user function signatures
are ignored.

This means:
- `f(x)` where `f` is a user function with a strict first argument still
  compiles `x` as a lazy `Thunk` — the compiler doesn't know `f` is strict
  in its first argument.
- The demand analysis pass runs, computes useful information, and then throws
  it away.

The intrinsic signatures replaced the old hacks and match baseline. User
function signatures are where the actual improvement comes from.

## Design

### Thread the signature table into the STG compiler

The analysis pass returns `(annotated_expr, signatures)` where `signatures`
is a `HashMap` mapping function identities to `Vec<Demand>`. Currently:

```rust
let (annotated, _signatures) = analyse_demands(&self.evaluand);
```

Change to:

```rust
let (annotated, signatures) = analyse_demands(&self.evaluand);
// ... thread `signatures` into StgSettings or the compiler
```

### STG compiler changes

In `src/eval/stg/compiler.rs`, when compiling an `App(f, args)` node:

1. **Look up `f`'s demand signature** in the signature table (if `f` is a
   known user function with a signature).

2. **Apply per-argument demands** from the signature to the compiled argument
   bindings, just as `compile_bif_application` does for intrinsics.

3. **Effect**: if `f`'s signature says argument 0 is `(Strict, AtMostOnce)`,
   the compiled binding for argument 0 gets `Demand::strict_once()` → the
   STG compiler emits `Value` instead of `Thunk` → no Update frame → the
   argument is evaluated eagerly at the call site.

### How to identify the callee

The challenge is matching the `App`'s callee expression to a signature table
entry. The signature table is keyed by binding identity (e.g. `Rc` pointer
or binding name). At `App` compilation time:

- If the callee is `Var(Free(name))` → look up `name` in the signature table
- If the callee is `Var(Bound(scope, binder))` → resolve to the binding's
  identity in the current scope
- If the callee is a complex expression → no signature available (conservative)

The simplest approach: key the signature table by **binding name** (String).
When compiling `App(Var(Free("f")), args)`, look up `"f"`. This works for
top-level and let-bound functions. It misses anonymous lambdas, but those
are rare in eucalypt source.

### Interaction with SCC splitting (eu-9tah.10)

Without SCC splitting, most user bindings are in `LetRec` scopes with
`Multi` cardinality. The demand signatures for these functions exist but
their argument demands are conservatively weak (the analysis can't prove
much inside a `LetRec`).

With SCC splitting, non-recursive functions become `Let` bindings. Their
demand signatures are precise. The signature wiring then becomes effective —
call sites can use the precise argument demands for thunk elision.

**This is why eu-9tah.10 should land first.**

### Blob prelude signatures

The blob prelude functions are pre-compiled. Their signatures could be:
1. **Computed during blob build** and stored in the blob metadata
2. **Recomputed on load** from the blob's binding bodies (wasteful)
3. **Seeded from intrinsic signatures** (already done for intrinsics)

Option 1 is the cleanest but requires blob format changes. For 0.10.0,
intrinsic signatures cover the prelude functions that matter most (str, cal,
etc.). User function signatures are the primary target.

## Acceptance Criteria

1. The demand signature table is threaded from the analysis pass into the
   STG compiler (no more `_signatures`)
2. `App(user_function, args)` compilation uses the callee's demand signature
   to set per-argument demands
3. At least one AoC example shows reduced `machine_allocs` with demand
   analysis enabled vs suppressed (verify with `--statistics`)
4. No regression on any AoC example
5. Full test suite passes
6. `cargo clippy --all-targets -- -D warnings` clean
7. `cargo fmt --all` clean

## Risks

- **Compilation cost**: looking up signatures for every `App` node adds a
  hash lookup per call site. Should be negligible (user programs have
  hundreds of call sites, not millions).
- **Soundness**: a wrong signature (e.g. claiming strict when actually lazy)
  would change evaluation order. The analysis is conservative by design —
  it only marks arguments strict when provably forced. But verify with
  the full test suite.
- **Name collisions**: if two different scopes have bindings with the same
  name, a name-keyed table could confuse them. Use scope-qualified names
  or binding identity pointers if this is a problem.

## Out of Scope

- Blob prelude signatures (option 1 above) — deferred
- Worker/wrapper transformation — W20
- Specialisation based on argument types — W20
