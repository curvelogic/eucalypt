# eu-9tah.2 + eu-aw9q (W11): Demand Analysis with Per-Function Signatures

**Date**: 2026-06-19
**Beads**: eu-9tah.2, eu-aw9q
**Agent**: Furnace
**Parent spec**: `2026-06-19-0.10.0-release-design.md`
**ROADMAP section**: W11 (lines 778-820)

## Motivation

Every `Let` binding reaching the STG compiler without a WHNF witness is compiled
as a `Thunk`: allocate the closure, black-hole it on entry, push an `Update`
continuation, evaluate, write back, leave a dead object for the GC to mark.
This happens on *every* binding unless the compiler can prove it's WHNF or
used-at-most-once.

The existing heuristics (W9, shipped in 0.9) are hand-rolled and cover only
sites that a programmer or intrinsic author flagged in advance:

1. **`suppress_update`** — a `bool` flag on `StgSyn::Case` and a runtime flag
   `suppress_next_update` in `vm.rs` that prevents O(N) Update accumulation
   in tail-recursive conditionals. Bespoke threading for IF alone.

2. **`single_use_args()`** — a trait method on `StgIntrinsic` with a default
   returning `&[]`. Only IF overrides it, returning `[1, 2]` (then/else
   branches). Consulted at `compiler.rs:986-1013`.

3. **Hardcoded `strict` vectors** — per-intrinsic `strict_args` lists in
   `src/eval/intrinsics.rs` encoding which arguments are strict.

No pass discovers strictness or cardinality from the program text.

## Design

### New core pass: `src/core/analyse_demand.rs`

A backward abstract interpretation over core expressions, run **after
inline/fuse, before STG compile**. Populates the `Demand` field on every
`CoreBinding` completely, replacing the partial heuristics.

### Lattice

The existing types in `src/core/demand.rs` are sufficient with minor extension:

**Strictness**: `{Unknown, Lazy, Strict}` — already defined. The analysis
refines `Unknown` to `Lazy` or `Strict`.

**Cardinality**: `{Unknown, AtMostOnce, Multi}` — already defined. Add
`Absent` for unused function arguments (distinct from dead bindings, which
the prune pass already eliminates).

**Demand** = `(Strictness, Cardinality)` — already the `Demand` struct. The
`whnf` field continues to be set by the STG compiler after compiling the
binding's body, not by the analysis pass.

### Lattice operations

Add `join` (least upper bound) to both `Strictness` and `Cardinality`:

```
Strictness::join:
  Strict ⊔ Strict = Strict
  _      ⊔ _     = Lazy     (conservative: if any branch is lazy, the whole is)

Cardinality::join:
  Absent    ⊔ x         = x
  x         ⊔ Absent    = x
  AtMostOnce ⊔ AtMostOnce = AtMostOnce  (both branches use it once → still once)
  _          ⊔ _         = Multi
```

And `lub` on `Demand` combining both axes.

### Analysis rules

The pass walks the core expression tree bottom-up, computing a **demand
environment**: a map from binding names to their aggregate demand.

For each expression form:

- **`Var(_, Bound(b))`**: the referenced binding gets demand `(Strict, AtMostOnce)`.
  Multiple references to the same binding join their cardinalities.

- **`App(_, f, args)`**: if `f` has a known demand signature (from a previous
  analysis of its `Lam` body, or from intrinsic seed data), propagate each
  argument's demand from the signature. Otherwise, all args get `(Lazy, Multi)`
  (conservative).

- **`Let(_, scope, _)`** / non-recursive: analyse the body, then propagate
  demands to each binding's RHS. A binding that appears in the body's demand
  environment is used; one that doesn't is `Absent` (for function args) or
  dead (for let bindings — already handled by prune).

- **`Let(_, scope, _)`** / recursive (`LetRec`): fixed-point iteration.
  Start all bindings at `(Lazy, Absent)`. Analyse the body and all RHS
  expressions, join demands, repeat until stable. The lattice has finite
  height (3 × 4 = 12 points per binding), so convergence is guaranteed in
  at most 12 iterations per binding (in practice 2-3).

- **`Lam(_, _, scope)`**: analyse the body. The demands on the lambda's
  parameters become its **demand signature** — a vector of `Demand`, one per
  parameter.

- **`Case` (implicit via desugaring to `if`/pattern match)**: the scrutinee
  is `Strict`. Branch bodies are joined (their demands are combined with
  `join`).

- **`Lookup(_, obj, key, fallback)`**: `obj` is `(Strict, AtMostOnce)`.
  `fallback`, if present, is `(Lazy, AtMostOnce)`.

- **`Literal`, `Intrinsic`, `Block`, `List`**: no demands on any binding.

### Per-function demand signatures

Each `Lam` node produces a demand signature from the analysis of its body.
This signature must be accessible at call sites.

**Storage**: Add a `signature: Option<Vec<Demand>>` field to `CoreBinding`
(alongside the existing `demand` field which describes how the *binding
itself* is used). Alternatively, store signatures in a side-table keyed by
binding identity (`Rc` pointer), avoiding changes to the serialised format.

**The side-table approach is preferred** — it avoids changing `CoreBinding`'s
serialisation and keeps the signature coupled to the analysis pass rather
than the data structure. The STG compiler receives the side-table and
consults it when compiling `App` nodes.

### Intrinsic seed signatures

The hardcoded `strict_args` vectors and `single_use_args` lists encode real
semantic knowledge. Before the analysis pass runs, seed the signature table
with demand signatures for all intrinsics.

#### Blanket rule

**All strict intrinsic arguments are also `AtMostOnce`**. Intrinsics evaluate
each argument exactly once and return — there is no case where a strict
intrinsic argument is evaluated more than once. This is the single biggest
refinement over the existing `strict_args` data, which only captures
strictness without cardinality. It eliminates Update frames for every strict
intrinsic call site.

Non-strict arguments default to `(Lazy, Multi)` unless refined below.

#### Specific refinements

- **IF**: `[(Strict, AtMostOnce), (Lazy, AtMostOnce), (Lazy, AtMostOnce)]`
  — condition evaluated exactly once; then/else each entered at most once
- **AND/OR** (short-circuit boolean): first arg `(Strict, AtMostOnce)`,
  second arg `(Lazy, AtMostOnce)` — second may not be evaluated at all
  (short-circuit), but if it is, it's evaluated once
- **Arithmetic** (+, -, *, /, etc.): all args `(Strict, AtMostOnce)`
- **Comparisons** (<, >, =, etc.): all args `(Strict, AtMostOnce)`
- **String operations**: all strict args `(Strict, AtMostOnce)` per blanket
  rule
- **LookupOr**: object `(Strict, AtMostOnce)`, fallback
  `(Lazy, AtMostOnce)` — fallback only evaluated if lookup fails, and at
  most once
- **IO bind** (`__IO_BIND`): action `(Strict, AtMostOnce)`, continuation
  `(Lazy, AtMostOnce)` — the continuation is called exactly once per IO step
- **map/filter function arg**: `(Lazy, Multi)` — genuinely called on every
  element, so `Multi` is correct; no refinement beyond the conservative default

#### Derivation

The seed table is derived mechanically from the existing `strict_args`
vectors (each strict index → `(Strict, AtMostOnce)` per blanket rule) plus
the specific refinements above for IF, AND/OR, LookupOr, and IO bind. The
old `strict_args` vectors and `single_use_args` trait are then deleted.

### Pipeline placement

In `src/driver/eval.rs` (or `prepare.rs`), insert between the inline/fuse
pass and STG compile:

```
... → inline → fuse-destructure → eliminate-2 → **analyse-demands** → stg-compile
```

The pass populates `CoreBinding.demand` on every binding and produces the
demand signature side-table, which is threaded into the STG compiler.

### STG compiler changes

In `src/eval/stg/compiler.rs`:

1. **`take_lambda_form()`** (line 534): already consults `demand.skip_update()`.
   No change needed here — the analysis populates the demand correctly.

2. **`compile_bif_application()`** (lines 983-1022): currently consults
   `single_use_args()` and `strict_args` per-intrinsic. Replace with a
   lookup into the demand signature side-table. The per-argument demand
   from the signature replaces both the `single_use_args` and `strict_args`
   consultations.

3. **Remove `single_use_args` consultation loop** — the signature table
   provides this information.

### Removals

After the analysis pass is working and tests pass, delete in order:

1. **`single_use_args()`** trait method from `StgIntrinsic`
   (`src/eval/machine/intrinsic.rs:124-126`)
2. **`single_use_args()` override** on IF (`src/eval/stg/boolean.rs:143-145`)
3. **Search for any other `single_use_args` overrides** and remove them
4. **`suppress_update` field** from `StgSyn::Case`
   (`src/eval/stg/syntax.rs:133`) and `HeapSyn::Case`
   (`src/eval/memory/syntax.rs:243`) and `ArenaStgSyn::Case`
   (`src/eval/stg/arena.rs:115`)
5. **`suppress_next_update` flag** and all related logic in `vm.rs`
   (lines 293-297, 430-452, 504-522, 836-856)
6. **`switch_suppress()`** in `syntax.rs` (line 541-553)
7. **Hardcoded `strict` vectors** from `src/eval/intrinsics.rs` — replaced
   by intrinsic seed signatures

Each deletion should leave tests passing. If a deletion causes a failure,
the analysis pass is not yet producing equivalent demands — fix the analysis,
not the deletion.

### Interaction with the prune pass

The prune pass (`src/core/simplify/prune.rs`) already populates
`CoreBinding.demand` from use counts (lines 458-474). The demand analysis
pass runs **after** prune and **overwrites** the demand with its more
precise analysis. This is safe because the analysis subsumes the prune
pass's use-count heuristic.

### Interaction with eu-aw9q (recursive scopes)

The bead eu-aw9q specifically concerns safe `AtMostOnce` for recursive
scopes. The analysis pass's `LetRec` rule (fixed-point iteration) handles
this: a binding that references itself gets `Multi` cardinality (recursive
use is always multi). A binding in a `LetRec` that does NOT reference itself
or any sibling that references it can safely get `AtMostOnce`.

The key insight from eu-aw9q: the prune pass's syntactic reference counting
cannot soundly emit `AtMostOnce` for `LetRec` bindings because the scope
is recursive. The demand analysis pass's fixed-point handles this correctly
by iterating until the cardinality stabilises.

### Debugging flag

Add `suppress_demand_analysis: bool` to the pipeline settings (alongside
the existing `suppress_updates`, `suppress_inlining`, `suppress_optimiser`
flags). When set, the analysis pass is skipped entirely and all demands
remain at `Unknown` (the conservative default). This enables A/B testing
of the analysis's effect on any program.

Expose via CLI flag `--suppress-demand-analysis` or environment variable.

### Dump support

Add `eu dump demands <file>` showing the core expression with demand
annotations on every binding, and the demand signature side-table for all
analysed functions.

## Acceptance Criteria

1. **`suppress_update`** flag and all runtime logic deleted from `syntax.rs`,
   `arena.rs`, `loader.rs`, `mutator.rs`, `cont.rs`, `vm.rs`
2. **`single_use_args()`** trait method and all overrides deleted from
   `intrinsic.rs`, `boolean.rs`
3. **Hardcoded `strict` vectors** deleted from `intrinsics.rs`
4. **`suppress_updates` global flag** in `StgSettings` / compiler remains
   (it's a debugging escape hatch, orthogonal to the analysis)
5. IF-heavy programs (AoC tail-recursive conditionals) show **no performance
   regression** — the analysis must produce demands at least as good as the
   hand-rolled heuristics
6. Thunk allocation count **reduced** compared to eu-9tah.4 baseline on at
   least 3 AoC examples
7. `eu dump demands` shows demand annotations on bindings
8. Tail-recursive conditionals show **flat stack depth** under `EU_STACK_DIAG=1`
   — no O(N) Update accumulation
9. All tests pass: `cargo test`
10. `cargo clippy --all-targets -- -D warnings` clean
11. `cargo fmt --all` clean

## Risks

- **Soundness**: a wrong `AtMostOnce` on a `Multi`-use binding causes
  redundant re-evaluation but never incorrect results (the binding is pure).
  The `Demand` doc comments already note this. Still, the analysis should be
  conservative — prefer `Multi` over `AtMostOnce` when uncertain.

- **Performance of the pass itself**: the fixed-point iteration for `LetRec`
  could be slow on the prelude's ~9,500-binding scope. Bound the iteration
  count and profile. The ROADMAP notes "linear pass adds milliseconds" as
  the expectation.

- **Prelude blob interaction**: the blob is pre-compiled STG. The demand
  analysis runs on core expressions before STG compile. When the blob is
  active, prelude bindings are not available as core expressions — the
  analysis only runs on user code. Intrinsic seed signatures cover the
  prelude functions accessed via `Ref::G`. This is correct: blob bindings
  already have their demands baked in at blob-compile time.

## Out of Scope

- Projection-based strictness (head-strict, spine-strict) — post-1.0
  refinement per ROADMAP
- Worker/wrapper transformation — requires demand signatures + unboxing,
  deferred to W20
- Type-directed specialisation — W20
- Strictness for IO actions — IO is inherently sequential, no analysis needed
