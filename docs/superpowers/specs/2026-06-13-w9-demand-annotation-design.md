# W9: One Demand Annotation Behind the Strictness Heuristics

- **Date:** 2026-06-13
- **Status:** Draft
- **Bead:** eu-kgsi.4
- **Roadmap:** W9 — One demand annotation behind the strictness heuristics

---

## 1. Problem

Eucalypt has six separate mechanisms that all decide *Value-vs-Thunk* or
*skip-Update* from facets of one question — is this binding used-at-most-once,
strict, or already in WHNF?

| # | Mechanism | Location | Scope |
|---|-----------|----------|-------|
| 1 | `LambdaForm::Value` vs `Thunk` | `compiler.rs:521` | Per-binding decision |
| 2 | Per-intrinsic `strict_args` | `intrinsics.rs:11` | Per-intrinsic arg list |
| 3 | Per-intrinsic `single_use_args` | `intrinsic.rs:124`, `boolean.rs:143` | IF branches only |
| 4 | `suppress_update` Case flag | `syntax.rs:99`, `vm.rs:815` | IF runtime threading |
| 5 | `is_whnf()` | `syntax.rs:143` | Per-expression predicate |
| 6 | Global `suppress_updates` | `mod.rs:296` | Whole-program kill switch |

Two are per-intrinsic hand lists; one is bespoke runtime threading for a
single intrinsic; one is a predicate on compiled syntax. The decision point
in `take_lambda_form` consults three of them (`single_use`, `is_whnf`,
`suppress_updates`); the others feed in indirectly through the compiler.

There is no single representation that captures what is known about a
binding's demand, and no place where a future analysis (W11) could write its
results.

## 2. Goal

Unify the compile-time heuristics behind **one demand annotation per binding**
that `take_lambda_form` consults as the single decision point. The existing
mechanisms become *populators* of the annotation. The annotation on an exported
binding becomes a **strictness signature** the Unit Interface (W2) can carry
for W6's cross-unit information.

This pays off before any analysis — one decision point instead of scattered
flags, testable and consistent — and provides the slot W11's demand analysis
will populate.

## 3. Design

### 3.1 The annotation

```rust
/// Demand information for a binding, accumulated through the pipeline.
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq, Serialize, Deserialize)]
pub struct Demand {
    /// How many times is this binding used?
    pub cardinality: Cardinality,
    /// Must this binding be evaluated by its context?
    pub strictness: Strictness,
    /// Is this known to already be in WHNF (post-STG-compilation)?
    pub whnf: bool,
}

/// Usage cardinality.
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq, Serialize, Deserialize)]
pub enum Cardinality {
    /// No information yet.
    #[default]
    Unknown,
    /// Used at most once — safe to skip Update (no memoisation benefit).
    AtMostOnce,
    /// Used more than once — memoisation may be worthwhile.
    Multi,
}

/// Strictness status.
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq, Serialize, Deserialize)]
pub enum Strictness {
    /// No information yet.
    #[default]
    Unknown,
    /// Definitely lazy — not necessarily evaluated.
    Lazy,
    /// Definitely strict — will be evaluated by context.
    Strict,
}
```

The lattice is deliberately minimal (two-point for each axis). Richer
demands (head-strict, spine-strict, projection-based) are a post-1.0
refinement (W11 depth).

### 3.2 Where the annotation lives

**On core bindings**: each binding in `Expr::Let` / `Expr::LetRec` carries an
optional `Demand`. This is the primary location — it flows through the core
pipeline and is the slot W11's analysis writes to.

```rust
// In src/core/expr.rs, the binding representation gains a Demand field.
// The exact representation depends on whether bindings are (name, expr)
// pairs or a struct — the annotation is added alongside.
```

**On STG compiler intermediates**: `ProtoLambda`, `ProtoApp`, and
`ProtoAppGroup` currently carry a `single_use: bool` field. This is replaced
by reading the binding's `Demand` annotation. The `is_whnf()` check remains
as a local STG-level refinement (it depends on the compiled form, which is
only known after STG compilation).

### 3.3 The decision point

`take_lambda_form` currently reads three separate sources:

```rust
// BEFORE (current):
if self.single_use() || syntax.is_whnf() || compiler.suppress_updates {
    Ok(dsl::value(syntax))
} else {
    Ok(dsl::thunk(syntax))
}
```

After W9:

```rust
// AFTER:
let demand = self.demand();
if demand.whnf
    || demand.cardinality == Cardinality::AtMostOnce
    || demand.strictness == Strictness::Strict  // future: W11 populates this
    || compiler.suppress_updates
{
    Ok(dsl::value(syntax))
} else {
    Ok(dsl::thunk(syntax))
}
```

Note: `Strictness::Strict` alone does not justify Value today — a strict
binding used multiple times still benefits from memoisation. The correct
rule is: Value when `AtMostOnce` OR `whnf` OR (`Strict` AND `AtMostOnce`).
But `Strict` without cardinality information is not sufficient. For 0.9,
with no analysis populating strictness, this arm is inert — it becomes
active when W11 lands.

Revised decision:

```rust
let demand = self.demand();
let skip_update = demand.whnf
    || demand.cardinality == Cardinality::AtMostOnce
    || compiler.suppress_updates;
if skip_update {
    Ok(dsl::value(syntax))
} else {
    Ok(dsl::thunk(syntax))
}
```

### 3.4 Populators

Each existing mechanism becomes a populator that sets fields on the `Demand`
annotation. The populators run at different pipeline stages:

**Core level (before STG compilation):**

| Populator | Sets | When |
|-----------|------|------|
| Binding usage count | `cardinality` | During inline/eliminate (existing dead-code analysis already counts uses) |
| `strict_args` | `strictness` on argument bindings | When compiling an intrinsic application — arguments listed in `strict_args` get `Strict` |
| W11 analysis (future) | `cardinality` + `strictness` | After inline/fuse, before STG compile |

**STG level (during compilation):**

| Populator | Sets | When |
|-----------|------|------|
| `is_whnf()` | `whnf` | After compiling the binding's body to `StgSyn` |
| Let-body position | `cardinality: AtMostOnce` | A let body is evaluated exactly once (already recognised at `compiler.rs:1221`) |

### 3.5 What stays unchanged

Two mechanisms are **not subsumed** in 0.9:

**`suppress_update` Case flag + `single_use_args`**: these handle the IF
tail-recursion problem at runtime. The compile-time `single_use_args` only
fires when IF is called as `Expr::Intrinsic` directly; user-written
`if(c, t, e)` goes through a bound variable and the compiler cannot know
it's IF. The runtime `suppress_update` flag catches this common case by
suppressing Update dynamically in the VM.

Replacing these requires W11's inter-procedural analysis to propagate
`cardinality: AtMostOnce` through bound variables to call sites — tracking
that `if` is bound to `IF` and therefore its branches are single-use. Until
then, the runtime flag stays.

**Global `suppress_updates`**: remains as a debugging escape hatch. Not
exposed via CLI.

### 3.6 The `single_use` field removal

The `single_use: bool` field on `ProtoLambda`, `ProtoApp`, and
`ProtoAppGroup` is replaced by reading `Demand.cardinality` from the
binding's annotation. This is the concrete simplification W9 delivers:
three struct fields and their threading through the compiler collapse into
one annotation read.

### 3.7 Unit Interface strictness signatures

The `Demand` annotation on an exported binding is a **strictness signature**
— a compact summary of what the exporting unit knows about how the binding
should be treated. The `UnitInterface` gains a field:

```rust
pub struct UnitInterface {
    // ... existing fields ...
    
    /// Demand annotations for exported bindings.
    pub demands: HashMap<String, Demand>,
}
```

This is consumed by W6's pre-compiled prelude blob and by the user-code
compiler (a missing signature costs only optimisation, never correctness).

Serde derives on `Demand`, `Cardinality`, and `Strictness` (already shown
in §3.1) support serialisation into the prelude blob.

### 3.8 Soundness

The demand annotation is **conservative by default**: `Unknown` cardinality
and `Unknown` strictness produce a Thunk (the safe choice). A wrong
`AtMostOnce` changes only cost (O(1) → O(k) if the binding is actually used
multiple times), never results — for a pure language, the worst case is
redundant re-evaluation, not incorrect output.

This one-directional soundness property means the first implementation can
be conservative (when in doubt, `Unknown`) and gradually improve coverage
as W11 lands.

## 4. Implementation sketch

### Phase 1: Define the annotation
- Add `Demand`, `Cardinality`, `Strictness` types with serde derives.
- New module `src/core/demand.rs` (~50 lines).

### Phase 2: Attach to core bindings
- Add an optional `Demand` field to the binding representation in
  `src/core/expr.rs`.
- Default to `Demand::default()` (all `Unknown`).
- Ensure the field survives through desugar, cook, eliminate, inline, fuse.

### Phase 3: Populate from existing heuristics
- **Let-body**: mark as `cardinality: AtMostOnce` (already recognised in
  compiler).
- **`strict_args`**: when compiling an intrinsic application, set
  `strictness: Strict` on argument bindings listed in `strict_args`.
- **Usage counting**: if the existing eliminate/inline passes already count
  uses, feed single-use bindings as `cardinality: AtMostOnce`.

### Phase 4: Refactor the STG compiler
- Remove `single_use: bool` from `ProtoLambda`, `ProtoApp`,
  `ProtoAppGroup`.
- `take_lambda_form` reads `Demand` from the binding annotation +
  `is_whnf()` from the compiled `StgSyn`.
- Thread the `Demand` from core bindings into the STG compiler's
  `ProtoLambda` construction.

### Phase 5: Unit Interface integration
- Add `demands: HashMap<String, Demand>` to `UnitInterface`.
- Populate during interface extraction.
- Add to `PreludeBlob` serialisation (W6).

### Phase 6: Validation
- Full harness — identical output (the refactoring must be behaviour-
  preserving).
- Verify at least one binding that was previously Thunk is now Value
  due to the unified annotation (confirming the heuristics compose
  correctly).
- `EU_STACK_DIAG=1` on tail-recursive conditionals — stack depth must
  not regress (the `suppress_update` mechanism is still active).
- Benchmark: no allocation regression on `001_naive_fib`,
  `002_thunk_updates`, `008_long_lived_graph`.

## 5. Test plan

- **Behaviour preservation**: full harness and conformance corpus output
  identical before and after refactoring.
- **Value/Thunk decision**: add a diagnostic mode (env var or dump flag)
  that logs each binding's `Demand` and the resulting `LambdaForm` choice.
  Verify on a representative set of tests.
- **Let-body optimisation**: a let body is compiled as Value (not Thunk).
- **Strict args**: intrinsic arguments listed in `strict_args` are compiled
  as Value when the intrinsic is called directly.
- **Single-use bindings**: a binding used exactly once is compiled as Value.
- **Tail recursion**: `countdown(100000)` (and similar) shows flat stack
  depth under `EU_STACK_DIAG=1`.
- **GC verification**: full harness under `EU_GC_VERIFY=2` / `EU_GC_POISON=1`.
- **No allocation regression**: benchmark suite shows no increase in
  allocation count.

## 6. Dependencies

- **W6 (pre-compiled prelude)**: the `Demand` annotation is included in the
  prelude blob's `UnitInterface`. W9 can land before or after W6 — if before,
  the annotation is computed at runtime; if after, it's serialised.
- **W11 (strictness analysis)**: W9 provides the annotation W11 populates.
  W9 is the prerequisite; W11 is the payoff.

## 7. Risks

- **Binding representation change**: adding a field to every core binding
  increases memory usage slightly. Mitigate: `Demand` is 3 bytes (could be
  packed to 1 byte with bit fields). Use `Option<Demand>` to avoid overhead
  for the majority of bindings that stay `Unknown`.
- **Compiler refactoring scope**: removing `single_use` from three
  intermediate types touches many lines in `compiler.rs`. Mitigate: the
  change is mechanical — replace field reads with annotation reads.
- **Incomplete populator coverage**: some existing `single_use` sites may
  not have an obvious mapping to a core-level annotation (they may be
  set during STG compilation based on structural properties). Mitigate:
  the STG compiler can set `whnf` and `cardinality` on the `Demand` during
  compilation, not only from the core annotation.

## 8. Success criteria

- **One decision point**: `take_lambda_form` consults `Demand` + `is_whnf()`
  + global flag. The `single_use: bool` field is removed from all compiler
  intermediates.
- **At least one `single_use` override subsumed**: a concrete case where
  the old `single_use` field is now handled by the annotation.
- **No harness regression**: identical output, no allocation increase.
- **Flat stack on tail recursion**: `suppress_update` still active, stack
  depth unchanged.
- **Strictness signature in UnitInterface**: exported bindings carry their
  demand annotation.
