# 0006 — Strictness & demand analysis (cut thunk churn)

- **Status:** Draft proposal for review
- **Track:** B — performance, runtime & concurrency
- **Classification:** Whitespace
- **Suggested horizon:** 0.9
- **Related:** H11 (type-system-evolution.md §H11), ADR-001, sibling proposals
  [0005 — generational nursery & GC](0005-generational-gc.md),
  [0007 — type-directed compilation](0007-type-directed-compilation.md)

---

## Summary

Eucalypt's STG machine allocates a heap closure for nearly every let-binding:
when a binding is not provably in WHNF and not provably single-use, the
compiler emits a `Thunk` `LambdaForm`, which the VM blackholes, evaluates, and
updates in place.  The infrastructure to avoid this — `LambdaForm::Value`,
`single_use`, `suppress_updates` — already exists and is already exploited in a
handful of hand-coded hot-paths, demonstrating the payoff.  What is missing is
a *systematic analysis* that identifies all bindings safe to compile as
`Value`.  A demand/strictness analysis pass over core expressions would extend
this coverage automatically, reducing thunk allocation, update-continuation
push/pop, GC mark pressure, and `Update` stack growth — all in one stroke.  The
result connects directly to 0005 (fewer thunks mean a smaller nursery working
set) and is partly superseded, for annotated code, by 0007 (type annotations
already imply strictness in their arguments).

---

## Motivation

### The thunk lifecycle cost

Every `let`-binding in eucalypt core that reaches the STG compiler without a
WHNF witness is compiled as a `Thunk` `LambdaForm`
(`src/eval/stg/syntax.rs:213–225`).  At runtime the VM:

1. Allocates an `EnvFrame` entry for the closure (heap pressure).
2. On first entry, overwrites the slot with a `BlackHole` to catch cycles
   (`src/eval/machine/vm.rs:434–441`).
3. Pushes an `Update` continuation (`src/eval/machine/vm.rs:443–449`).
4. Evaluates the thunk body.
5. Returns through the `Update` continuation, writing the WHNF result back
   into the slot.
6. The original thunk closure becomes unreachable — it is dead on the heap,
   and the GC mark phase must traverse and reclaim it.

Steps 1–6 happen on *every* binding unless the compiler can prove the binding
is either already in WHNF (`StgSyn::is_whnf`, `src/eval/stg/syntax.rs:143`)
or used at most once (the `single_use` flag,
`src/eval/stg/compiler.rs:499–517`).

In traversal-heavy workloads — list maps, block transforms, recursive
conditionals — the mark phase dominates VM time.  Every unnecessary thunk kept
on the heap adds a reachable object to the mark queue.  The GC benchmark
(`benches/gc.rs`) already measures mark cost directly through the
`gc_collect_with_survivors` family.

One allocation site has already been addressed: as of 0.6.2, arithmetic
operations return native atoms directly rather than boxing the result
(`CHANGELOG.md: "Arithmetic native returns"`), saving one heap allocation per
arithmetic result.  This is a concrete partial win on the same allocation
pressure described here; the broader opportunity — compile let-bindings as
`Value` rather than `Thunk` wherever the demand analysis permits — remains
open and is what this proposal addresses.

### The existing hand-rolled heuristics

The codebase already has two partial escape valves, each addressing a specific
observed pain point:

**`single_use` on let bodies and strict args.**  The
`ProtoSyntax::take_lambda_form` method (`src/eval/stg/compiler.rs:506–517`)
checks `self.single_use()` before deciding whether to emit a `Thunk` or a
`Value`.  Let-body expressions are always compiled as `single_use: true`
(`src/eval/stg/compiler.rs:1205–1209`) because a let body is evaluated exactly
once.  Strict intrinsic arguments are likewise marked single-use when the
corresponding `Intrinsic::strict_args` list includes their position
(`src/eval/stg/compiler.rs:943`).

**`single_use_args` / `suppress_update` for the `IF` intrinsic.**  The
`StgIntrinsic` trait exposes `single_use_args`
(`src/eval/machine/intrinsic.rs:119–126`).  `If` overrides it to mark its
then- and else-branches (indices 1 and 2) as single-use
(`src/eval/stg/boolean.rs:143–145`).  The `IF` wrapper also emits a
`switch_suppress` case expression (`src/eval/stg/boolean.rs:150`) whose
`suppress_update: true` field instructs the VM to skip the `Update` push when
entering a branch whose body is a bare local atom
(`src/eval/machine/vm.rs:273–279`, `src/eval/stg/syntax.rs:95–99`).  Without
this, tail-recursive conditional loops such as
`countdown(n) = if(n = 0, 0, countdown(n-1))` accumulate an O(N) stack of
`Update` continuations — one per iteration — before any can be popped.

**`suppress_updates` global flag.**  The `Compiler` struct carries a
`suppress_updates: bool` field (`src/eval/stg/compiler.rs:483`).  When set, it
forces *every* binding to be compiled as `Value`, effectively switching from
call-by-need to call-by-name (`src/eval/stg/mod.rs:291`).  This exists as a
debugging/experimentation knob, not a correctness-preserving optimisation: it
changes semantics if any binding is shared (evaluated more than once), turning
`O(1)` memoised computation into `O(k)` re-evaluation.  It cannot be used in
production.

These three mechanisms cover known hot-paths but rely entirely on the
*programmer or intrinsic author* knowing in advance which arguments are
single-use or strict.  There is no pass that discovers this information from
the program text.

---

## Prior art & landscape

### GHC demand analysis

The canonical reference is Sergey, Vytiniotis, and Peyton Jones, *"Modular,
Higher-Order Cardinality Analysis in Theory and Practice"* (POPL 2014; draft at
simon.peytonjones.org), which extends the earlier *"Theory and Practice of
Demand Analysis in Haskell"* (JFP draft, Microsoft Research 2017,
<https://www.microsoft.com/en-us/research/publication/theory-practice-demand-analysis-haskell/>).

GHC's demand analyser is a backward abstract interpretation: it propagates a
*demand* lattice from results back to arguments.  A demand carries two
components:

- **Strictness** — is the binding definitely evaluated?  If yes, no thunk is
  needed.
- **Cardinality (usage)** — is the binding evaluated at most once?  If yes, no
  `Update` continuation is needed (call-by-name suffices for that binding).

The worker/wrapper transformation then exploits these results: strict arguments
can be unboxed at call sites; once-used thunks can be replaced with ordinary
lambdas.  GHC's experience is unambiguous: this is one of the highest-return
optimisations in the pipeline for a lazy language.

### Projection-based strictness (Wadler–Hughes)

Wadler and Hughes, *"Projections for Strictness Analysis"* (FPCA 1987,
<https://link.springer.com/chapter/10.1007/3-540-18317-5_21>) introduced the
projection framework, which can express finer-grained demands (e.g.,
head-strictness in a list, spine-strictness vs. full-strictness) using
continuous endofunctions on domains.  This handles structured data richer than
flat scalars.  For eucalypt, where most strictness wins are at the scalar
binding level, the simpler two-point (`L ⊑ S`) strictness lattice is likely
sufficient for a first implementation; projection generalisation is a
post-1.0 refinement.

### Peer configuration languages

Peer languages in the configuration/data space do not generally publish
analyses at this level of detail:

- **Jsonnet** (Go/C++ interpreters): eager evaluation by default; no lazy
  semantics to optimise.
- **Dhall**: normalisation-to-beta-normal-form at typecheck time; lazy
  evaluation is not the bottleneck because terms are not turing-complete.
- **Nickel** (Rust, lazy): uses a thunk graph with sharing; no published demand
  analysis.  Eucalypt's situation is closest to Nickel's.
- **CUE / KCL**: constraint-driven; a different execution model; not directly
  comparable.

The lesson from the PLT literature is that for a *pure*, *lazy* language with
*sharing* (i.e. exactly eucalypt's execution model), demand analysis pays
particularly well because: (a) purity means no side effects can invalidate
strictness proofs; (b) sharing means thunks *are* beneficial when genuinely
shared, so the analysis must be sound — it must only skip thunks when the
binding is genuinely used at most once or the value is definitely demanded.
GHC is the only mature system at eucalypt's scale operating under exactly these
constraints, making it the primary design reference.

---

## Proposed design

### Analysis placement

The analysis should operate on **core expressions** (`src/core/expr/`), after
the inline and fuse passes but before STG compilation.  At core level, the
expression tree is in a form that is easy to traverse, scopes are explicit as
`Let`/`LetRec`/`Lam` nodes, and free-variable sets are computable.  Operating
at core rather than STG avoids re-implementing scope bookkeeping in two layers.

### Demand lattice

A minimal two-point strictness lattice suffices for a first pass:

| Demand | Meaning |
|--------|---------|
| `L` (lazy) | may or may not be evaluated — must compile as `Thunk` |
| `S` (strict) | definitely evaluated at least once |
| `U1` (used-at-most-once) | evaluated at most once — `Value` is safe even if `L` |
| `SU1` | definitely evaluated exactly once — `Value`, no thunk, no update needed |

A binding is safe to compile as `Value` if its demand is `U1` or `SU1` — i.e.,
the memoisation that a `Thunk` provides is never exploitable because the result
is never shared.

### The backward pass

The analysis is a single bottom-up traversal of each core definition:

1. **Atoms and literals**: demand is `SU1` (evaluated exactly once when
   referenced).
2. **Application `f(a₀, …, aₙ)`**: propagate the *strictness signature* of `f`
   back to the arguments.  For known intrinsics, signatures are already available
   via `Intrinsic::strict_args` — these can be read directly.  For user-defined
   functions, the analysis computes signatures by analysing the body under a
   demand context for each argument position.
3. **`Case` / conditional**: an argument is strict if it is strict in *all*
   branches, lazy otherwise.  (This is the standard join in the lattice.)
4. **`Let`/`LetRec` bindings**: analyse the body under a demand environment;
   the demand on each binding is the demand the body places on it.  If the
   demand is `U1` or `SU1`, mark the binding single-use.
5. **`Lam`**: analyse the body; the demands on the bound variables become the
   function's strictness signature, to be used when callers are analysed.

The analysis is **modular**: each definition is analysed once.  For
mutual recursion (`LetRec`), a fixed-point iteration over the binding group
is needed (bounded by the depth of the lattice, which is 2, so it converges in
at most two passes over the group).

### STG compilation changes

The analysis annotates each let-binding with a `DemandAnnotation` before STG
compilation.  The STG compiler's `compile_binding` function
(`src/eval/stg/compiler.rs:1245–1251`) then consults this annotation: if
the demand is `U1` or `SU1`, it calls `dsl::value(…)` rather than
`dsl::thunk(…)`.  This is a two-line change in `take_lambda_form`
(`src/eval/stg/compiler.rs:512–514`) — the analysis result replaces the
`self.single_use()` heuristic rather than adding to it.

### Safety condition

The analysis must be **sound**: it must only label a binding `U1` if the
program *certainly* evaluates the binding at most once across all execution
paths.  In particular:

- Recursive bindings cannot be `U1` without a termination proof (conservative:
  treat all recursive references as `S`, not `U1`).
- Bindings used in both branches of a conditional are `S` but not `U1` — the
  join of uses in two branches is `max(|branch₁|, |branch₂|)`, not their sum.
- Bindings used inside a lambda body must be treated as potentially multi-use
  (the lambda may be called many times).

Getting this wrong changes semantics: a binding that is genuinely shared would
be re-evaluated on each reference rather than returning the memoised value.
For a pure language this does not change *results*, but it can change *cost*
— potentially from O(1) to O(k) for work that the programmer expected to share.
The first implementation should be conservative: when in doubt, emit a `Thunk`.

### Subsuming the hand-coded heuristics

Once the analysis is in place:

- `single_use_args` on `StgIntrinsic` becomes unnecessary for intrinsics whose
  signatures can be derived from the analysis (though it can serve as a
  cross-check during transition).
- The `suppress_update` / `suppress_next_update` story in the VM
  (`src/eval/machine/vm.rs:270–276`) is no longer a *workaround* for O(N)
  update accumulation in tail-recursive conditionals — instead, those bindings
  are simply compiled as `Value` and the accumulation problem does not arise.
- The `suppress_updates` global flag remains useful as a correctness-stress
  mode and a debugging tool, but is no longer load-bearing.

---

## Interaction with the existing roadmap

**0005 — generational GC.**  Fewer thunks means a smaller nursery live set:
objects promoted from a generational nursery are typically those that survive
their first GC.  If bindings that would previously have been short-lived thunks
are compiled as `Value` closures (or eliminated entirely), the nursery survives
fewer objects and the promotion rate falls.  The proposals compound: 0005 makes
GC cheaper per collection; 0006 reduces the number of objects that need
collecting.

**0007 — type-directed compilation (H11).**  Type annotations in the form
`number -> number` already encode strictness in the typed arguments.  An
inferred type `T -> U` for a user function says that every call is strict in
its argument.  The type-directed compilation proposal (H11a, H11f) will exploit
this for unboxing and direct dispatch.  This proposal covers the *complementary*
case: unannotated code, where the type checker has not been invoked or has not
produced a type for a binding.  The two analyses therefore cover the full space
with no overlap: 0007 applies to annotated/typed code; 0006 applies to
everything.  When 0007 is implemented, its output can be *used as an input* to
the demand analysis — a type annotation that says `T -> U` is a strictness
signature that the analysis can trust without inspecting the function body.

**H11 (type-system-evolution.md §H11).**  The H11 family of type-directed
optimisations is predicated on types being available and trusted.  Demand
analysis is type-independent: it works on unannotated eucalypt code, which is
the common case for configuration scripts and the prelude.  The two should be
scheduled sequentially: demand analysis in 0.9, type-directed compilation
post-1.0 as per the README portfolio.

---

## Implementation sketch

### Phase 1 — Analysis infrastructure (small)

- Add a `DemandAnnotation` enum (or extend the `Expr` metadata map) in
  `src/core/expr/`.
- Implement the backward analysis as a new module `src/core/analyse/demand.rs`,
  ~300–400 lines.
- Wire it into the pipeline in `src/driver/eval.rs` between the fuse pass and
  the STG compiler call.

### Phase 2 — Compiler integration (tiny)

- Modify `ProtoSyntax::take_lambda_form` in `src/eval/stg/compiler.rs:506–517`
  to consult the annotation.
- Retire the `single_use` fields on `ProtoApp`, `ProtoInline`, and
  `ProtoAppGroup` in favour of the analysis result (or keep them as overrides
  for hand-written intrinsic wrappers).

### Phase 3 — LetRec fixed-point (medium)

- Extend the analysis to handle `LetRec` groups with a two-pass fixed-point
  iteration.
- Add test cases covering mutually recursive bindings.

### Phase 4 — Intrinsic signature cross-check (small)

- Audit `single_use_args` overrides on all `StgIntrinsic` implementations
  against the analysis.  Where they agree, remove the override.  Where they
  disagree, investigate.

### Risk and size

The analysis itself is ~400 lines of straightforward Rust.  The STG compiler
change is ~10 lines.  The main risk is in Phase 3: fixed-point analysis of
recursive groups can be tricky to get right.  The conservative safe-default
(treat all recursively-referenced bindings as `S`) means that an incorrect
analysis can only *fail to optimise*, never introduce an unsoundness visible to
the user.  This makes the implementation relatively low-risk.

---

## Alternatives considered

**Inliner-only approach.**  Aggressive inlining (already present in
`src/core/inline/`) can eliminate some bindings entirely, removing the
thunk question.  But inlining has its own cost: code size growth, loss of
sharing for genuinely shared subexpressions.  Demand analysis is orthogonal
and cheaper than more aggressive inlining.

**Whole-program analysis.**  Rather than a modular per-definition analysis,
a whole-program pass could propagate demands across call sites with more
precision.  This is what GHC does at higher optimisation levels.  For
eucalypt's use case (configuration scripts of hundreds to low thousands of
lines), the precision gain is unlikely to justify the implementation cost or
the compile-time increase.  Modular analysis is sufficient.

**Type-only approach (wait for 0007).**  Type annotations already encode
argument strictness for annotated functions.  One could argue: wait for 0007
and derive strictness from types.  The counterargument is that the vast majority
of eucalypt code is unannotated and will remain so — the prelude is being
annotated incrementally, user scripts essentially never are.  A type-only
approach would capture a small fraction of the available wins.

**Extend `suppress_updates` to partial mode.**  Making `suppress_updates` a
per-binding flag rather than a global would achieve similar goals without a
proper analysis.  This was essentially the `single_use` approach: it works for
known sites, fails to discover new ones.

---

## Risks & what would kill this

**Soundness errors.** The most dangerous failure mode: a binding is incorrectly
labelled `U1` but is actually evaluated more than once.  For a pure language,
the *result* is unchanged (purity guarantees referential transparency), but
performance regresses badly if expensive work is duplicated.  Mitigation:
start conservatively, add property-test coverage that checks analysis results
against a `suppress_updates=true` vs. default run for a large body of test
programs.

**Fixed-point non-termination.** Mutual recursion analysis requires fixed-point
iteration.  The two-point lattice guarantees convergence in at most two
iterations per binding group, but the implementation must handle it correctly.
The eucalypt test harness (`tests/harness/`) provides a large corpus for
regression testing.

**Compile-time cost.** The analysis is a linear pass (modulo LetRec groups,
which iterate at most twice).  Compile latency is already ~500–700 ms;
a 400-line backward pass adds a few milliseconds at most and is not a concern.

**Interaction with the metadata channel.** Eucalypt's `Meta`/`DeMeta` nodes
can wrap any expression.  The analysis must treat metadata extraction
(`DeMeta`) conservatively — if a binding is wrapped in `Meta`, the wrapper's
demand may not be the same as the body's demand.  A conservative choice (mark
the body as `L` unless proven otherwise) is correct.

---

## Success criteria

1. **Fewer thunk allocations in benchmark runs.**  Measure
   `metrics.alloc(bindings.len())` in `vm.rs:501` (currently totalled as
   allocation count) before and after.  A 20 % reduction in allocation count
   on representative corpus programs (prelude-heavy block transforms) is the
   target.

2. **GC mark time reduction.**  The `gc_collect_with_survivors` benchmark
   (`benches/gc.rs:70–96`) measures mark cost as a function of survivor count.
   Fewer live thunks at GC time → lower mark cost.  Target: measurable
   improvement on configurations with high survivor rates.

3. **No regression on the harness test suite.**  All 50+ tests in
   `tests/harness/` must pass with the analysis enabled, including
   error-condition and edge-case tests.

4. **`single_use_args` override count reduced.**  At least one
   `StgIntrinsic::single_use_args` override is correctly subsumed and removed,
   demonstrating that the analysis covers what the hand-coding was doing.

5. **No O(N) update accumulation on tail-recursive conditionals.**  The
   `IF`/`suppress_update` mechanism addressed an observed stack-overflow class
   of bug.  After the analysis, a countdown benchmark should show flat stack
   depth under `EU_STACK_DIAG=1`, with no `suppress_update` flag required.

---

## References

- `src/eval/stg/syntax.rs:213–225` — `LambdaForm` variants (`Thunk`, `Value`,
  `Lambda`); `src/eval/stg/syntax.rs:143` — `StgSyn::is_whnf`.
- `src/eval/stg/compiler.rs:477–538` — `Compiler`, `ProtoSyntax`,
  `take_lambda_form`, `single_use` decision.
- `src/eval/stg/compiler.rs:924–952` — `single_use_args` propagation for
  intrinsic argument compilation.
- `src/eval/machine/intrinsic.rs:119–126` — `StgIntrinsic::single_use_args`.
- `src/eval/stg/boolean.rs:133–159` — `If` intrinsic: `single_use_args`
  override and `switch_suppress` wrapper.
- `src/eval/machine/vm.rs:273–279` — `suppress_next_update` field: rationale
  and VM-level suppression of `Update` push.
- `src/eval/machine/vm.rs:409–449` — Thunk entry: blackholing and `Update`
  continuation push.
- `src/eval/stg/mod.rs:286–305` — `StgSettings::suppress_updates`.
- `benches/gc.rs` — GC mark cost benchmarks.
- Sergey, Vytiniotis, Peyton Jones. *"Modular, Higher-Order Cardinality
  Analysis in Theory and Practice."* POPL 2014.
  <https://simon.peytonjones.org/assets/pdfs/modular-higher-order-cardinality.pdf>
- Peyton Jones et al. *"Theory and Practice of Demand Analysis in Haskell."*
  JFP draft, Microsoft Research.
  <https://www.microsoft.com/en-us/research/publication/theory-practice-demand-analysis-haskell/>
- Wadler, Hughes. *"Projections for Strictness Analysis."* FPCA 1987.
  <https://link.springer.com/chapter/10.1007/3-540-18317-5_21>
