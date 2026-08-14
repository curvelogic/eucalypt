# CG5 assessment — type-gated codegen (eu-u9xj.5) for the 0.15 scope decision

> Owner-accepted 2026-08-11. Produced for the ROADMAP §9 re-dating decision.

Date: 2026-08-11. Assessor: Fable assessment agent. Read-only; probes run against
the release build at `/Users/greg/dev/curvelogic/eucalypt-worktrees/furnace-oufc/target/release/eu`
(commit 991c21d5, post HeapSyn-deletion). All figures **measured-single**, load
average ~4.2 on a shared machine with other agents active — indicative only.

## 1. What CG5 gates on, and its reliability today

The ROADMAP mechanism (§9, lines 546–577): the checker emits a node-keyed
side-table of synthesised types with a `Synthesised`/`Trusted` provenance bit;
the STG compiler consults it and fires only on `Synthesised`-concrete facts.
Three structural facts say this substrate does not exist and is further away
than the ROADMAP assumed:

**(a) No side-table, no provenance bit.** `grep Synthesised|provenance` over
`src/` finds nothing type-related. The checker's outputs
(`TypeCheckResult`, `src/core/typecheck/check.rs:3464`) are: warnings, a
flat name→Type env, Smid-keyed *lambda param* types (for LSP hover), and
aliases. Per-expression types are computed transiently in `synthesise()` and
discarded. The only side-table precedent is demand analysis
(`src/core/analyse_demand.rs`), keyed by raw `Rc` pointer and computed *after*
inlining, immediately before STG compile.

**(b) The checker and the evaluator run separate pipelines.** On every `eu`
run, type checking is a parallel load/translate/cook with its own
`SourceLoader` and its own `SourceMap` (`src/bin/eu.rs:170–245`,
`run_type_checker` / `run_type_checker_from_blob_core` in
`src/driver/check.rs:476`). The eval pipeline's core expression is never the
tree the checker saw. The only cross-link today is Smid→source-coordinate
resolution for warning links (eu-1tkk.7.40). Threading per-node facts to the
compiler means either merging the checker into the eval pipeline (pre-inline,
then surviving inline/prune/reflatten via Smid keys, with Smid duplication
under inlining to handle) or a two-source-map translation layer. This is
significant plumbing that precedes any optimisation.

**(c) Synthesis reach is literals-only.** Empirical probes (files in this
scratchpad, `probe1–10.eu`):

| Probe | Warned? |
|---|---|
| `str.split-on(",", 42)` — literal arg | yes |
| `d: {a: 42} … d.a` — literal-block field | yes |
| `` ` { type: s"number" } n: "oops"`` — annotation mismatch | yes |
| `[1,2,3] str.split-on(",")` — list literal | yes |
| `1 + "hello"` — operator misuse | **silent** |
| `v: 1 + 2 … str.split-on(",", v)` — operator result | **silent** |
| `f(n): 42 … str.split-on(",", f(1))` — user fn result | **silent** |
| `[1,2,3] map(_ * 2) head` — through prelude HOF | **silent** |

Operator applications are handled as warnings-only *constraints*
(`discharge_constraint`, check.rs:610) matched by gradual *consistency*
(`is_consistent`) against union overloads (`+` is
`number → number → number | array → array → array`, prelude.eu:659). No
concrete result type is ever synthesised for an operator application, and
unannotated user functions get no inferred signature. So on realistic
unannotated code (AoC corpus — where the checker is "quiet": 0 warnings, i.e.
it also *proves* nothing), the set of interior expressions carrying a
`Synthesised` concrete numeric type is approximately empty. Even
`fib`'s `n - 1` sites would not fire (`n` is unknown).

The soundness bar makes this worse than "little win": ROADMAP §4.2 is explicit
that a wrong gate is memory corruption, not a type error. A checker whose
operator layer silently passes `1 + "hello"` today is not at the assurance
level where its positive claims should license a memory-unsafe fast path.
Consistency-based (gradual) reasoning is structurally the wrong basis for a
gate; CG5 needs a separate precise-subtyping judgement plus the provenance
machinery, none of which exists.

## 2. Per-win analysis

### (a) Unboxing / direct intrinsic dispatch — largely eaten by FusedPrimop

The CG5 target as written ("`+` compiles to a tag-`case` wrapper…skip the
tag-`case`") describes the pre-July world. **eu-9mvh closed it type-free**:
`Op::FusedPrimop` (PR #980) + direct-inline fusion (PR #982) collapsed the
case-of-case `binary_wrapper` tree on hot paths — fib went 115.78M→88.85M
ticks, −33 % wall, bc/hs 1.96×→1.265× (bead close note, coordinator-verified).

What remains per primop today (`src/eval/bytecode/machine.rs:1221–1372`):
- two operand-forcing continuations (`FusedPrimopLeft/Right`) — required by
  laziness; types cannot remove them (that is demand analysis's job, and a
  `Synthesised number` does not imply WHNF);
- a boxed-vs-native tag classification (`fused_boxed_field`) — a cheap,
  well-predicted branch;
- a scratch `BcEnvFrame` heap allocation per dispatch (`finish_fused_primop`)
  — real cost, but removable **type-free** (a register-pair or dedicated
  Bif-operand slot needs no type facts);
- inside `Add::execute` (`src/eval/stg/arith.rs:88`): `num_arg` re-checks and
  the i64/u64/f64 ladder — nanoseconds per op.

Numbers are already unboxed at value level: `Native::Num(serde_json::Number)`
is carried inline in `BcValue::Native`; `BoxedNumber` appears only where a
native must stand as a closure. True unboxed arithmetic — i64 in a register
slot, no `Native` wrapper, no continuation traffic — requires BV3 register
frames (eu-2sa6.2, **still OPEN**) plus a typed unboxed calling convention.
That matches the ROADMAP's own caveat ("lands cleanest on BV").

Current profile (measured-single, load ~4.2): `001_naive_fib` = 76.7M ticks,
18.8M allocs, 1.64 s eval — allocation- and call-dominated; the residual
alloc gap is tracked separately (eu-cj1h.2). The type-gated increment on
today's architecture is the num_arg checks and one branch — plausibly
low single-digit % on arithmetic-pure code, invisible elsewhere.

### (b) Dead-branch elimination — target mostly gone

The big uniform case tables were the intrinsic wrappers; FusedPrimop removed
them from hot paths. What is left is user/prelude pattern-match `Case` tables,
where pruning a branch needs exactly the synthesis reach that does not exist
(§1c). And a *wrongly* pruned branch is a `NoBranchForDataTag` error on
correct code — a correctness bug, if not a memory-safety one. No measurable
target identified.

### (c) Interpolation specialisation — no workload evidence, and the real win is type-free

Interpolation desugars to `JOIN([STR(chunk), lit, …], "")`
(`src/core/desugar/rowan_ast.rs:2091–2120`): a cons-list build + one `STR`
conversion per interpolated expression + a `JOIN` intrinsic. The type-gated
part (elide `STR` on proven strings) is the smallest slice; `STR` on a string
is already a cheap runtime identity check. The larger wins — fusing the list
build + JOIN into a string-builder opcode, sizing the output buffer — need no
types at all (same pattern as FusedPrimop).

Workload check (measured-single): `013_interpolation` runs in **0.11 ms**
eval / 8,805 ticks; `018_string_scale` in **67 ms** / 2.6M ticks. Both are
below the engine-AB protocol's ~200 ms ratio floor. There is no string/render
benchmark heavy enough to demonstrate a win, which effectively triggers the
ROADMAP's own kill switch ("if real render workloads show <2×… it is
shelved") by absence of evidence.

## 3. Effort & risk

Effort to a *sound* CG5, in order of necessity:
1. **Checker synthesis maturity** — concrete result types at operator
   applications (precise overload selection, not consistency), inference for
   unannotated user functions, instantiation through prelude HOFs. This is
   real inference work well beyond today's bidirectional checker; it is also
   the *only* part with standalone value (it directly closes the `1 + "hello"`
   warning gap).
2. **Pipeline unification + node-keyed type table with provenance** — merge or
   re-run the checker inside the eval pipeline; Smid-keyed facts surviving
   inline/prune/reflatten; `Trusted`-vs-`Synthesised` discipline at every
   `any` crossing. New, correctness-critical plumbing.
3. **BV3 register frames + unboxed calling convention** — the prerequisite for
   the only version of "unboxing" that still has headroom.
4. The optimisations themselves — the smallest part.

Risk: highest in the codebase class. A wrong `Synthesised` fact is memory
corruption (§4.2); the gate must be right 100 % of the time on a checker that
is currently silent on basic operator misuse. Interaction with demand
analysis is a second-order hazard: types must never be read as strictness
(a `Synthesised number` can still be a thunk); the two side-tables would need
clearly disjoint contracts.

## 4. Sequencing

The ROADMAP's own dependency line ("TY default-on … maturing the checker
toward CG's type-gated tier"; "CG type-gated … lands cleanest on BV") has
half-executed: TY default-on shipped, but the maturation it was meant to force
has not reached the operator layer; BV3 has not started. Both of CG5's
prerequisites are missing, while the type-free levers (FusedPrimop; the
scratch-env alloc removal; a JOIN/STR string-builder fusion) have taken or can
take most of the claimed win at zero soundness risk.

## 5. Recommendation

**DEFER — out of 0.15, revisit no earlier than 0.16, behind three named
gates:**
1. **BV3 register frames (eu-2sa6.2) landed** — without them the unboxing win
   is num_arg checks and one branch.
2. **A checker-synthesis milestone**: concrete operator-application result
   types with precise (non-consistency) overload selection, `1 + "hello"`
   warns, and synthesis propagates through at least annotated user functions.
   Worth doing for the warning surface regardless of CG5 — file it as its own
   bead.
3. **A measured string/render workload ≥200 ms** demonstrating interpolation
   cost, or the interpolation claim is dropped per the ROADMAP kill switch.

Extract the **type-free** remainder now (separate candidate beads, cheap and
sound): scratch-env elimination in `finish_fused_primop`, and a JOIN/STR
string-builder fusion for interpolation. These capture most of CG5's
remaining practical value without the gate.

No spike is needed to make this call: the two questions a spike would ask —
"does the checker produce consumable per-node facts?" (no: no table, separate
pipeline) and "does synthesis reach real code?" (no: literals only, probes
above) — are already answered by inspection and measurement.
