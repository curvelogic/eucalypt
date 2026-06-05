# 0007 — Type-directed compilation (types as a runtime asset)

- **Status:** Draft proposal for review
- **Track:** B — performance, runtime & concurrency
- **Classification:** Stage-C-Fork
- **Suggested horizon:** post-1.0
- **Related:** H11 (type-system-evolution.md §1, §4), open question 4 (§5); sibling proposals [0002 — gradual-typing boundary policy](0002-gradual-typing-boundary-policy.md), [0006 — strictness analysis](0006-strictness-analysis.md), [0005 — generational GC](0005-generational-gc.md)

## Summary

The type checker is, today, **purely advisory**: it runs after `prepare`,
emits warnings, and is discarded — the same untyped core proceeds to STG
compilation unchanged (`src/bin/eu.rs:123-146`). Eucalypt therefore pays the
full cost of designing, specifying and implementing a gradual type system and
banks **none** of its runtime value. This proposal argues for closing that gap:
**use** the synthesised types to drive code generation. **Coverage depends on the
workload.** Eucalypt's primary use — *generating* JSON/YAML — works with
eucalypt-native data (literals, interpolations, computed blocks) that the checker
*synthesises concrete types for*, so the optimisations have real coverage there;
the *import-transform* case is `any`-dominated and depends on contracts
([0009](0009-structural-contracts-validation.md)) concretising data at ingress.
And the wins are honestly an **aggregate of modest dispatch/branch eliminations**
for generation — typed-intrinsic dispatch (H11f), literal-symbol branch folding
(H11e), and specialising the per-interpolation value→string dispatch — with
numeric unboxing (H11a, the ~5–10× headline) the *narrower* case, since generation
is string/block-heavy, not arithmetic-heavy. This is what converts the type system from a correctness
aid into a performance asset, and it is the strategic answer to "why invest in
types at all" for a rendering tool. It is a **Stage-C fork**: large, post-1.0,
and gated on two maintainer decisions — whether runtime performance matters
(open question 4) and what the boundary policy is ([0002](0002-gradual-typing-boundary-policy.md)).

## Motivation

### The erased-types reality

The pipeline orders type-checking *after* all core transformation and *before*
nothing that consumes its output. In `src/bin/eu.rs`, `prepare::prepare`
(`:112`) produces the final core expression; `--type-check` (`:123`) then clones
that expression, calls `type_check(&core_expr)` (`:126`), prints the resulting
`TypeWarning`s (`:129-132`), and — crucially — `eval::run(&opt, loader)` (`:146`)
runs on the *same loader* with the evaluand untouched. Inside the executor,
`stg::compile(stg_settings, self.evaluand.clone(), rt)` (`src/driver/eval.rs`,
in `try_execute`) receives core that carries no type information at all. Types
never cross into STG. They are erased — not even erased *late*, but never
admitted.

That means every numeric and structural operation pays for dynamic dispatch it
may not need. Consider `+`. The prelude binds it to the `ADD` intrinsic
(`lib/prelude.eu:644`: `(l + r): __ADD(l, r)`; intrinsic name at
`src/eval/intrinsics.rs:123`). Its STG wrapper, `binary_wrapper`
(`src/eval/stg/arith.rs:452`), is a nested `case` dispatching on each
argument's boxing tag — `BoxedNumber`, `BoxedString`, `BoxedSymbol`, `BoxedZdt`
— with a boxed path (force/unbox) and a native/native fast path that skips
unpacking when both operands are already unboxed. Even on the fast path the
dispatch overhead is unavoidable: the `case` itself must be evaluated. The
intrinsic body (`Add::execute`, `:88-138`) then re-checks at runtime: resolves
each operand to a `Native`, tests `matches!(a, Native::NdArray(_))`, and falls
through `as_i64` → `as_u64` → `as_f64`. For an expression the checker has
*already proven* is `number -> number -> number`, all of this — the `case`
dispatch, the `NdArray` test, the numeric ladder — is redundant. The STG already
has the unboxed target representation, `Native::Num(Number)` alongside `Sym`,
`Str`, `Zdt`, `NdArray` (`src/eval/memory/syntax.rs:37-51`). What is missing is
*permission*, derived from a type, to take the direct path.

The same shape recurs for IO: a `{ :io … }` block desugars to a chain of
`IO_BIND` constructors on the heap (`src/eval/stg/io.rs:47-65`), which the driver
walks one node at a time (`src/driver/io_run.rs:1217-1244`). Every step allocates
a constructor that exists only to be interpreted away.

And — the case that matters most for *generation* — every `"{x}"` interpolation
converts its value to a string through a runtime `match` on the `Native` tag
(`src/eval/stg/string.rs:125-135`: `Sym → resolve`, `Str → as_str`, `Num →
format!`, …). A synthesised type for `x` could compile straight to the selected
arm, skipping the dispatch. The win per interpolation is modest — the `format!`
and the string allocation it guards dominate and remain — but templating is
interpolation-*dense*, so this is the most *pervasive* (if not the largest)
type-directed win in generation.

### The opportunities (H11 sub-items)

The evolution doc's H11 enumerates six candidates. The recommended package
(§1, H11 recommendation) is **a, c, e, f**; **b and d are dropped**. Walking
them honestly:

| Item | What it does | Verdict |
|---|---|---|
| **H11a** unboxing | Compile a proven `number`-typed pipeline to a primitive-arithmetic path skipping box/force/dispatch. STG already has `Native` (`syntax.rs:37`). | **Take, but narrow.** ~5–10× on tight arithmetic — yet generation is string/block-heavy, so this is the *least*-covered win, not the lead. |
| **H11f** direct intrinsic dispatch | A typed `+`/`<`/`min`/`max`/`length` compiles straight to its intrinsic with the tag `case` of `binary_wrapper` removed. As of 0.7.0, `min` and `max` carry structural operator constraints (`"<(a, a) => a → a → a"`, `">(a, a) => a → a → a"`, `lib/prelude.eu:916,891`), expanding the set of annotated numeric targets for this optimisation. | **Take.** Cheapest; complements H11a (unboxing is what makes the BIF call legal without re-checking). |
| **H11c** IO flattening | Collapse a statically-all-`IO(T)` bind-chain into a direct-style tagged sequence, cutting one heap constructor per step (`io.rs`, `io_run.rs`). | **Take.** Allocation win on IO-heavy renders; ties to [0005](0005-generational-gc.md). |
| **H11e** dead-branch elimination | `if(x = :foo, A, B)` with `x : LiteralSymbol(:foo)` reduces to `A`. | **Take.** Nearly free; synergises with literal types (H16). Must key on the *brancher set* (`if`/`then`/`cond`/`‖`), not the name `if` — see H15's "branching is not syntax". |
| **(new)** interpolation conversion | Specialise the per-`"{x}"` value→string dispatch (`string.rs:125`) to the arm the synthesised type selects. | **Take.** *Most pervasive* generation win (templating is interpolation-dense), though modest per instance — `format!`/alloc dominates. |
| **H11b** lens fusion | Compose `at(:a) ∘ at(:b)` to a direct two-step access instead of allocating an intermediate metadata-carrying function. | **Drop (defer).** Elegant, but lens-heavy code is rare; the doc parks it. |
| **H11d** block field offset | Compile `b.x` on a known closed record to a fixed-offset read. | **Drop.** Needs a parallel typed-block representation; most eucalypt blocks are dynamically constructed cons-lists (ADR-001), so the runtime fork rarely pays. |

The reason **b and d are dropped** is the same in both cases: their payoff is
gated on a usage pattern (statically-composed optics; statically-shaped blocks)
that is *not* how typical eucalypt is written, whereas a/c/e/f hit the hot paths
of *every* numeric or IO program. We adopt the doc's recommendation unchanged.

## Prior art & landscape

Type-directed compilation is the central idea behind several optimising
functional compilers; the question is what transfers to a **gradual,
structural, lazy** setting.

- **GHC — strictness/demand analysis + worker/wrapper, and SpecConstr.** GHC's
  demand analyser proves which arguments are evaluated, then worker/wrapper
  unboxes those arguments and exposes an unboxed calling convention to a
  specialised worker that inlines at every call site
  ([GHC user's guide §5.3](https://downloads.haskell.org/ghc/latest/docs/users_guide/using-optimisation.html)).
  SpecConstr specialises a function for the constructor shapes it is called
  with. The lesson for eucalypt: **unboxing is a strictness story as much as a
  type story** — you may only pass a value unboxed if it is definitely
  evaluated. That binds H11a to [0006 — strictness analysis](0006-strictness-analysis.md):
  a `number` annotation gives the *representation*, demand analysis gives the
  *permission*.
- **MLton — whole-program monomorphisation + unboxing + representation
  selection.** MLton defunctorises and monomorphises down to a simply-typed
  first-order IR, then unboxes and flattens
  ([Weeks, *Whole-Program Compilation in MLton*, ML 2006](http://www.mlton.org/References.attachments/060916-mlton.pdf)).
  Transferable: *representation selection* (boxed vs unboxed per type) is exactly
  H11a/H11f. Not transferable: closed-world monomorphisation conflicts with
  eucalypt's gradual, separately-imported, `any`-permeated world.
- **Roc — lambda-set specialisation.** Roc threads concrete function values
  through types and unconditionally defunctionalises closures at compile time
  ([Brandon et al., *Better Defunctionalization through Lambda Set Specialization*, ICFP 2023](https://dl.acm.org/doi/10.1145/3591260)).
  Powerful, but predicated on *mandatory, total* inference — Roc has no `any`.
  Eucalypt's gradual checker synthesises a concrete type only *sometimes*; we
  can specialise only where it does.
- **Idris 2 — erasure by quantity.** Every binder carries a quantity; `0`-use
  arguments are erased from the runtime
  ([Brady, *Idris 2*, ECOOP 2021](https://drops.dagstuhl.de/storage/00lipics/lipics-vol194-ecoop2021/LIPIcs.ECOOP.2021.9/LIPIcs.ECOOP.2021.9.pdf)).
  The relevant transfer is conceptual: *types decide what survives to runtime*.
  Eucalypt's analogue is weaker (we erase representation overhead, not values)
  but the discipline — only act on what the type *proves* — is identical.
- **OCaml flambda.** A pass pipeline doing inlining, specialisation and
  unboxing of single-constructor types
  ([OCaml manual, Flambda](https://ocaml.org/manual/5.4/flambda.html)). Closest
  to the pragmatic, opt-in posture we want: an optimisation layer that improves
  idiomatic code without changing its meaning.

**Net:** borrow the *representation-selection* and *specialisation* mechanics
from MLton/flambda and the *strictness-gates-unboxing* discipline from GHC;
**reject** the closed-world, total-inference assumptions of MLton and Roc, which
a gradual structural system cannot honour.

## The crux — soundness depends on the boundary policy

This is the central argument, and it is what makes the proposal a fork rather
than a routine optimisation.

Type-directed optimisation must fire **only on proven-concrete types** — types
the checker *synthesised* from the code — and **never on a type the checker was
merely told** through an annotation that was trusted across an `any`. The
default gradual boundary is silent: an `any` value flowing into a `T`-annotated
position is trusted, not checked (house-style §1.3; H13a is the standing
default). That is fine when types are advisory, because a lying annotation only
mis-*describes* a value; the runtime still tag-checks everything and behaves
correctly. **It stops being fine the moment a type unlocks a dispatch-free,
unboxed code path.** If `f` is annotated `number -> number` but is actually
reached, through an `any`, by a *string*, then:

- under today's advisory regime, `+` inside `f` resolves the operand to a
  `Native::Str`, fails the numeric ladder in `Add::execute`
  (`arith.rs:131-137`), and raises a clean `NumericDomainError`;
- under a naïve H11a/H11f that *trusted the annotation*, the unboxed path would
  read the operand's payload **as a number** with no tag check — reinterpreting a
  string pointer as an integer. That is memory corruption, not a type error.

So the optimisation cannot trust annotations-through-`any`. Two sound positions,
and the choice is [0002](0002-gradual-typing-boundary-policy.md)'s to make:

1. **Synthesis-only.** Optimise solely where the checker *synthesised* a
   concrete type from surrounding code (a literal, a prelude result, a value
   already narrowed) — never where the type rests on an unchecked annotation at
   an `any` boundary. Conservative, sound, and requires no runtime checks; the
   cost is that fewer sites optimise (annotated-but-unverified hot loops stay on
   the slow path).
2. **Check-at-the-boundary.** Insert exactly the targeted runtime checks that
   [0002](0002-gradual-typing-boundary-policy.md) discusses (transient/concrete-style,
   à la Vitousek/Greenberg) at each `any → T` site that feeds an optimised path,
   so that a lying `any` is caught *before* it reaches the unboxed code. Wider
   coverage, but reintroduces a (small, targeted) runtime cost and depends on
   [0002](0002-gradual-typing-boundary-policy.md) shipping a blame/cast mechanism.

Either way, **0007 cannot land before 0002 settles the boundary policy.** The
two are joined at the hip: 0002 decides where a type can be *trusted*, and 0007
is the consumer that turns that trust into generated code. A type-directed
optimiser built on the silent-trust default would be unsound. This is the
proposal's load-bearing claim.

## Proposed design

Nothing in the surface language changes — this is wholly an implementation
matter (non-negotiable §1 honoured trivially; no new syntax, no new metadata).

**1. Thread type facts past the erasure point.** Today the checker's results die
in `src/bin/eu.rs`. Instead, the bidirectional checker should deposit, for each
core node it visits, a *post-check annotation* recording the synthesised type
and — critically — a **provenance bit**: `Synthesised` (proven from code) vs
`Trusted` (rests on an annotation crossing `any`). Only `Synthesised` facts (or
`Trusted` facts guarded per the boundary policy) are actionable. This is a
side-table keyed on node identity, not a change to the core `Expr`
representation, so the existing pipeline and the LSP path are undisturbed.

**2. A type-aware specialisation pass in the inliner.** The evolution doc's own
recommendation (§1, H11): *"when a callee has a declared type and a call site
has concrete arguments, emit a specialised version."* The inliner is the right
home — it already does depth-aware beta reduction and call-site distribution
(`src/core/inline/reduce.rs`; entry `reduce::inline_pass`, wired at
`src/driver/source.rs:459`). The pass walks call sites; where the callee carries
an actionable `number`-family signature and the arguments are actionable-concrete,
it emits a specialised body whose arithmetic primitives are tagged for the
unboxed/dispatch-free STG path (H11a + H11f together — unboxing is what makes the
direct BIF call legal). H11e (literal-type dead-branch folding) slots into the
same walk, recognising the *brancher set* structurally as H15's narrowing does,
not by the name `if`. H11c (IO flattening) is a separate, later pass over
`IO_BIND` chains the checker has certified as escape-free.

**3. STG support.** The STG already represents `Native` unboxed values and has
the intrinsics; what is new is a compilation mode for a specialised arithmetic
node that bypasses `binary_wrapper`'s tag `case` and emits the BIF call on
already-unboxed operands. This is additive — the existing boxed wrappers remain
for every unspecialised (i.e. most) call.

Illustratively, in real syntax, the optimisable shape is an ordinary annotated
numeric function:

```eu
` { type: "number -> number -> number" }
hot(a, b): a * a + b
```

When `hot` is called with arguments the checker synthesises as `number`, the
pass emits a specialised `hot#num` whose `*` and `+` compile to the unboxed
path; the generic `hot` is retained for any `any`-typed call site (which keeps
the safe, tag-checking wrapper).

## Interaction with the existing roadmap

- **Sequencing.** This is **Stage C** (evolution doc §2). It presupposes Stage A
  (literal types H16, narrowing H15 — for H11e) and benefits from Stage B
  (richer synthesis). It must sequence *after* the Phase-A/B type work and after
  [0002](0002-gradual-typing-boundary-policy.md). Within Track B it sits *behind*
  [0004](0004-compiled-unit-caching.md) (latency), [0005](0005-generational-gc.md)
  and [0006](0006-strictness-analysis.md).
- **[0006 — strictness](0006-strictness-analysis.md).** Tight coupling, both
  ways. A `number` annotation *implies strictness* — a primitive-arithmetic
  operand must be evaluated — so H11a is partly a strictness result, and a
  strictness analyser is the natural place to discharge the "is this operand
  definitely forced?" obligation GHC's worker/wrapper relies on. The two
  reinforce: strictness without types still helps; types without strictness
  cannot unbox lazily-supplied arguments.
- **[0005 — generational GC](0005-generational-gc.md).** Synergistic. Less
  boxing (H11a/H11f) and fewer `IO_BIND` constructors (H11c) mean **fewer
  allocations**, which means **less to mark** — and mark cost dominates
  traversal-heavy VM time (recon §6). Type-directed compilation is, in part, a
  GC-pressure reduction.
- **Supersedes nothing; depends on much.** It consumes the entire type-system
  investment rather than replacing any of it.

## Implementation sketch

| Component | Change | Size / risk |
|---|---|---|
| `src/core/typecheck/` | Emit a node-keyed type+provenance side-table from the checker | Medium / low |
| `src/core/inline/` | Type-aware specialisation pass (H11a/f), literal-branch fold (H11e) | Large / medium |
| `src/eval/stg/` (`arith.rs`, `compiler.rs`) | Specialised unboxed-arithmetic compilation mode bypassing `binary_wrapper` | Medium / **high** (must stay sound for every operand shape) |
| `src/eval/stg/io.rs`, `src/driver/io_run.rs` | Flattened IO sequence form (H11c) | Medium / medium |
| `src/bin/eu.rs`, `src/driver/eval.rs` | Carry the side-table from check into `stg::compile` instead of discarding it | Small / low |
| Boundary checks (if policy 2) | Per [0002](0002-gradual-typing-boundary-policy.md) | depends on 0002 |

**Phasing.** (P1) side-table + carry it to compile; (P2) H11f direct dispatch on
synthesis-only sites — smallest, most measurable; (P3) H11a unboxing, gated on
[0006](0006-strictness-analysis.md); (P4) H11e; (P5) H11c. Each phase is
independently shippable and independently benchmarkable.

This is a **large, post-1.0 effort**. The highest-risk surface is the STG
unboxed path: a single case where an optimised node meets an operand shape the
type was wrong about is a correctness bug, not a slowdown — which is precisely
why the boundary policy is load-bearing.

## Alternatives considered

- **Leave types advisory forever.** Legitimate if the answer to open question 4
  is "we don't care about interpreter speed." Then the type system earns its
  keep purely as a correctness/tooling aid and 0007 is shelved. This is the
  honest null hypothesis and the proposal does not pretend otherwise.
- **Optimise without the type system** (pure strictness/demand analysis,
  [0006](0006-strictness-analysis.md) alone). Captures some of H11a's unboxing
  without any typing, and should be done regardless. But it cannot do H11f
  (typed dispatch), H11e (literal branches) or the targeted specialisation —
  those genuinely need types.
- **Trust annotations directly (no provenance bit).** Simplest, and *unsound* —
  the corruption scenario above. Rejected outright.
- **Whole-program monomorphisation (MLton/Roc style).** Incompatible with
  gradual, separately-imported, `any`-permeated code. Rejected.

## Risks & what would kill this

- **Open question 4 answered "no."** If interpreter runtime performance is not a
  goal, the entire proposal is moot. This is the primary kill switch.
- **[0002](0002-gradual-typing-boundary-policy.md) chooses "silent trust forever"
  with no synthesis/trust distinction.** Then there is no sound foundation to
  optimise on, and 0007 cannot proceed safely.
- **Measured wins fail to materialise.** If a prototype H11f/H11a on real render
  workloads shows <2× on representative (not micro-benchmark) programs — because
  real eucalypt is dominated by block traversal and string formatting, not tight
  arithmetic — the cost/benefit collapses. *This must be measured before
  committing*; the 5–10× figure is the doc's claim for *tight arithmetic loops*,
  which may be rare in templating workloads.
- **Soundness escapes.** Any path where an optimised node sees a mistyped
  operand is a P1 memory-safety bug. The blast radius is why this is Stage C.

## Success criteria

- A typed numeric micro-benchmark (e.g. a tight `fold` over numbers) shows the
  documented 5–10× over the boxed path, *verified on a clean release build*
  (CLAUDE.md benchmark-verification rule).
- A representative render workload shows a measurable, independently-reproduced
  reduction in allocations and mark time (cross-checked against
  [0005](0005-generational-gc.md) instrumentation).
- **Zero** correctness regressions across the harness suite with optimisation
  on, including under `EU_GC_VERIFY=2` and the boundary-stress cases
  [0002](0002-gradual-typing-boundary-policy.md) defines.
- A clear, documented rule for *which* sites optimise (the synthesis/trust
  boundary), so the performance model is predictable.

## References

**Eucalypt source**
- `src/bin/eu.rs:112-146` — type-check is advisory, then the untyped evaluand is run
- `src/driver/eval.rs` (`try_execute`) — `stg::compile` receives core with no type info
- `src/eval/stg/arith.rs:88-138` (`Add::execute`), `:452` (`binary_wrapper`) — runtime tag-check dispatch
- `src/eval/memory/syntax.rs:37-51` — `Native` unboxed value representation
- `lib/prelude.eu:891-892` (`max`, `>(a, a)` constraint), `:916-917` (`min`, `<(a, a)` constraint) — structural operator constraints shipped in 0.7.0, adding to the annotated numeric targets for H11f
- `src/eval/stg/io.rs:47-65` — `IoBind` constructor; `src/driver/io_run.rs:1217-1244` — heap-walking interpreter
- `src/core/inline/reduce.rs`; `src/driver/source.rs:459` — inliner entry (`reduce::inline_pass`)
- `lib/prelude.eu:644`; `src/eval/intrinsics.rs:123` — `+` → `ADD` intrinsic

**Type-system evolution**
- `docs/development/type-system-evolution.md` §1 H11 (a–f), §4 (use of types in the implementation), §5 open question 4

**External**
- GHC user's guide §5.3, *Optimisation* (worker/wrapper, SpecConstr, strictness) — <https://downloads.haskell.org/ghc/latest/docs/users_guide/using-optimisation.html>
- Weeks, *Whole-Program Compilation in MLton*, ML 2006 — <http://www.mlton.org/References.attachments/060916-mlton.pdf>
- Brandon et al., *Better Defunctionalization through Lambda Set Specialization*, ICFP 2023 (Roc) — <https://dl.acm.org/doi/10.1145/3591260>
- Brady, *Idris 2: Quantitative Type Theory in Practice*, ECOOP 2021 (erasure) — <https://drops.dagstuhl.de/storage/00lipics/lipics-vol194-ecoop2021/LIPIcs.ECOOP.2021.9/LIPIcs.ECOOP.2021.9.pdf>
- OCaml manual, *Optimisation with Flambda* — <https://ocaml.org/manual/5.4/flambda.html>
