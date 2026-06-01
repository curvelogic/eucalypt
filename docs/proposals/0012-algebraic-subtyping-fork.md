# 0012 — The algebraic-subtyping fork (H8 MLsub/MLstruct): a decision dossier

- **Status:** Draft proposal for review
- **Track:** C — type system & language (beyond the roadmap)
- **Classification:** Stage-C-Fork
- **Suggested horizon:** post-1.0
- **Related:** H8 and open-question 5 (type-system-evolution.md §5); siblings [0011 — "typeclasses without classes"](0011-typeclasses-without-classes.md), [0007 — type-directed compilation](0007-type-directed-compilation.md), [0008 — parallel & speculative evaluation](0008-parallel-evaluation.md); depends conceptually on the Phase B HKT keystone (higher-kinded-types-spec.md, bead B1)

## Summary

Eucalypt's type checker is a hand-rolled Hindley–Milner-with-subtyping engine: freshen a scheme, walk the two types, unify structurally, and run subtyping as a side-check. H8 asks whether to **replace that core wholesale** with an MLsub/MLstruct-style *algebraic-subtyping* algorithm, under which type variables carry *bounds* rather than equalities, every term has a principal type, and rows, unions/intersections, and structural constraints "fall out for free." This is the single biggest type-system fork on the table. This dossier lays out the case for, the case against, the cost (a year of careful engineering), and a recommendation — framed as the maintainer's call. The short answer is **probably not before 1.0, and likely not after** — but the document states precisely the evidence that would change that.

## Motivation

The current core is small, readable, and predictable. Its shape is worth stating exactly, because the fork is a bet against this specific code.

- **Unification is first-order with an occurs check.** `unify(t1, t2, subst)` resolves both sides through the substitution, then matches structurally: variables bind after an occurs check (`unify.rs:62-81`, `unify.rs:462`), function types are contravariant/covariant by recursion (`unify.rs:87-93`), and `any` unifies with everything (`unify.rs:56`) — the gradual escape hatch.
- **Polymorphism is rank-1 prenex.** A `TypeScheme` is `forall vars. body` with a reserved-but-unused `constraints: Vec<Constraint>` (`types.rs:21`, `types.rs:30-36`). On each variable reference the scheme is *freshened* — quantified variables renamed to `_t0`, `_t1`, … via a rename substitution (`unify.rs:385-402`; lookup site `check.rs:191`). There is no nested `forall`, no rank-N.
- **Subtyping is a separate relation, and type variables are treated as `any` within it.** `is_subtype(s, t)` is reflexive/transitive structural subtyping; critically, *"uninstantiated type variables are treated as `any` (consistent with everything)"* (`subtype.rs:32-33`, arm at `subtype.rs:60`). The checker tries `unify` first and falls back to `is_subtype` on failure (`check.rs:789-799`).
- **Overloads are unions tried left-to-right.** `+` carries a `Union` of arrow types; `apply_union` commits to the first variant that unifies (`check.rs:873-901`). This is the poor-man's stand-in for constraint-based ad-hoc polymorphism.
- **Rows are half-implemented and one-directional.** Records carry an optional named `row`; unification does *greedy absorption* — a row variable swallows the other side's extra fields (`unify.rs:207-254`) — but inference of *fresh* row variables for unannotated functions is deferred to bead TS-B9 (type-system-evolution.md, H3).

The limits this core will hit as Phase A/B land are foreseeable. Treating variables as `any` in subtyping (`subtype.rs:60`) means a subtyping check involving an un-unified variable is *unsound by construction* — acceptable under the gradual contract, but it means the engine has no notion of "this variable is *at least* `number` and *at most* `top`." The `unify`-then-fall-back-to-`is_subtype` two-step (`check.rs:789`) is order-sensitive and does not compose: it cannot accumulate a *lattice* of constraints on a variable, only a single equality plus a yes/no subtype probe. As unions (H15/H16) and operator constraints (H10) arrive, the checker will increasingly want to say "this variable flows from these producers and into these consumers" — which is exactly what the current representation cannot express, and exactly what algebraic subtyping is built to express.

## Prior art & landscape

Algebraic subtyping is a focused line of PLT research, not a folklore technique.

- **Dolan & Mycroft, *Polymorphism, Subtyping, and Type Inference in MLsub* (POPL 2017)**, and **Dolan's PhD thesis** (*Algebraic Subtyping*, Cambridge 2016). MLsub types a strict superset of core ML, combines subtyping with ML polymorphism, and **guarantees compact principal types**. The mechanism is **biunification**: a strict separation between types describing *inputs* (negative) and *outputs* (positive), with the classical unification algorithm extended to subtyping constraints between them. Compactness comes from type simplification that exploits a deep connection between subtyping and the algebra of regular languages.
- **Parreaux, *The Simple Essence of Algebraic Subtyping* (ICFP 2020, functional pearl)**. A re-presentation, **SimpleSub**, that achieves the same principal-type result *without* biunification's polar types and bisubstitution. Type variables instead **carry explicit lower and upper bounds**, and inference is data-flow: bounds record that a value flows from producers (positive) into consumers (negative). The reference implementation is **under 500 lines**. This is the version any serious eucalypt attempt would start from — it is the most approachable correct account.
- **Parreaux et al., *MLstruct* (OOPSLA 2022)** and the surrounding **MLscript** language. MLstruct adds **first-class unions, intersections, and complement (negation)** as a *Boolean algebra* of structural types, with **equirecursive types** and class/instance matching, while *keeping* ML-style principal inference. It is the state of the art and the only system that combines that feature-set with principal polymorphic inference.

What eucalypt should borrow conceptually is striking: the *outputs* of these algorithms are precisely the features Phase A/B add by hand. **Rows** (H3, bead A1/TS-B9), **unions/intersections** (H15, H16, H17), **structural operator constraints** (H10) — in MLsub/MLstruct these are not bolt-ons; they are what the algorithm naturally produces. So the fork is a genuine choice of *route to the same destination*:

| Feature | Hand-rolled route (current roadmap) | Algebraic-subtyping route |
|---|---|---|
| Row polymorphism | H3 / A1 / TS-B9: extend the unifier, infer fresh row vars | Falls out of bounded variables |
| Unions & narrowing | H15/H16/H17: union representation + `synthesise_app` special cases | First-class in the lattice |
| Operator constraints | H10: finite overload enumeration before `=>` | Subtyping bounds on a variable |
| Principal types | Not guaranteed; synthesis is best-effort | **Guaranteed by construction** |

The temptation is real: *build four features carefully on a predictable core*, versus *get them free from one new, less-familiar core*.

## The decisive tension: MLsub fights the 0.7 keystone

Here is the argument that dominates the decision. **MLsub does not extend cleanly to higher-rank polymorphism or higher-kinded types.** This is not a detail; it collides head-on with eucalypt's headline near-term feature.

The Phase B keystone is **HKT (bead B1)** — *"B1 is the keystone of Phase B"* (higher-kinded-types-spec.md). It introduces a real kind system (`Kind = Star | Arrow`), constructor application (`Type::Con` / `Type::App`), and **explicit higher-rank quantification** (`Type::Forall`) so that `monad()` can be typed honestly: each derived combinator is *separately* polymorphic in `a, b` while sharing the monad constructor `m` — *rank-2 inside a record*. B1's discipline is "predicative, annotation-driven higher-rank, **checked, never inferred**" (higher-kinded-types-spec.md §B1.3, citing Peyton Jones et al., *Practical type inference for arbitrary-rank types*). The current core supports this *because* it freshens annotated schemes and never tries to infer rank-N: HKT slots into the existing freshen/unify machinery (the B1 file-by-file lists `freshen` walking `Forall`, kind-aware binding in `unify.rs`).

MLsub's principal-types guarantee, by contrast, is a **rank-1** result. Biunification and SimpleSub's bounded variables are defined over a first-order, prenex world; higher-rank and higher-kinded extensions are open research. MLstruct *chips* at the structural-type side (unions/intersections/recursion with principal inference) but says nothing about HKT or higher-rank — its README and paper scope are records, unions, intersections, complement, equirecursion, instance matching, and ML-style inference, with no kind system. So a wholesale MLsub swap would, at best, **trade away or re-derive from scratch the very higher-rank/HKT story that 0.7 is built on**, and at worst make B1 *harder*, not easier. The fork is therefore not "new core, same features" — it is "new core that is hostile to the feature we just shipped."

This single fact is why the recommendation leans strongly negative. You do not replace your engine with one that is weaker exactly where your flagship feature lives.

## Cost

Even setting the HKT conflict aside, the cost is severe and the H8 notes name it: **"replacing the core checker is a year of careful engineering."** Concretely, it rewrites `unify.rs`, `subtype.rs`, and the inference heart of `check.rs` (≈2,000 lines today, the bulk being the bidirectional synthesis/check logic), plus the `Type` representation (polar types or bounded variables), plus a type-simplification pass (without it, principal types are correct but unreadably large — Dolan's regular-algebra simplification is *not* optional in practice). It must reproduce, bit-for-bit, the behaviour the existing harness and `eu check lib/prelude.eu` already pin down, while introducing a fundamentally different solver.

The one genuine *de-risker*: **the user-facing type language need not change at all.** Annotations live in `type:` metadata strings parsed by a separate DSL (`parse.rs`); the surface stays identical, and only the *engine* behind it changes. That is the same property B1 relies on. It means an MLsub swap could in principle be invisible to users — which lowers the *blast radius* but not the *implementation cost* or the HKT conflict.

There is also a soundness-model question. Eucalypt's runtime is lazy, pure, and **single-threaded over an `UnsafeCell` heap whose safety depends on stop-the-world access** (`heap.rs:8`). Algebraic subtyping is a *static* discipline and does not touch the runtime, so this is not a blocker — but it is a reminder that the type checker is *advisory and erased* today, so the entire payoff of a more-principal core is *better warnings and tooling*, not runtime safety. A year of work that produces no runtime guarantee, against a gradual checker users can already ignore, is a hard sell on the tool-first framing (this is exactly open-question 4's "how much do we care").

## Interaction with the existing roadmap

H8 sits in **Stage C** alongside the other "radical options," and the roadmap is explicit that **these are mutually substitutable — "pick at most one or two"** (type-system-evolution.md §2, Stage C). H8 competes for the same scarce "big rebuild" budget as:

- **[0007] type-directed compilation (H11)** — using types as a *runtime* asset (unboxing, IO sequencing). This delivers measurable runtime wins on the predictable core and is *additive*; H8 delivers none.
- **[0008] parallel & speculative evaluation** — the concurrency rebuild, which fights the single-threaded heap (`heap.rs:8`).

These three cannot all be done at once; arguably no two can. H8 is the *least* additive of them: 0007 and 0008 give the user something new (speed), whereas H8 gives a *cleaner internal core* whose user-visible benefit is indirect. Crucially, H8 also **conflicts with, rather than builds on, the Phase B work** ([0011] and B1): 0011 matures "typeclasses without classes" *on the hand-rolled core's* finite-overload model (H10), and B1 adds HKT to it. H8 would obsolete the H10 machinery (subsuming it into bounds) but at the price of the HKT story. The roadmap's own verdict stands: *"The choice between H8 and 'keep extending the current core' is the biggest fork. Reassess after Stage B"* (type-system-evolution.md §2).

## Recommendation

**Keep the predictable hand-rolled core through 1.0, and probably beyond. Build Phase A/B on it as planned.** Specifically:

1. **Ship Phase A (rows, `Dict`, recursive types, narrowing, literals) and Phase B (HKT/B1, operator constraints/H10, `Partial`) on the current engine.** They are specced against it and fit it.
2. **Do not begin any MLsub work before Phase B is complete and in users' hands.** The information needed to make this call does not exist until then.
3. **Reassess only after Phase B, and only if the hand-rolled core is *visibly creaking*** under the weight of unions and constraints — see the falsification triggers below. Absent that evidence, leave the engine alone: predictability is worth more than principality for an advisory, tool-first checker.
4. **If ever pursued, target MLstruct, not classic MLsub** — it is the only variant that even gestures at the structural richness (unions/intersections) eucalypt wants, and start from SimpleSub's ~500-line presentation for comprehensibility. Even then, treat the HKT incompatibility as a *first-class research risk* to be retired *before* committing, not discovered mid-rebuild.
5. **Treat H8 as one of three competing big rebuilds** ([0007], [0008]). If runtime performance is the priority, [0007] dominates; if expressiveness, finishing Phase B on the current core dominates. H8 wins only in a world where the *internal* maintainability of the checker has become the binding constraint — which is not today's world and is unlikely to be 1.0's.

So: **probably not.** The precise evidence that would change that verdict is in the next section.

## Alternatives considered

- **Incrementally bolt bounds onto the current unifier** (variables gain optional lower/upper bounds without a full algebraic rewrite). Rejected as the worst of both: it abandons the predictability of the simple core *and* the principal-types guarantee of the real algorithm, landing in an unprincipled middle that is hard to reason about. If you want bounds, do the real thing.
- **Adopt MLstruct's Boolean-algebra unions only** (the union/intersection lattice) while keeping HM unification elsewhere. Tempting, but the union machinery is entangled with the bounded-variable solver; you cannot cleanly take the lattice without the inference engine. H15/H16's hand-rolled unions are the pragmatic substitute.
- **Do nothing, ever.** The honest default. The risk is that H10 + unions + narrowing accrete enough special cases in `synthesise_app` that the core becomes *de facto* an ad-hoc constraint solver — at which point you have paid most of the complexity cost with none of the principality. The triggers below are exactly the early-warning signs of this drift.

## Risks & what would kill this

The bet *against* H8 is falsified — i.e. H8 becomes worth its year — if, **after Phase B**, all of these hold:

1. **The core is creaking measurably.** The narrowing/overload special cases in `synthesise_app` (the `apply_union` path, `check.rs:873`, plus H15's brancher-classification) have grown into a tangle that is *generating wrong or unprincipled warnings* users complain about — not merely "large code."
2. **Principality is being missed in practice.** Concrete cases arise where the hand-rolled synthesiser infers a *non-principal* type and a user is bitten (e.g. an annotation accepted in one position, rejected in an equivalent one), and these are *not* fixable by local patches.
3. **The HKT conflict has been retired.** Someone (ideally upstream MLscript work) has shown a *checked, annotation-driven* higher-rank/HKT extension of algebraic subtyping that preserves B1's `Forall` discipline. Until this exists, H8 is **ruled out**, full stop — it would regress 0.7's keystone.
4. **The runtime-performance fork ([0007]) has been judged not worth it**, freeing the single "big type-system rebuild" slot.

If any one of (1)–(3) fails, keep the current core. (3) is the dominant gate: it is currently *open against* H8.

The open questions this touches: **§5 Q5** ("invest in algebraic subtyping as a long-term core replacement, or stay with HM-with-subtyping forever?") — answered here as *stay, pending the triggers*; **§5 Q4** (how much we care about runtime performance) — which, if "a lot," redirects the budget to [0007]; **§5 Q3** (encourage user-defined monads, pulling in HKT) — which, if "yes," *hardens* the case against H8.

## Success criteria

Because the recommendation is "defer/decline," success is measured as **a correctly-made non-decision**:

- Phase A and Phase B (including B1/HKT) ship on the hand-rolled core and pass the existing harness and `eu check lib/prelude.eu` unchanged.
- No MLsub engineering is started before the four triggers are evaluated against shipped Phase B reality.
- The trigger list above is revisited once post-Phase-B, with a written yes/no, rather than left as a standing "someday."

Were H8 ever greenlit, *its* success criteria would be: identical warning behaviour to the prior core on the full harness; principal types demonstrably recovered on the cases that motivated the switch; a working type-simplifier keeping displayed types readable; and — the gate — HKT/B1 preserved end-to-end.

## References

**Eucalypt source**

- `src/core/typecheck/unify.rs` — first-order unification, occurs check (`:462`), `any`-unifies-all (`:56`), freshen (`:385`), greedy row absorption (`:207`).
- `src/core/typecheck/subtype.rs` — `is_subtype`/`is_consistent`; type variables treated as `any` (`:32`, `:60`).
- `src/core/typecheck/check.rs` — bidirectional synthesise/check; freshen-on-lookup (`:191`); union-overload `apply_union` (`:873`); unify-then-subtype fallback (`:789`).
- `src/core/typecheck/types.rs` — `TypeScheme` (`:30`), reserved `constraints` (`:21`), `humanise` (`:248`).
- `src/eval/memory/heap.rs:8` — single-threaded `UnsafeCell` heap soundness invariant.
- `docs/development/type-system-evolution.md` — H8 (§1) and open-question 5 (§5); Stage C "pick at most one or two" (§2).
- `docs/development/higher-kinded-types-spec.md` — bead B1, the Phase B keystone; checked-not-inferred higher-rank (§B1.3).

**Papers & systems**

- Dolan & Mycroft, *Polymorphism, Subtyping, and Type Inference in MLsub*, POPL 2017 — [popl17.sigplan.org](https://popl17.sigplan.org/details/POPL-2017-papers/47/Polymorphism-subtyping-and-type-inference-in-MLsub), [ACM](https://dl.acm.org/doi/10.1145/3093333.3009882); Dolan, *Algebraic Subtyping* (PhD thesis, Cambridge 2016) — [repository.cam.ac.uk](https://www.repository.cam.ac.uk/handle/1810/261583).
- Parreaux, *The Simple Essence of Algebraic Subtyping* (functional pearl), ICFP 2020 — [ACM](https://dl.acm.org/doi/10.1145/3409006); SimpleSub overview — [lptk.github.io](https://lptk.github.io/programming/2020/03/26/demystifying-mlsub.html).
- Parreaux et al., *MLstruct: Principal Type Inference in a Boolean Algebra of Structural Types*, OOPSLA 2022 — [ACM](https://dl.acm.org/doi/10.1145/3563304), [TACO Lab](https://cse.hkust.edu.hk/~parreaux/publication/oopsla22a/), [implementation](https://github.com/hkust-taco/mlstruct).
- Peyton Jones, Vytiniotis, Weirich & Shields, *Practical type inference for arbitrary-rank types* (JFP 2007) — the higher-rank discipline B1 follows.

**Sibling proposals:** [0007 — type-directed compilation](0007-type-directed-compilation.md), [0008 — parallel & speculative evaluation](0008-parallel-evaluation.md), [0011 — "typeclasses without classes" maturation](0011-typeclasses-without-classes.md).
