# 0012 — The algebraic-subtyping fork (H8 MLsub/MLstruct): won't-do

- **Status:** Draft proposal for review
- **Track:** C — type system & language (beyond the roadmap)
- **Classification:** Stage-C-Fork
- **Suggested horizon:** post-1.0
- **Related:** H8 and open-question 5 (type-system-evolution.md §5); siblings [0011 — "typeclasses without classes"](0011-typeclasses-without-classes.md), [0007 — type-directed compilation](0007-type-directed-compilation.md), [0008 — parallel & speculative evaluation](0008-parallel-evaluation.md); collides directly with the now-shipped HKT keystone (0.7.0; higher-kinded-types-spec.md, bead B1)

## Summary

Eucalypt's type checker is a hand-rolled Hindley–Milner-with-subtyping engine: freshen a scheme, walk the two types, unify structurally, and run subtyping as a side-check. As of 0.7.0 it has grown a real kind system, higher-kinded constructor application, higher-rank quantification, and higher-order (Miller-pattern) unification — all on that same predictable spine. H8 asks whether to **replace that core wholesale** with an MLsub/MLstruct-style *algebraic-subtyping* algorithm, under which type variables carry *bounds* rather than equalities, every term has a principal type, and rows, unions/intersections, and structural constraints "fall out for free." This is the single biggest type-system fork on the table. This dossier lays out the case for, the case against, the cost (a year of careful engineering), and the decision. **The decision is won't-do:** algebraic subtyping is too cutting-edge in combination with HKT — it does not extend cleanly to the higher-rank/higher-kinded machinery that *shipped* in 0.7.0, so the swap would mean tearing out the working keystone eucalypt's flagship monad story is built on. The document records the (research-grade) evidence that would have to land before this could even be reopened.

## Motivation

The current core is small, readable, and predictable. Its shape is worth stating exactly, because the fork is a bet against this specific code.

- **Unification is structural with an occurs check.** `unify(t1, t2, subst)` resolves both sides through the substitution, then matches structurally: variables bind after an occurs check (`unify.rs:480-488`), function types are contravariant/covariant by recursion (`unify.rs:269`), and `any` unifies with everything (`unify.rs:148`) — the gradual escape hatch. As of 0.7.0 it also performs higher-order Miller-pattern unification for HKT (`unify.rs:153-203`), so "first-order" no longer describes it.
- **Polymorphism is prenex by default, but explicit higher-rank now exists.** A `TypeScheme` is `forall vars. body` with a `constraints: Vec<Constraint>` field that is *now live* — it carries the structural operator constraints shipped in 0.7.0 (`types.rs:140-144`, `types.rs:133-136`). On each variable reference the scheme is *freshened* — quantified variables renamed to `_t0`, `_t1`, … via a rename substitution (`unify.rs:604`; lookup site `check.rs:491`), and `freshen_with_constraints` freshens the constraints alongside the body (`check.rs:1382`). Crucially, the representation now *also* carries `Type::Forall` (rank-N, annotation-driven; `types.rs:273`) — nested quantification is no longer absent, it is the basis of HKT.
- **Subtyping is a separate relation, and type variables are treated as `any` within it.** `is_subtype(s, t)` is reflexive/transitive structural subtyping; critically, *"uninstantiated type variables are treated as `any` (consistent with everything)"* (`subtype.rs:40`, arm at `subtype.rs:103-104`). The checker tries `unify` first and falls back to `is_subtype` on failure (`check.rs:1595`, `check.rs:1719`).
- **Overloads are unions tried left-to-right, now backed by discharged structural constraints.** `+` carries a `Union` of arrow types; `apply_union` commits to the first variant that unifies (`check.rs:1694`). 0.7.0 added the constraint side: an annotation like `"<(a, a) => a → a → a"` (`min`, `lib/prelude.eu:916`) is freshened with its body and `discharge_constraint` verifies the chosen overload accepts the resolved arguments (`check.rs:587`, `check.rs:1427-1428`), staying silent when an argument is `any` or unbound. This is a deliberately *finite*, enumeration-based stand-in for ad-hoc polymorphism — hand-built and working, not the lattice MLsub would give.
- **Rows are now fully inferred, by hand.** Records carry an optional named `row`; unification does *greedy absorption* — a row variable swallows the other side's extra fields (`unify.rs:346`) — and 0.7.0 closed the remaining gap: unannotated parameters used as blocks now infer fresh, row-polymorphic types instead of `any` (the TS-B9/H3 work has shipped). This is the textbook case of a feature MLsub would have produced *for free* that eucalypt has instead *built by hand and shipped*.

The limit this core retains, now that Phase A/B have landed, is a structural one. Treating variables as `any` in subtyping (`subtype.rs:103-104`) means a subtyping check involving an un-unified variable is *unsound by construction* — acceptable under the gradual contract, but it means the engine has no notion of "this variable is *at least* `number` and *at most* `top`." The `unify`-then-fall-back-to-`is_subtype` two-step (`check.rs:1595`) is order-sensitive and does not compose: it cannot accumulate a *lattice* of constraints on a variable, only a single equality plus a yes/no subtype probe. Unions (H15/H16) and operator constraints (H10) have now arrived — and the engine handles them by *enumeration and discharge* (`apply_union`, `discharge_constraint`) rather than by accumulating a flow lattice. Where the checker would want to say "this variable flows from these producers and into these consumers," it still cannot — that remains the one thing the current representation does not express and algebraic subtyping does. The point is no longer "this is coming"; it is "this is the residual gap after building everything else by hand."

## Prior art & landscape

Algebraic subtyping is a focused line of PLT research, not a folklore technique.

- **Dolan & Mycroft, *Polymorphism, Subtyping, and Type Inference in MLsub* (POPL 2017)**, and **Dolan's PhD thesis** (*Algebraic Subtyping*, Cambridge 2016). MLsub types a strict superset of core ML, combines subtyping with ML polymorphism, and **guarantees compact principal types**. The mechanism is **biunification**: a strict separation between types describing *inputs* (negative) and *outputs* (positive), with the classical unification algorithm extended to subtyping constraints between them. Compactness comes from type simplification that exploits a deep connection between subtyping and the algebra of regular languages.
- **Parreaux, *The Simple Essence of Algebraic Subtyping* (ICFP 2020, functional pearl)**. A re-presentation, **SimpleSub**, that achieves the same principal-type result *without* biunification's polar types and bisubstitution. Type variables instead **carry explicit lower and upper bounds**, and inference is data-flow: bounds record that a value flows from producers (positive) into consumers (negative). The reference implementation is **under 500 lines**. This is the version any serious eucalypt attempt would start from — it is the most approachable correct account.
- **Parreaux et al., *MLstruct* (OOPSLA 2022)** and the surrounding **MLscript** language. MLstruct adds **first-class unions, intersections, and complement (negation)** as a *Boolean algebra* of structural types, with **equirecursive types** and class/instance matching, while *keeping* ML-style principal inference. It is the state of the art and the only system that combines that feature-set with principal polymorphic inference.

What eucalypt should borrow conceptually is striking: the *outputs* of these algorithms are precisely the features Phase A/B have *already added by hand and shipped*. **Rows** (H3, bead A1/TS-B9 — full inference shipped in 0.7.0), **unions/intersections** (H15, H16, H17), **structural operator constraints** (H10 — `min`/`max` now carry `<(a, a) => …` annotations, `lib/prelude.eu:916`) — in MLsub/MLstruct these are not bolt-ons; they are what the algorithm naturally produces. So the fork is no longer a choice of *route to the same destination*: eucalypt has already arrived at that destination by the hand-rolled route. What remains is a choice between *keep the predictable core that already delivers these features* and *rip it out for an unfamiliar core that derives them differently*:

| Feature | Hand-rolled route (shipped) | Algebraic-subtyping route |
|---|---|---|
| Row polymorphism | H3 / A1 / TS-B9: extended unifier, fresh row vars inferred — **shipped 0.7.0** | Falls out of bounded variables |
| Unions & narrowing | H15/H16/H17: union representation + `synthesise_app` special cases — **shipped 0.6.2/0.7.0** | First-class in the lattice |
| Operator constraints | H10: finite overload enumeration + `discharge_constraint` — **shipped 0.7.0** | Subtyping bounds on a variable |
| Principal types | Not guaranteed; synthesis is best-effort | **Guaranteed by construction** |

The temptation that *was* real — *build four features carefully on a predictable core* versus *get them free from one new, less-familiar core* — has largely evaporated: those four features are built, working, and in users' hands. MLsub's marginal appeal is correspondingly lower; principal types are now nearly the only thing it would add that the hand-rolled core does not.

## The decisive tension: MLsub fights the 0.7 keystone

Here is the argument that dominates the decision. **MLsub does not extend cleanly to higher-rank polymorphism or higher-kinded types.** This is not a detail; it collides head-on with eucalypt's headline feature — and that feature is no longer a plan, it is *shipped code*.

The Phase B keystone was **HKT (bead B1)** — *"B1 is the keystone of Phase B"* (higher-kinded-types-spec.md) — and it landed in **0.7.0**. It introduced a real kind system (`Kind = Star | Arrow`, `types.rs:38-44`), constructor application (`Type::Con` / `Type::App`, `types.rs:225-231`), **explicit higher-rank quantification** (`Type::Forall`, `types.rs:273`), and a type-level lambda (`Type::Lam`, `types.rs:282`) so that `monad()` can be typed honestly: each derived combinator is *separately* polymorphic in `a, b` while sharing the monad constructor `m` — *rank-2 inside a record*. To make that work for arbitrary monads — including those whose action type is not a bare constructor — the unifier gained **higher-order (Miller-pattern) unification**: when a `* → *` variable applied to a type variable meets a concrete structural type, it abstracts the variable out into a `Lam` and binds the constructor (`unify.rs:153-203`). B1's discipline is "predicative, annotation-driven higher-rank, **checked, never inferred**" (higher-kinded-types-spec.md §B1.3, citing Peyton Jones et al., *Practical type inference for arbitrary-rank types*). The current core supports this *because* it freshens annotated schemes and never tries to infer rank-N: HKT slotted into the existing freshen/unify machinery rather than fighting it.

MLsub's principal-types guarantee, by contrast, is a **rank-1** result. Biunification and SimpleSub's bounded variables are defined over a first-order, prenex world; higher-rank and higher-kinded extensions are open research. MLstruct *chips* at the structural-type side (unions/intersections/recursion with principal inference) but says nothing about HKT or higher-rank — its README and paper scope are records, unions, intersections, complement, equirecursion, instance matching, and ML-style inference, with no kind system. So a wholesale MLsub swap would now mean **tearing out the shipped, working higher-rank/HKT machinery that 0.7.0 is built on** — the `Forall`/`Lam`/Miller-pattern apparatus and the HKT-typed `monad()` — and either abandoning that capability or re-deriving it from scratch on a core that resists it. The fork is therefore not "new core, same features"; it is "new core that is hostile to a feature we have already built and released."

This single fact is why the recommendation leans strongly negative — and the shipping of HKT has only sharpened it. You do not replace a working engine with one that is weaker exactly where your flagship feature already lives.

## Cost

Even setting the HKT conflict aside, the cost is severe and the H8 notes name it: **"replacing the core checker is a year of careful engineering."** And the target has only grown since that estimate: it rewrites `unify.rs` (~1,300 lines, now including the Miller-pattern machinery), `subtype.rs` (~1,100 lines), and the inference heart of `check.rs` (~4,350 lines today — bidirectional synthesis/check, flow narrowing, and the new constraint discharge), plus the `Type` representation (now `Con`/`App` constructor application with a `Kind` system and `Forall`/`Lam`, all of which a polar-type or bounded-variable rewrite would replace), plus a type-simplification pass (without it, principal types are correct but unreadably large — Dolan's regular-algebra simplification is *not* optional in practice). It must reproduce, bit-for-bit, the behaviour the existing harness and `eu check lib/prelude.eu` already pin down — *including* the HKT-typed `monad()` and the operator-constraint warnings — while introducing a fundamentally different solver.

The one genuine *de-risker*: **the user-facing type language need not change at all.** Annotations live in `type:` metadata strings parsed by a separate DSL (`parse.rs`); the surface stays identical, and only the *engine* behind it changes. That is the same property B1 exploited when it added the kind system and `forall` to the DSL without disturbing existing annotations. It means an MLsub swap could in principle be invisible to users — which lowers the *blast radius* but not the *implementation cost* or the HKT conflict.

There is also a soundness-model question. Eucalypt's runtime is lazy, pure, and **single-threaded over an `UnsafeCell` heap whose safety depends on stop-the-world access** (`heap.rs:8`). Algebraic subtyping is a *static* discipline and does not touch the runtime, so this is not a blocker — but it is a reminder that the type checker is *advisory and erased* today, so the entire payoff of a more-principal core is *better warnings and tooling*, not runtime safety. A year of work that produces no runtime guarantee, against a gradual checker users can already ignore, is a hard sell on the tool-first framing (this is exactly open-question 4's "how much do we care").

## Interaction with the existing roadmap

H8 sits in **Stage C** alongside the other "radical options," and the roadmap is explicit that **these are mutually substitutable — "pick at most one or two"** (type-system-evolution.md §2, Stage C). H8 competes for the same scarce "big rebuild" budget as:

- **[0007] type-directed compilation (H11)** — using types as a *runtime* asset (unboxing, IO sequencing). This delivers measurable runtime wins on the predictable core and is *additive*; H8 delivers none.
- **[0008] parallel & speculative evaluation** — the concurrency rebuild, which fights the single-threaded heap (`heap.rs:8`).

These three cannot all be done at once; arguably no two can. H8 is the *least* additive of them: 0007 and 0008 give the user something new (speed), whereas H8 gives a *cleaner internal core* whose user-visible benefit is indirect. Crucially, H8 also **conflicts with, rather than builds on, the now-shipped Phase B work** ([0011] and B1): 0011 matures "typeclasses without classes" *on the hand-rolled core's* finite-overload model (H10), which now ships as live constraint discharge, and B1 has *already added HKT to it* (0.7.0). H8 would obsolete the H10 machinery (subsuming it into bounds) but at the price of the HKT story it would have to discard. The roadmap's own verdict was to reassess after Stage B — and Stage B has now landed: *"The choice between H8 and 'keep extending the current core' is the biggest fork. Reassess after Stage B"* (type-system-evolution.md §2). With Stage B shipped and stable, the reassessment runs *against* H8.

## Recommendation

**Decision: won't-do. Keep the predictable hand-rolled core — Phase A/B are built on it, and leave them there.** Specifically:

1. **Phase A (rows, `Dict`, recursive types, narrowing, literals — 0.6.2) and Phase B (HKT/B1, operator constraints/H10, `Partial` — 0.7.0) have *shipped* on the current engine.** They were specced against it and they fit it; the bet that the hand-rolled core could carry them has paid off.
2. **Do not begin any MLsub work to displace that shipped foundation.** The information needed to make this call now *does* exist — Phase B is in users' hands — and it points away from a rebuild, not towards one.
3. **Reassess now that Phase B has landed, and act only if the hand-rolled core is *visibly creaking*** under the weight of unions and constraints — see the falsification triggers below. The shipped evidence shows it is not creaking: it absorbed HKT, full row inference, and constraint discharge without losing its shape. Absent that creaking, leave the engine alone: predictability is worth more than principality for an advisory, tool-first checker.
4. **If ever pursued, target MLstruct, not classic MLsub** — it is the only variant that even gestures at the structural richness (unions/intersections) eucalypt wants, and start from SimpleSub's ~500-line presentation for comprehensibility. Even then, treat the HKT incompatibility as a *first-class research risk* to be retired *before* committing, not discovered mid-rebuild.
5. **Treat H8 as one of three competing big rebuilds** ([0007], [0008]). If runtime performance is the priority, [0007] dominates; if expressiveness, the current core has *already delivered* Phase B's expressiveness, so the marginal case for a rebuild is correspondingly thin. H8 wins only in a world where the *internal* maintainability of the checker has become the binding constraint — which is not today's world and is unlikely to be 1.0's.

So: **won't-do.** The narrow, research-grade conditions under which it could ever be reopened are in the next section.

## Alternatives considered

- **Incrementally bolt bounds onto the current unifier** (variables gain optional lower/upper bounds without a full algebraic rewrite). Rejected as the worst of both: it abandons the predictability of the simple core *and* the principal-types guarantee of the real algorithm, landing in an unprincipled middle that is hard to reason about. If you want bounds, do the real thing.
- **Adopt MLstruct's Boolean-algebra unions only** (the union/intersection lattice) while keeping HM unification elsewhere. Tempting, but the union machinery is entangled with the bounded-variable solver; you cannot cleanly take the lattice without the inference engine. H15/H16's hand-rolled unions — now shipped, with literal-string singletons and union smart-constructors that deduplicate and absorb (0.6.2) — are the pragmatic substitute, and they already work.
- **Do nothing, ever.** The honest default. The risk is that H10 + unions + narrowing accrete enough special cases in `synthesise_app` that the core becomes *de facto* an ad-hoc constraint solver — at which point you have paid most of the complexity cost with none of the principality. The triggers below are exactly the early-warning signs of this drift.

## Risks & what would kill this

This is **won't-do**. The only path that could *reopen* it — i.e. make H8 worth its year — requires **all** of the following to hold, and the dominant one (3) is a research result that does not yet exist:

1. **The core is creaking measurably.** The narrowing/overload special cases in `synthesise_app` (the `apply_union` path, `check.rs:1694`, plus H15's brancher-classification and the `discharge_constraint` path) have grown into a tangle that is *generating wrong or unprincipled warnings* users complain about — not merely "large code." (The 0.7.0 shipping is itself early evidence *against* this: the additions integrated cleanly.)
2. **Principality is being missed in practice.** Concrete cases arise where the hand-rolled synthesiser infers a *non-principal* type and a user is bitten (e.g. an annotation accepted in one position, rejected in an equivalent one), and these are *not* fixable by local patches.
3. **The HKT conflict has been retired.** Someone (ideally upstream MLscript work) has shown a *checked, annotation-driven* higher-rank/HKT extension of algebraic subtyping that preserves B1's `Forall` discipline. Until this exists, H8 is **ruled out**, full stop — it would now mean ripping out 0.7.0's *shipped, working* keystone (`Forall`/`Lam`/Miller-pattern unification, HKT-typed `monad()`), not merely complicating a planned one.
4. **The runtime-performance fork ([0007]) has been judged not worth it**, freeing the single "big type-system rebuild" slot.

If any one of (1)–(3) fails, keep the current core. (3) is the dominant gate: it is currently *open against* H8, which is **why the decision is won't-do, not merely deferred** — and the shipping of HKT has raised the stakes of failing it, since the swap now destroys working capability rather than merely deferring a feature.

The open questions this touches: **§5 Q5** ("invest in algebraic subtyping as a long-term core replacement, or stay with HM-with-subtyping forever?") — answered here as *stay, pending the triggers*; **§5 Q4** (how much we care about runtime performance) — which, if "a lot," redirects the budget to [0007]; **§5 Q3** (encourage user-defined monads, pulling in HKT) — which, if "yes," *hardens* the case against H8.

## Success criteria

Because the decision is **won't-do**, success is measured as a correctly-made and correctly-recorded decline:

- Phase A and Phase B (including B1/HKT) have *shipped* on the hand-rolled core (0.6.2/0.7.0) and pass the existing harness and `eu check lib/prelude.eu` — this criterion is now *met*, which is the strongest single argument against the rebuild.
- No MLsub engineering is started before the four triggers are evaluated against the now-shipped Phase B reality.
- The trigger list above is revisited now that Phase B has landed, with a written yes/no, rather than left as a standing "someday."

Were H8 ever greenlit, *its* success criteria would be: identical warning behaviour to the prior core on the full harness; principal types demonstrably recovered on the cases that motivated the switch; a working type-simplifier keeping displayed types readable; and — the gate — HKT/B1 preserved end-to-end.

## References

**Eucalypt source**

- `src/core/typecheck/unify.rs` — structural unification with occurs check (`:819`), `any`-unifies-all (`:148`), freshen (`:604`), greedy row absorption (`:346`), and **higher-order Miller-pattern unification** for HKT (`:153-203`).
- `src/core/typecheck/subtype.rs` — `is_subtype`/`is_consistent`; type variables treated as `any` (`:32`, `:60`).
- `src/core/typecheck/check.rs` — bidirectional synthesise/check; union-overload `apply_union` (`:1694`); unify-then-subtype fallback (`is_subtype`, `:1595`/`:1719`); **live operator-constraint discharge** `discharge_constraint` (`:587`), freshen-with-constraints (`:1382`), constraint discharge site (`:1427-1428`).
- `src/core/typecheck/types.rs` — `Con`/`App` constructor representation (`:225-231`), `Kind` system (`:38-44`), `Type::Forall` rank-N (`:273`), `Type::Lam` (`:282`); `TypeScheme` (`:140`) with now-**live** `constraints` (`:143`) carrying structural operator constraints; `Mu` (`:266`); `humanise` (`:611`).
- `lib/prelude.eu:891`/`:916` — `max`/`min` carrying shipped structural operator constraints (`">(a, a) => …"`, `"<(a, a) => …"`).
- `src/eval/memory/heap.rs:8` — single-threaded `UnsafeCell` heap soundness invariant.
- `docs/development/type-system-evolution.md` — H8 (§1) and open-question 5 (§5); Stage C "pick at most one or two" (§2).
- `docs/development/higher-kinded-types-spec.md` — bead B1, the (now shipped, 0.7.0) Phase B keystone; checked-not-inferred higher-rank (§B1.3).
- `CHANGELOG.md` — 0.6.2 (Phase A) and 0.7.0 (Phase B + HKT) entries.

**Papers & systems**

- Dolan & Mycroft, *Polymorphism, Subtyping, and Type Inference in MLsub*, POPL 2017 — [popl17.sigplan.org](https://popl17.sigplan.org/details/POPL-2017-papers/47/Polymorphism-subtyping-and-type-inference-in-MLsub), [ACM](https://dl.acm.org/doi/10.1145/3093333.3009882); Dolan, *Algebraic Subtyping* (PhD thesis, Cambridge 2016) — [repository.cam.ac.uk](https://www.repository.cam.ac.uk/handle/1810/261583).
- Parreaux, *The Simple Essence of Algebraic Subtyping* (functional pearl), ICFP 2020 — [ACM](https://dl.acm.org/doi/10.1145/3409006); SimpleSub overview — [lptk.github.io](https://lptk.github.io/programming/2020/03/26/demystifying-mlsub.html).
- Parreaux et al., *MLstruct: Principal Type Inference in a Boolean Algebra of Structural Types*, OOPSLA 2022 — [ACM](https://dl.acm.org/doi/10.1145/3563304), [TACO Lab](https://cse.hkust.edu.hk/~parreaux/publication/oopsla22a/), [implementation](https://github.com/hkust-taco/mlstruct).
- Peyton Jones, Vytiniotis, Weirich & Shields, *Practical type inference for arbitrary-rank types* (JFP 2007) — the higher-rank discipline B1 follows.

**Sibling proposals:** [0007 — type-directed compilation](0007-type-directed-compilation.md), [0008 — parallel & speculative evaluation](0008-parallel-evaluation.md), [0011 — "typeclasses without classes" maturation](0011-typeclasses-without-classes.md).
