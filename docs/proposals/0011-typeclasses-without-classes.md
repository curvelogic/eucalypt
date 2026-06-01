# 0011 — "Typeclasses without classes" maturation (beyond H10)

- **Status:** Draft proposal for review
- **Track:** C — type system & language (beyond the roadmap)
- **Classification:** Extends-Roadmap
- **Suggested horizon:** 0.7+
- **Related:** H1 (HKT) and H10 (structural operator constraints) (`docs/development/type-system-evolution.md`); TS-B2, TS-B8 (`docs/development/operator-constraints-and-monad-types-spec.md`); TS-B1 (`docs/development/higher-kinded-types-spec.md`); open-questions 1 & 3 (`type-system-evolution.md` §5); sibling proposals [0001](0001-v1-charter.md), [0012](0012-algebraic-subtyping-fork.md)

## Summary

Phase B (0.7) introduces *ad-hoc polymorphism, the eucalypt way*: a function may
be annotated `"<(a, a) => a -> a -> a"`, meaning "valid wherever `<` accepts
`a`", where the **dictionary is the operator's own union of declared overloads**
and resolution is **finite enumeration over those overloads** — no class, no
global instance database, no dictionary passing. This proposal does not invent
that mechanism (B2 specs it; B1/B8 supply the HKT it pairs with); it **maps how
far the idiom should go, and where it must stop** to preserve structural-over-
nominal. The thesis: structural finite-enumeration resolution has *no orphan/
coherence problem* and stays simple and local, at the cost of instance-based
extensibility and law-checked coherence — and for a config/data language that is
the *right* trade. Concretely it answers two of the maintainer's open questions:
**(1)** no, "no nominal" need not be breached for export contracts; **(3)** user
monads should be *supported but not foregrounded*, remaining a prelude-assembled
`monad()` facility, typed once for all once HKT lands.

## Motivation

Eucalypt already has one half of ad-hoc polymorphism and is about to get the
other. The half it has: operators carry a **union type covering all overloads**,
and "when an argument is applied to a union-typed function, the checker tries
each variant in order and commits to the first that unifies"
(`src/core/typecheck/check.rs:18-22`; the engine is `apply_union`,
`check.rs:873`). The half it is about to get: `TypeScheme.constraints` is a
`Vec<Constraint>` that is "reserved for future constraint support — always empty
for now" (`src/core/typecheck/types.rs:35`), with the `Constraint` representation
— `{ function: String, args: Vec<Type> }` — *already present and unused*
(`types.rs:25-28`). B2 makes that field live.

The user pain B2 removes is concrete. `min` and `max` are today written
`min(l, r): if(l < r, l, r)` / `max(l, r): if(l > r, l, r)`
(`lib/prelude.eu:917,892`) with **no `type:` annotation at all** — because the
only honest annotation available is an exhaustive union of every comparable pair
(`number→number→number | string→string→string | …`), which is both verbose and
incomplete. A constraint says the true thing in one line:
`"<(a, a) => a -> a -> a"`. The same gap recurs for any namespace-generic
function: `labels: map(str.of)` is honestly `"str.of(a) => [a] -> [string]"`, and
`str.of` is a real prelude function (`lib/prelude.eu:1742`), not a class method.

The strategic gap is larger than `min`. Eucalypt's identity rests on **structural
over nominal** (non-negotiable #2). B2 is the first feature that *looks*, from a
Haskell vantage, like a type class — and so the first place the structural
discipline could quietly erode into a nominal one. This document draws that line
*before* the code lands, so B2 ships as "typeclasses without classes" and not as
"classes, eventually". It is also where two parked open questions (§5.1, §5.3)
become unavoidable, since both turn on how far this idiom is pushed.

## Prior art & landscape

Ad-hoc polymorphism has a well-mapped axis from **nominal/global** to
**structural/local**. Eucalypt's `find-a-declared-overload-that-fits` sits at the
far structural end, and it is worth locating precisely.

| System | Dictionary | Resolution | Coherence model |
|---|---|---|---|
| Wadler–Blott (1989) | record passed at runtime | type-directed, elaborated to dictionary args | global, by construction |
| Haskell classes | global instance table | constraint solving + instance search | **global uniqueness** expected; orphans break it |
| Rust traits | per-impl vtable / monomorphised | trait-bound resolution | coherence via orphan rules (`impl` must be local) |
| Scala 3 given/using | term-inferred `given` value | scoped implicit search | **opt-in** coherence; multiple `given`s may exist |
| OCaml modular implicits | first-class module | type-directed implicit module search | **scoped**, not global; can be incoherent |
| PureScript row classes | dictionary, but `Cons`/`Union`/`Nub` over rows | functional-dependency-driven | global, like Haskell |
| **eucalypt B2** | **an operator's union of overloads** | **finite enumeration, left-to-right, no backtrack** | **N/A — no global search to be incoherent** |

Two reference points sharpen the choice. **Wadler & Blott, "How to make ad-hoc
polymorphism less ad hoc" (1989)** is the source of dictionary passing: a class
constraint elaborates to an *extra value argument* carrying the methods. Eucalypt
keeps the dictionary but **never passes it** — the overload set is read statically
from the operator's binding and discharged at the type level. That single
divergence is what keeps the feature advisory and erasable (non-negotiable #3).

The second is the **coherence/orphan** literature. Haskell "expects global
uniqueness of instances", but GHC "does not enforce uniqueness … merely
guarantees that the subset of the instance database it uses … is confluent and
coherent" (ezyang, 2014). An **orphan instance** — in a module owning neither the
type nor the class — is the canonical way that breaks, which is why Rust bakes an
orphan rule into the language and Haskell relies on a social one. It matters
because libraries lean on it: a single `Ord` instance must serve *every*
operation on an ordered tree, or its invariant silently rots (ezyang; "Can orphan
instances cause bugs?", Haskell Discourse 2024). Scala 3 and OCaml modular
implicits trade this away on purpose — Scala lets you "choose, for each type
class, whether it should be coherent or not" and can compile a program with more
than one `Num` instance for `Int` (Scala 3 docs; Racordona 2025); modular
implicits accept *scoped*, hence potentially incoherent, resolution as the price
of locality (Yallop & White, *Modular Implicits*, 2015).

**Eucalypt's position is a sidestep off that axis, not a point on it.** Because
the dictionary is *one named operator's own overload set* with no open,
importable, globally-accumulated instance pool, the questions coherence answers
**do not arise**: there is exactly one overload set for `<`, it cannot be extended
from another module, and resolution is a bounded search over it — nothing to be
non-unique about. Borrow the *finite, local, type-directed* flavour of Scala/OCaml
resolution; do **not** borrow the open instance pool (Haskell/Rust/PureScript),
runtime dictionary passing (Wadler–Blott), or the coherence/orphan apparatus,
which buys nothing once the pool is closed.

## Proposed design

The design is mostly *boundaries*: ratify the mechanism B2/B8 build, bless a
small set of disciplined extensions, and name a hard stop.

### 1. The mechanism, ratified (B2 + B8)

A constrained scheme is a comma-separated constraint list before `=>`, then a
body type, all inside a `type:` string (so no surface-syntax change; records
inside use escaped braces, per the DSL):

```
` { type: "<(a, a) => a -> a -> a" }              # min/max
` { type: "<(a, a), +(a, a) => a -> a -> a" }     # needs both < and +
` { type: "str.of(a) => [a] -> [string]" }        # namespace-function constraint
```

`<(a, a)` is `Constraint { function: "<", args: [Var(a), Var(a)] }`
(`types.rs:25-28`). Resolution (B2.4):

- **Overload set.** The set of `Function` types `name` is annotated with — one
  `Function` is one overload, a `Union` of them is several. Read from `name`'s
  binding, reusing the existing union machinery (`check.rs:873`).
- **Discharge.** When the scheme is instantiated and the constraint's `args`
  resolve to concrete types, succeed iff some overload accepts them
  (`is_consistent`); else a **warning** (never an error — non-negotiable #3).
- **Propagation.** If an `arg` variable is still unbound, carry the constraint
  into the enclosing scheme's `constraints` vector. Constrained polymorphism
  composes; multiple constraints are a left-to-right conjunction sharing only
  the type variables the rest of the signature already unifies. *No
  inter-constraint interaction, no backtracking.*
- **Gradual.** An `any` argument satisfies any constraint vacuously; the
  boundary stays silent.

B8 is the companion: with B1's HKT, `monad()` — `lib/prelude.eu:104`, today
typed `"{{..}} → {{..}}"` (line 103), i.e. almost nothing — gets its real rank-2
signature over `m :: * -> *`, returning a record of its **nine** derived
combinators (`bind`, `return`, `map`, `then`, `and-then`, `join`, `sequence`,
`map-m`, `filter-m`, verified against the definition, lines 104-137). Because
`for`, `let`, `random` and `state` are each *built by calling `monad()`*, they
**inherit** typed combinators "for free" — the annotation is written once.

### 2. Disciplined extensions worth taking

These deepen the idiom without crossing into nominal territory:

- **Multi-parameter constraints (already in B2).** `"<(a, a), +(a, a) => …"`.
  The representation already allows it (`Vec<Constraint>`); the DSL adds one
  production. This is the structural analogue of `MultiParamTypeClasses`,
  minus the class.
- **Namespace-function constraints.** `"str.of(a) =>"`, `"io.bind(m) =>"`. The
  constraint references a dotted prelude path, not a class membership — a clean
  generalisation that types `map(str.of)`-shaped functions honestly. Already in
  scope for B2; this proposal endorses it as a *first-class* use, not an edge.
- **Constraint aliases (new, optional, post-0.7).** A recurring conjunction —
  say `"<(a,a), >(a,a), =(a,a)"` — could earn a DSL-level *alias*, e.g. a
  `constraints:` metadata entry binding `Ord(a)` to that conjunction, expanded
  by the parser. This is **textual abbreviation only**: it introduces no class,
  no membership, no instance; `Ord(a)` is sugar that expands to the same
  finite-enumeration checks. It buys readability for the handful of multi-op
  numeric/comparison routines and nothing more. *Recommended only if real
  annotations show the conjunctions repeating;* skip otherwise.

### 3. The hard boundary — what eucalypt must NOT add

| Tempting | Why refused |
|---|---|
| **Nominal classes / `class Ord a`** | Breaks non-negotiable #2 outright. The overload set already *is* the dictionary; a class adds a name and a global pool for no expressive gain. |
| **`implements` / instance declarations** | Reintroduces the open, accumulating instance database — and with it orphans, coherence, and an import-order-sensitive search. The whole point is that there is no pool to declare into. |
| **Global instance coherence / orphan rules** | Solves a problem eucalypt does not have. There is one overload set per operator, fixed at its binding; uniqueness is structural, not enforced. |
| **Runtime dictionary passing** | Defeats erasability (the checker is advisory and types vanish before STG). Resolution is a *static* discharge, not a value threaded at runtime. |
| **Constraint *solving* (backtracking search, superclasses, fundeps)** | B2 is deliberately *enumeration*, not *solving*. Superclasses and functional dependencies are the machinery a class hierarchy needs; with no hierarchy they are dead weight, and they are exactly where coherence proofs get hard (Racordona 2025). |

The discipline is a one-line test: **a constraint may only reference a function
that already exists and is already typed.** It never introduces a new namespace
of "classes" to be populated. If a proposed extension requires *declaring* a new
overload from outside the operator's own binding, it has crossed the line.

### 4. The key trade, stated plainly

What users **gain**: a tiny, predictable mechanism with no import-order surprises,
no orphan hazard, no coherence caveats, and no runtime cost. `min` finally has a
type. Resolution is "look at the overloads of `<`; does one fit?" — explicable in
a sentence, debuggable by reading one binding.

What users **lose**, honestly: (i) **instance-based extensibility** — a user
cannot make their own type `<`-comparable by "adding an instance"; they extend
the *overload union of `<`* (or write a monomorphic function), which is a more
localised, less ceremonious act but also a less familiar one. (ii)
**Law-checked coherence** — nothing proves the overloads of `<` form a consistent
order; the language trusts the prelude author exactly as it trusts every other
advisory annotation. For a configuration/data tool — whose programs are render
pipelines, not libraries shipping cross-module abstractions — (i) is rarely
wanted and (ii) is already the prevailing bargain (types are advisory,
non-negotiable #3). The trade is correct for the domain.

## Interaction with the existing roadmap

- **Builds on B2.** Supersedes nothing; it *frames* B2 and adds the boundary
  policy and the optional, deferred constraint-alias extension. B2's forwards
  compatibility holds — a union-overload annotation is a strict specialisation of
  a constraint.
- **Builds on B1/B8 (HKT).** B8's "user monads inherit typed combinators for
  free" is the linchpin of the open-question-3 recommendation; without B1 there is
  no honest `monad()` type to inherit.
- **Sibling [0001](0001-v1-charter.md)** owns *stability*; its open-question-1
  reading ("a 1.0 export contract is a structural promise … structural typing
  already expresses it") is the conclusion reached here from the type-design side.
  **Sibling [0012](0012-algebraic-subtyping-fork.md)**: an MLsub/MLstruct rebuild
  would make constraints, unions and rows fall out of *one* algorithm, subsuming
  B2's bespoke enumeration — so this proposal's boundary (no class, no solving) is
  also the constraint on what 0012 must preserve.

### Open question 1 — is "no nominal" inviolable? (the export-contract newtype)

The floated exception (§5.1) is "a small opt-in nominal newtype mechanism …
tolerated for export contracts" — a way to say "this value is a `Port`, not just
a `number`" at a unit's boundary. **Recommendation: stay structural.**

A newtype would *buy* nominal distinctness at a boundary (a `Port` not
interchangeable with a `Timeout` though both are `number`) and a stable contract
name. It would *cost* the first nominal entity in the language — which at once
raises "where are its instances declared?", reopening every coherence/orphan
question just closed — plus an edition-gated breaking surface (per 0001) and a
second, fragmenting way to model data. And the benefit is reachable structurally:
a literal-symbol-tagged record `{tag: :port, value: number}`, or a `Dict`/record
contract, gives a distinguishable *shape* without a name, while 0001's editions
supply the stability lever. The newtype's *only* irreducible gain is
**distinctness without a discriminator field** — nice, but not worth being the
thin end of nominal typing. Keep it structural; if boundary distinctness becomes a
measured pain, revisit as an edition-gated, instance-free *opaque alias* (a name,
no class), never a class system.

### Open question 3 — encourage user-defined monads? (HKT in the user's lap)

User monads are **already possible**: `state` is a user-importable monad
assembled by `state: monad{bind: state-bind, return: state-ret}`
(`lib/state.eu:59`), with a `!`-asserted `type:` annotation (lib/state.eu:21,54).
So the question is not *can* users write monads but *should we foreground it* —
which pulls HKT (B1) into ordinary users' view. **Recommendation: supported, not
foregrounded.** Monads remain a **prelude-assembled facility via `monad()`**,
typed once when HKT lands (B8), with user-defined monads a documented capability
for advanced users — not a headline feature, not a tutorial topic, not a thing
the language pushes. Rationale: B8 gives the *payoff* (correct derived-combinator
types) to anyone who calls `monad()` **without that user writing a single HKT
annotation** — the kind variable lives in `monad()`'s signature, not theirs. That
is the inference-first principle (non-negotiable #3) working exactly as intended:
the hard type lives in the prelude; the user gets typed `bind`/`map`/`sequence`
for free. Foregrounding user monads would invert that — pushing rank-2/kind
syntax onto users for a facility a config/data language rarely needs — for which
the cost-benefit plainly does not pencil. `state` should reconcile its existing
asserted annotation with the HKT `monad()` signature (B8.2) and stand as the
worked example; that is sufficient encouragement.

## Implementation sketch

Most of the build is B1/B2/B8, already specced and sequenced (one PR, B2
independent of B1, B8 gated on B1). This proposal's *own* additions are small:

1. **Boundary policy as doc + lint (~S).** Ratify §3 as the rule "a constraint
   references only an already-typed function". A cheap check in `parse.rs`/
   `check.rs` can warn if a constraint names an unknown function — catching typos
   and, by construction, refusing the "declare a new class" mistake.
2. **Constraint aliases (~S, deferred, optional).** A `constraints:`-metadata
   expansion in the DSL parser (`src/core/typecheck/parse.rs`), purely textual.
   Build only on evidence of repetition.
3. **`state` reconciliation + worked example (~S).** Land B8's HKT `monad()`
   annotation; reconcile `lib/state.eu`'s asserted `type:`; document `state` as
   *the* user-monad example.

Risk is low and concentrated in B1's representation rewrite (`Con`/`App`/`Forall`,
covered by B1's spec), not in anything new here. No STG/VM/GC change; types stay
erased.

## Alternatives considered

- **Full nominal type classes (Haskell/PureScript).** Rejected: violates
  non-negotiable #2, imports the entire coherence/orphan burden, and adds runtime
  dictionary semantics this advisory checker would have to erase anyway. The
  overload union already *is* the dictionary.
- **Scala-3-style scoped `given`/`using` with opt-in coherence.** Rejected:
  scoped implicit *search* (and its incoherence modes) is precisely the
  complexity B2's closed-overload-set design avoids. The flexibility buys nothing
  for a data-transformation tool.
- **Constraint *solving* (superclasses, fundeps, backtracking).** Deferred
  indefinitely: needed only if a class *hierarchy* exists, which it does not.
  Enumeration is the whole feature.
- **An opaque nominal alias for export contracts (open-question 1).** Deferred,
  not rejected: kept on the table as an *instance-free* name behind an edition
  gate, should structural distinctness prove a measured pain.

## Risks & what would kill this

- **Boundary creep.** The named failure mode: B2 ships, users (or a future
  contributor) ask for `implements`/instances "to make it complete", and the
  closed-overload-set discipline erodes into an open pool — at which point
  coherence/orphans arrive and the structural identity is gone. The §3 table and
  the one-line test are the guard; this document exists largely to make that
  creep an explicit, reviewable decision rather than a drift.
- **Constraints feel like a class anyway.** If users read `"<(a,a) =>"` as "an
  `Ord` class" and expect instance extensibility, the mental model mismatches the
  mechanism. Mitigation: documentation frames it as "find an overload that fits",
  and the worked examples stay operator/namespace-keyed.
- **HKT slips, so B8's free typing doesn't land.** Then open-question 3's
  recommendation weakens (no inherited monad types). B2 still ships
  independently; the user-monad story simply waits for B1.
- **Falsifier for the trade.** If real eucalypt programs turn out to want
  user-defined, cross-module, instance-style extensibility *often* — a pattern
  that would show up as repeated requests to "make my type comparable" — then the
  config/data framing is wrong and 0012's algebraic-subtyping route (which
  absorbs constraints into inference) becomes the better long bet. Nothing
  observed today suggests this.

## Success criteria

- `min`/`max`/comparison-generic functions in `lib/prelude.eu` carry honest
  constraint annotations and `eu check lib/prelude.eu` is clean (B2 acceptance).
- A user function that *propagates* a constraint type-checks; `min(blockA,
  blockB)` warns; an existing union-overload annotation still works (B2 test
  plan).
- A user monad built with `monad()` gets correct derived-combinator types with no
  user-written HKT annotation; `io`/`for`/`let`/`random`/`state` chains check
  (B8 test plan).
- The §3 boundary is ratified policy: no class, no `implements`, no global
  coherence, no dictionary passing — and a constraint naming an unknown function
  warns.
- Open questions 1 and 3 are *closed in the docs* with the recommendations above,
  unblocking the 0001 charter's G4 dependency on a coherent Stage-A/B type story.

## References

**Eucalypt:** `src/core/typecheck/types.rs:25-28` (`Constraint` representation),
`:35` (`TypeScheme.constraints` reserved/unused); `src/core/typecheck/check.rs:18-22`
(operators carry a `Union` of overloads), `:873` (`apply_union`);
`lib/prelude.eu:104` (`monad(m)`, nine derived combinators, lines 104-137), `:103`
(current `"{{..}} → {{..}}"` type), `:917,892` (`min`/`max`, currently
un-annotated), `:1742` (`str.of`); `lib/state.eu:59` (`state: monad{…}`), `:21,54`
(asserted `type:`); `docs/development/type-system-evolution.md` H1, H10, §5
open-questions 1 & 3; `docs/development/operator-constraints-and-monad-types-spec.md`
(B2/B8); `docs/development/higher-kinded-types-spec.md` (B1); sibling
[0001](0001-v1-charter.md) (export contracts, editions), [0012](0012-algebraic-subtyping-fork.md)
(MLsub/MLstruct fork).

**External:** Wadler & Blott, *How to make ad-hoc polymorphism less ad hoc*
(POPL 1989) — dictionary passing; ezyang, *Type classes: confluence, coherence
and global uniqueness* (https://blog.ezyang.com/2014/07/type-classes-confluence-coherence-global-uniqueness/);
Racordona, *On the State of Coherence in the Land of Type Classes*
(https://arxiv.org/pdf/2502.20546, 2025); *Coherence of Type Class Resolution*
(https://arxiv.org/pdf/1907.00844); Yallop & White, *Modular Implicits*
(https://arxiv.org/abs/1512.01895, 2015); Scala 3 *Contextual Abstractions*
(https://docs.scala-lang.org/scala3/reference/contextual/) — opt-in coherence,
scoped `given`/`using`; PureScript `Prim.Row` (`Cons`/`Union`/`Nub`,
https://pursuit.purescript.org/builtins/docs/Prim.Row) — row-class constraints;
*Can orphan instances cause bugs?* (https://discourse.haskell.org/t/can-orphan-instances-cause-bugs/13637).
