# 0011 — "Typeclasses without classes" maturation (beyond H10)

- **Status:** Draft proposal for review
- **Track:** C — type system & language (beyond the roadmap)
- **Classification:** Extends-Roadmap
- **Suggested horizon:** 0.8+
- **Related:** H1 (HKT) and H10 (structural operator constraints) (`docs/development/type-system-evolution.md`); TS-B2, TS-B8 (`docs/development/operator-constraints-and-monad-types-spec.md`); TS-B1 (`docs/development/higher-kinded-types-spec.md`); open-questions 1 & 3 (`type-system-evolution.md` §5); sibling proposals [0001](0001-v1-charter.md), [0009](0009-structural-contracts-validation.md), [0012](0012-algebraic-subtyping-fork.md), [0013](0013-type-dsl-embedding.md)

## Summary

Phase B *shipped* in 0.7.0: *ad-hoc polymorphism, the eucalypt way* is now live.
A function may be annotated `"<(a, a) => a -> a -> a"`, meaning "valid wherever
`<` accepts `a`", where the **dictionary is the operator's own union of declared
overloads** and resolution is **finite enumeration over those overloads** — no
class, no global instance database, no dictionary passing. The mechanism is real
code (B2 built it; B1/B8 supplied the HKT it pairs with): `min`/`max` carry these
annotations today and `monad()` carries a higher-kinded `forall (m :: * -> *)`
type. This proposal therefore no longer argues *whether* to ship the idiom; it
**maps how far the shipped idiom should mature, and where it must stop** to
preserve structural-over-nominal. The thesis: structural finite-enumeration
resolution has *no orphan/coherence problem* and stays simple and local, at the
cost of instance-based extensibility and law-checked coherence — and for a
config/data language that is the *right* trade. Concretely it answers two of the
maintainer's open questions: **(1)** no, "no nominal" need not be breached for
export contracts; **(3)** user monads are now *supported but should not be
foregrounded*, remaining a prelude-assembled `monad()` facility, typed once for
all now that HKT has landed.

## Motivation

Eucalypt now has both halves of ad-hoc polymorphism. The first half it has had
for a while: operators carry a **union type covering all overloads**, and "when
an argument is applied to a union-typed function, the checker tries each variant
in order and commits to the first that unifies" (`src/core/typecheck/check.rs:18-22`;
the engine is `apply_union`). The second half landed in 0.7.0:
`TypeScheme.constraints` is a `Vec<Constraint>` that is now **live** — populated
from annotations and discharged by the checker (`src/core/typecheck/types.rs:143`;
written at `check.rs:877`, freshened at `check.rs:1378`, discharged by
`discharge_constraint`, `check.rs:587`). The `Constraint` representation —
`{ function: String, args: Vec<Type> }` (`types.rs:133-136`) — is no longer a
parked placeholder.

The user pain B2 removed is concrete. `min` and `max` were until 0.6.2 written
`min(l, r): if(l < r, l, r)` / `max(l, r): if(l > r, l, r)` with **no `type:`
annotation at all** — because the only honest annotation available was an
exhaustive union of every comparable pair
(`number→number→number | string→string→string | …`), both verbose and
incomplete. They now carry the one-line constraint that says the true thing:
`max` is `">(a, a) => a → a → a"` (`lib/prelude.eu:891-892`) and `min` is
`"<(a, a) => a → a → a"` (`lib/prelude.eu:916-917`) — the shipped poster child
for structural constraints. The same gap recurs for any namespace-generic
function: `labels: map(str.of)` is honestly `"str.of(a) => [a] -> [string]"`, and
`str.of` is a real prelude function (`lib/prelude.eu:957-959`), not a class
method.

The strategic gap is larger than `min`. Eucalypt's identity rests on **structural
over nominal** (non-negotiable #2). Operator constraints are the first feature
that *looks*, from a Haskell vantage, like a type class — and so the first place
the structural discipline could quietly erode into a nominal one. Now that the
code has landed, this document's job is to hold that line, so the feature stays
"typeclasses without classes" and does not drift into "classes, eventually". It
is also where two parked open questions (§5.1, §5.3) become unavoidable, since
both turn on how far this shipped idiom is pushed.

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
| **eucalypt 0.7** | **an operator's union of overloads** | **finite enumeration, left-to-right, no backtrack** | **N/A — no global search to be incoherent** |

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

Now that B2/B8 have shipped, the design is mostly *boundaries*: hold the line on
the mechanism that landed, bless a small set of disciplined extensions to mature
in 0.8+, and name a hard stop.

### 1. The mechanism, as shipped (B2 + B8)

A constrained scheme is a comma-separated constraint list before `=>`, then a
body type, all inside a `type:` string — or, per [0013], an `s"…"` literal,
which (interpolation off) lets record types inside drop the `{{..}}`
brace-doubling the plain string needs. The DSL parses these today
(`parse::parse_scheme`, `src/core/typecheck/parse.rs:1208`):

```
` { type: "<(a, a) => a -> a -> a" }              # min/max (live in the prelude)
` { type: "<(a, a), +(a, a) => a -> a -> a" }     # needs both < and +
` { type: "str.of(a) => [a] -> [string]" }        # namespace-function constraint
```

`<(a, a)` is `Constraint { function: "<", args: [Var(a), Var(a)] }`
(`types.rs:133-136`). Resolution, as implemented:

- **Overload set.** The set of `Function` types `name` is annotated with — one
  `Function` is one overload, a `Union` of them is several. Read from `name`'s
  binding by **flat name** (`discharge_constraint` keys the overload map on the
  binding name, `check.rs:608-621`; the map is built from Let-binding names,
  `cook/fixity.rs:35-37`), reusing the existing union machinery (`apply_union`).
  This flat-name lookup is why a *dotted* head needs an extension (§2).
- **Discharge.** When the scheme is instantiated and the constraint's `args`
  resolve to concrete types, `discharge_constraint` (`check.rs:587`) succeeds iff
  some overload accepts them (`is_consistent`); else a **warning** (never an
  error — non-negotiable #3).
- **Propagation.** If an `arg` variable is still unbound, the constraint is
  carried into the enclosing scheme's `constraints` vector and freshened with the
  body on each use (`check.rs:1378`). Constrained polymorphism composes; multiple
  constraints are a left-to-right conjunction sharing only the type variables the
  rest of the signature already unifies. *No inter-constraint interaction, no
  backtracking.*
- **Gradual.** An `any` argument satisfies any constraint vacuously; the
  boundary stays silent.

B8 shipped alongside: with B1's HKT, `monad()` now carries its real rank-N
signature `"!forall (m :: * -> *). {{..}} → {{…}}"` over `m :: * -> *`
(`lib/prelude.eu:92-102`), returning a record of its **nine** derived combinators
(`bind`, `return`, `map`, `then`, `and-then`, `join`, `sequence`, `map-m`,
`filter-m`). Crucially, higher-order pattern (Miller-fragment) unification
(`Type::Lam`; `src/core/typecheck/unify.rs:153-200`) makes this work for *any*
monad by pure unification — including monads whose action type is not a bare
constructor (e.g. `state`'s `state → {value, state}`): when the `* -> *` variable
is applied to a type variable and unified against a concrete type, the unifier
abstracts the variable out to build the type-level function, with no
special-casing. Because `io`, `for`, `let`, `random` (`lib/prelude.eu:30,2145,2137,158`)
and user monads like `state` (`lib/state.eu:54`) are each *built by calling
`monad()`*, they **inherit** typed combinators "for free" — the annotation is
written once, in `monad()`.

### 2. Disciplined extensions worth maturing (0.8+)

These deepen the shipped idiom without crossing into nominal territory:

- **Multi-parameter constraints (shipped).** `"<(a, a), +(a, a) => …"`. The
  representation already allows it (`Vec<Constraint>`) and the DSL parses it
  (`parse.rs:1208`, tested at `parse.rs:2036`). This is the structural analogue
  of `MultiParamTypeClasses`, minus the class; the maturation work is exercising
  it across the prelude rather than building new machinery.
- **Namespace-function constraints (parse-only today — needs a discharge
  extension).** `"str.of(a) =>"`, `"io.bind(m) =>"`. The constraint references a
  dotted prelude path, not a class membership — a clean generalisation that types
  `map(str.of)`-shaped functions honestly. But it is **not yet wired up.** The DSL
  *parses* a dotted head into `function: "str.of"` (`parse.rs:2045,2048`), yet
  `discharge_constraint` resolves the overload set by **flat binding name**
  (`check.rs:608-621`) and that map is keyed by Let-binding names, never by a
  dotted path (`cook/fixity.rs:35-37`). So today a dotted constraint finds no
  overload set and falls through to the silent "unknown operator" branch
  (`check.rs:625-628`) — it is **vacuously satisfied**, checking nothing. Maturing
  it requires real (if small) work: teach discharge to resolve a dotted head to
  the namespace block's field type. This is the one place the "bulk has shipped,
  residual is exercise-only" framing over-claims.
- **Constraint aliases (new, optional, 0.8+).** A recurring conjunction —
  say `"<(a,a), >(a,a), =(a,a)"` — could earn a DSL-level *alias*: a
  `constraints:` metadata entry binding, say, `Comparable(a)` to that
  conjunction, expanded by the parser. This is **textual abbreviation only**: it
  introduces no class, no membership, no instance; the alias is sugar that expands
  to the same finite-enumeration checks. **Avoid class-famous names** (`Ord`,
  `Eq`, `Num`): they maximally invite the "this is a type class" misreading this
  document fights — pick a plainly descriptive name. Note too that a constraint
  alias is a **distinct kind** from a type/spec alias: a constraint is a predicate
  over type *variables*, so — unlike a [0009] type alias — it is **not reifiable**
  (there is no `validate(s"Comparable(a)", x)`; you cannot run a constraint over a
  value). That is exactly why `constraints:` is a separate namespace from `types:`,
  not a redundant third alias channel. It buys readability for the handful of
  multi-op numeric/comparison routines and nothing more. *Recommended only if real
  annotations show the conjunctions repeating;* skip otherwise.

### 3. The hard boundary — what eucalypt must NOT add

| Tempting | Why refused |
|---|---|
| **Nominal classes / `class Ord a`** | Breaks non-negotiable #2 outright. The overload set already *is* the dictionary; a class adds a name and a global pool for no expressive gain. |
| **`implements` / instance declarations** | Reintroduces the open, accumulating instance database — and with it orphans, coherence, and an import-order-sensitive search. The whole point is that there is no pool to declare into. |
| **Global instance coherence / orphan rules** | Solves a problem eucalypt does not have. There is one overload set per operator, fixed at its binding; uniqueness is structural, not enforced. |
| **Runtime dictionary passing** | Defeats erasability (the checker is advisory and types vanish before STG). Resolution is a *static* discharge, not a value threaded at runtime. |
| **Constraint *solving* (backtracking search, superclasses, fundeps)** | What shipped is deliberately *enumeration*, not *solving*. Superclasses and functional dependencies are the machinery a class hierarchy needs; with no hierarchy they are dead weight, and they are exactly where coherence proofs get hard (Racordona 2025). |

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

- **Builds on shipped B2.** Supersedes nothing; it *frames* the landed feature
  and adds the boundary policy and the optional constraint-alias extension. B2's
  forwards compatibility holds — a union-overload annotation is a strict
  specialisation of a constraint.
- **Builds on shipped B1/B8 (HKT).** B8's "user monads inherit typed combinators
  for free" is the linchpin of the open-question-3 recommendation, and it now
  holds in code: `monad()` carries the kind-polymorphic type and Miller-pattern
  unification discharges it for any monad.
- **Sibling [0001](0001-v1-charter.md)** owns *stability*; its open-question-1
  reading ("a 1.0 export contract is a structural promise … structural typing
  already expresses it") is the conclusion reached here from the type-design side.
  **Sibling [0012](0012-algebraic-subtyping-fork.md)**: an MLsub/MLstruct rebuild
  would make constraints, unions and rows fall out of *one* algorithm, subsuming
  the bespoke enumeration that shipped — so this proposal's boundary (no class, no
  solving) is also the constraint on what 0012 must preserve.

### Open question 1 — is "no nominal" inviolable? (the export-contract newtype)

The floated exception (§5.1) is "a small opt-in nominal newtype mechanism …
tolerated for export contracts" — a way to say "this value is a `Port`, not just
a `number`" at a unit's boundary. **Recommendation: stay structural.**

A newtype would *buy* nominal distinctness at a boundary (a `Port` not
interchangeable with a `Timeout` though both are `number`) and a stable contract
name. It would *cost* the first nominal entity in the language — which at once
raises "where are its instances declared?", reopening every coherence/orphan
question just closed — plus an edition-gated breaking surface (per 0001) and a
second, fragmenting way to model data. And the benefit is reachable
structurally — now concretely, via [0009]: an export contract *is* a structural
spec, so a `Port` is a refined spec (an `s"number"` plus a `refine` predicate for
the valid range) validated at the boundary, and a literal-symbol-tagged record
`{tag: :port, value: number}` gives a distinguishable *shape* without a name.
[0001]'s editions supply the stability lever. The newtype's *only* irreducible gain is
**distinctness without a discriminator field** — nice, but not worth being the
thin end of nominal typing. Keep it structural; if boundary distinctness becomes a
measured pain, revisit as an edition-gated, instance-free *opaque alias* — a
name, no class, in practice a named [0009] spec / [0013] `s"…"` alias — never a
class system.

### Open question 3 — encourage user-defined monads? (HKT in the user's lap)

User monads are **fully realised**: `state` is a user-importable monad assembled
by `state: monad{bind: state-bind, return: state-ret}` (`lib/state.eu:54`) and it
no longer needs *any* `type:` annotation of its own — as of 0.7.0 it carries only
`monad:` metadata for LSP hints and inherits all nine combinator types from
`monad()` via HKT unification. So the question was never *can* users write monads
but *should we foreground it* — which pulls HKT (B1) into ordinary users' view.
**Recommendation: supported, not foregrounded.** Monads remain a
**prelude-assembled facility via `monad()`**, now typed once via HKT (B8), with
user-defined monads a documented capability for advanced users — not a headline
feature, not a tutorial topic, not a thing the language pushes. The shipped
mechanism vindicates the recommendation: B8 gives the *payoff* (correct
derived-combinator types) to anyone who calls `monad()` **without that user
writing a single HKT annotation** — the kind variable lives in `monad()`'s
signature, not theirs, and the unifier even abstracts over `state`'s
non-constructor action type (`state → {value, state}`) automatically. That is the
inference-first principle (non-negotiable #3) working exactly as intended: the
hard type lives in the prelude; the user gets typed `bind`/`map`/`sequence` for
free. Foregrounding user monads would invert that — pushing rank-N/kind syntax
onto users for a facility a config/data language rarely needs — for which the
cost-benefit plainly does not pencil. `state` already stands as the worked
example, with its asserted annotation now retired in favour of inherited types;
that is sufficient encouragement.

## Implementation sketch

The bulk of the build — B1/B2/B8 — has shipped in 0.7.0 (`Con`/`App`/`Kind`/`forall`,
Miller-pattern unification, live `TypeScheme.constraints`, HKT-typed `monad()`).
This proposal's *own* residual additions are small:

1. **Boundary policy as doc + lint (~S).** Ratify §3 as the rule "a constraint
   references only an already-typed function". A cheap check in `parse.rs`/
   `check.rs` can warn if a constraint names an unknown function — catching typos
   and, by construction, refusing the "declare a new class" mistake. (The discharge
   path, `check.rs:587`, already locates the operator's overloads; the lint is a
   complementary up-front check.)
2. **Constraint aliases (~S, deferred, optional).** A `constraints:`-metadata
   expansion in the DSL parser (`src/core/typecheck/parse.rs`), purely textual.
   Build only on evidence of repetition.
3. **Dotted-constraint discharge, then exercise across the prelude (~S–M).**
   `min`/`max` already carry (operator) constraints. *Namespace*-generic
   annotations (`map(str.of)`-shaped) first need discharge taught to resolve a
   dotted head to the namespace block's field type (see §2) — until then they
   parse but check nothing. Then extend honest annotations across the prelude and
   keep `state` documented as *the* user-monad worked example. Annotations are
   written as [0013] `s"…"` literals (no `{{..}}` doubling).

Risk is now low: the representation rewrite that carried it (`Con`/`App`/`Forall`)
is done and tested. No STG/VM/GC change; types stay erased.

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

- **Boundary creep.** The named failure mode: B2 has shipped, and now users (or a
  future contributor) ask for `implements`/instances "to make it complete", and
  the closed-overload-set discipline erodes into an open pool — at which point
  coherence/orphans arrive and the structural identity is gone. With the mechanism
  live this is the *live* risk, not a hypothetical one. The §3 table and the
  one-line test are the guard; this document exists largely to make that creep an
  explicit, reviewable decision rather than a drift.
- **Constraints feel like a class anyway.** If users read `"<(a,a) =>"` as "an
  `Ord` class" and expect instance extensibility, the mental model mismatches the
  mechanism. Mitigation: documentation frames it as "find an overload that fits",
  and the worked examples stay operator/namespace-keyed.
- **HKT proves over-general in practice.** Now that B8's free typing has landed,
  the residual worry is the opposite of a slip: that kind-polymorphic `monad()`
  plus Miller-pattern unification under-constrains some user spec and silently
  accepts a malformed monad. Mitigation is the worked-example coverage
  (`io`/`for`/`let`/`random`/`state`) and the `monad:` element-type hints that keep
  LSP diagnostics honest.
- **Falsifier for the trade.** If real eucalypt programs turn out to want
  user-defined, cross-module, instance-style extensibility *often* — a pattern
  that would show up as repeated requests to "make my type comparable" — then the
  config/data framing is wrong and 0012's algebraic-subtyping route (which
  absorbs constraints into inference) becomes the better long bet. Nothing
  observed today suggests this.

## Success criteria

0.7.0 already meets the core acceptance bar (`min`/`max` carry honest constraint
annotations, `lib/prelude.eu:891-892,916-917`; a user monad built with `monad()`
gets correct derived-combinator types with no user-written HKT annotation, with
`io`/`for`/`let`/`random`/`state` all assembled from it). The maturation bar for
0.8+ is:

- Comparison-generic functions beyond `min`/`max` carry honest constraint
  annotations and `eu check lib/prelude.eu` is clean; *namespace*-generic
  constraints discharge for real (not vacuously) once dotted resolution lands.
- A user function that *propagates* a constraint type-checks; `min(blockA,
  blockB)` warns; an existing union-overload annotation still works (regression
  coverage holds).
- The §3 boundary is ratified policy and *defended now that the code exists*: no
  class, no `implements`, no global coherence, no dictionary passing — and a
  constraint naming an unknown function warns.
- Open questions 1 and 3 stay *closed in the docs* with the recommendations above,
  unblocking the 0001 charter's G4 dependency on a coherent Stage-A/B type story.

## References

**Eucalypt:** `src/core/typecheck/types.rs:133-136` (`Constraint` representation),
`:143` (`TypeScheme.constraints`, now live); `src/core/typecheck/check.rs:18-22`
(operators carry a `Union` of overloads), `:587` (`discharge_constraint`), `:877`
(constraints populated from annotation), `:1378` (constraints freshened with the
body); `src/core/typecheck/unify.rs:153-200` (higher-order/Miller-pattern
unification, `Type::Lam`); `src/core/typecheck/parse.rs:1208` (`parse_scheme`, the
`=>` constraint DSL); `lib/prelude.eu:92-102` (`monad(m)` with its HKT
`"!forall (m :: * -> *). …"` type, nine derived combinators), `:891-892,916-917`
(`max`/`min`, now `">(a, a) => …"` / `"<(a, a) => …"`), `:957-959` (`str.of`),
`:30,2145,2137,158` (`io`/`for`/`let`/`random` assembled from `monad()`);
`lib/state.eu:54` (`state: monad{…}`, no own `type:` — types inherited);
`docs/development/type-system-evolution.md` H1, H10, §5 open-questions 1 & 3;
`docs/development/operator-constraints-and-monad-types-spec.md` (B2/B8);
`docs/development/higher-kinded-types-spec.md` (B1); `CHANGELOG.md` 0.6.2/0.7.0;
sibling [0001](0001-v1-charter.md) (export contracts, editions),
[0012](0012-algebraic-subtyping-fork.md) (MLsub/MLstruct fork).

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
