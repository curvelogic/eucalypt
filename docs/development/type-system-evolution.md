# Possible Evolutions of Eucalypt's Gradual Type System

**Status**: Exploratory design notes — not a spec, not a commitment.
**Date**: 2026-05-17
**Branch**: `claude/summarize-open-beads-M5tsa`

This document gathers hypotheses about how eucalypt's gradual type system
could evolve beyond the current spec ([gradual-typing-spec.md](./gradual-typing-spec.md))
and the monad metadata follow-up ([monad-type-checking-spec.md](./monad-type-checking-spec.md)).
Each hypothesis is sketched in enough detail to argue about. Nothing here
is decided.

Throughout, two non-negotiables apply:

- **Syntactic conservatism** — eucalypt's surface syntax stays as it is.
  New type machinery must live in metadata strings, symbols, blocks, or
  thin extensions of what already exists.
- **Structural over nominal** — no named classes, no `implements` clauses,
  no nominal types. Constraints and interfaces are shapes, not names.

The arrangement is: a brief look at what we have today, then eighteen
hypotheses grouped roughly by ambition, then a suggested ordering, then
cross-cutting themes.

---

## 0. Where we stand

The current system is a freshen-and-unify bidirectional checker with
subtyping. Its representation already accommodates type variables,
polymorphic schemes (`forall a b. ...`), open and closed records with
named row variables, literal symbol singletons, and opaque type
constructors for `IO(T)`, `Lens(a, b)`, `Traversal(a, b)`. The spec
reserves space for structural operator constraints in `TypeScheme`,
though the field is always empty today.

Inference is rank-1 prenex. There is no kind system, no constraint
resolution, no rank-N or rank-2 quantification, no recursive types, no
flow-sensitive narrowing, no use of types at runtime, no embedded type
language — annotations live in metadata strings. Two further gaps
worth naming because later hypotheses lean on them: there is **no way
to type a value's metadata channel** (so the van Laarhoven lens
internals cannot be typed honestly — H2), and **no homogeneous-block
type** — `Map(symbol, T)` / dictionary — distinct from named records
(H19).

The interesting unfinished business already on the spec's roadmap:
recursive types (section 12), named row variables in inference (section
2), structural operator constraints (section 14). The rest of this
document picks up where those leave off.

A reminder of two pieces of runtime reality that bound design:

1. **IO has dedicated data constructors** (`IoReturn`, `IoBind`,
   `IoAction`, `IoFail`) in the STG runtime — it is a genuine type, not
   a tagged block.
2. **Lenses are van Laarhoven-encoded with metadata-driven dispatch**:
   `(b -> f b) -> (a -> f a)` where the inner function carries `fmap`,
   `pure`, `ap` as metadata. The "functor" is chosen at the boundary,
   not by the lens. Any honest type for the *internals* must reckon
   with this.

---

## 1. Hypotheses

The hypotheses are numbered for cross-reference, not ranked. A summary
ordering is in §2.

### H1. Higher-kinded types for `monad()` and friends

**Motivation.** The flagship case is `monad(m)`: a function that takes
a record with `bind` and `return` and returns a record with eight
derived combinators, where every signature is polymorphic in the
*same* type constructor. Today the prelude annotates it as
`{..} -> {..}` — almost no information. With higher-kinded types it
could be expressed properly.

**Sketch.** With a kind system that admits constructors as well as
types:

```
` { type: "{bind: (m a -> (a -> m b) -> m b),
            return: (a -> m a)}
            -> {bind: m a -> (a -> m b) -> m b,
                return: a -> m a,
                map: (a -> b) -> m a -> m b,
                then: m b -> m a -> m b,
                join: m (m a) -> m a,
                sequence: [m a] -> m [a],
                map-m: (a -> m b) -> [a] -> m [b],
                filter-m: (a -> m bool) -> [a] -> m [a]}" }
monad(m): ...
```

The variable `m` ranges over `* -> *` constructors. The full quantifier
prefix is `forall (m :: * -> *) a b. ...`, with each combinator scheme
nested. Strictly this is rank-2 inside a record. We avoid the worst of
the inference horror because the user (or prelude) writes the
annotation; we only need to *check* it, and freshen on use.

**Eucalypt-friendly framing.** There is no dictionary passing and no
implicit resolution. The "type class" is just the block that `monad()`
receives. This is closest to *records-of-functions* (Idris 2 interfaces
desugared, OCaml first-class modules, Scala 3 given/using when no
implicit resolution is needed). The kind variable is the only addition
beyond what we have.

**Syntactic notes.** The DSL change is small: `m a` (juxtaposition) for
constructor application. This collides with our own catenation, but
inside a type string there is no catenation operator to worry about. To
distinguish higher-kinded variables from ordinary ones, two options:

1. Convention: variables that appear in head position of an application
   in a signature are inferred to be of kind `* -> *`.
2. Optional explicit kinds: `forall (m :: * -> *) a. ...`. The
   `forall` is implicit at present; only kinds need new syntax.

Convention works for the common cases. Where ambiguous (e.g.
`a a`), the parser can require an explicit kind.

**Cost.** Medium-high. The type representation needs `App(Type, Type)`
and `Kind` (just `Type` and `Arrow Kind Kind` to start). The unifier
needs kind-aware variable allocation. Inference is unchanged for
annotated functions. Display gets one new case.

**Prior art.** Haskell type classes (Wadler & Blott 1989), GHC's
`MultiParamTypeClasses`, PureScript's row-based constraint system,
Idris 2 interfaces (Brady 2021), OCaml first-class modules,
Scala 3 given/using.

**Payoff.** `monad()` and `map-m` etc. typed properly. User monads
inherit correct types. The lens internals (H2b) become writable.

---

### H2. Typing the lens encoding

The current spec types `Lens` and `Traversal` *opaquely* — at the
interface. That catches composition and view/over misuse. It cannot
catch errors inside the lens library itself, and cannot describe the
internal contract of "the `(b -> f b)` argument carries `fmap`
metadata".

**The hard prerequisite: typing metadata.** Before any of the routes
below, note a gap the rest of this hypothesis depends on. The type
representation today has *no way to talk about a value's metadata
channel at all*. The gradual-typing-spec defers this explicitly
(§14, "Metadata-Typed Values"). Eucalypt's lens encoding is not a
real van Laarhoven lens — there is no functor `f` at runtime. The
"functor" is a *metadata-dispatch fiction*: the inner function carries
`fmap`/`pure`/`ap` as metadata, and `view` vs `over` differ only in
which metadata block they attach (`fmap-get` vs `fmap-set`). The
value `f b` is representationally just `b` with a dispatch table
hanging off it. So any honest internal typing must be able to say
"a function value whose metadata is a block of shape
`{fmap: …, pure: …, ap: …}`". Until the type system can attach types
to the metadata channel, H2b–H2d are not implementable — not merely
hard. Metadata typing is a feature in its own right (it would also
benefit fixity/precedence metadata, `target`, and the `type:` field
itself) and is a genuine precondition here, separate from HKT.

Four routes, increasing in ambition:

#### H2a. Stay opaque (current plan)

`Lens(a, b)` and `Traversal(a, b)` as opaque constructors with hand-
written `at`, `ix`, `item`, `each` signatures. We have this; it works
for users.

#### H2b. van Laarhoven internals — the Haskell-idealised type

The textbook type:

```
Lens(s, t, a, b) = forall (f :: * -> *). Functor f => (a -> f b) -> (s -> f t)
```

This needs three things we don't have — *and* a fourth we'd be wrong
to ignore:

1. **Higher-kinded variables** (H1).
2. **Rank-2 quantification** — `forall f` inside the alias body, used
   wherever the lens is *applied*.
3. **A way to spell `Functor f`** without introducing typeclasses.
4. **Metadata typing** (the prerequisite above) — because in eucalypt
   there is no `f`. The "functor" is the dispatch table on the inner
   function's metadata.

Point (3) on its own fits eucalypt nicely: a structural constraint
reading "the metadata block provides `fmap` of the right shape" —
i.e. a row constraint `{fmap: (a -> b) -> f a -> f b, ..}`, Functor as
namespace shape rather than class. But point (4) is the one that
bites: the type `(a -> f b) -> (s -> f t)` describes a runtime that
does not exist. The eucalypt-honest type is closer to

```
Lens(a, b) = (b -@F-> b) -> (a -@F-> a)
```

where `-@F->` reads "function carrying functor-dispatch metadata of
shape `F = {fmap, pure, ap}`". HKT and rank-2 are needed to *say* `F`
abstractly; metadata typing is needed to *attach* it. So H2b is
really "metadata typing + structural constraints + HKT + rank-2"
together. That is a large package for a payoff (checking the bodies
in `lib/lens.eu`) that affects one library file.

#### H2c. Profunctor optics

`Lens s t a b = forall p. Strong p => p a b -> p s t`. Same machinery
(HKT + rank-2 + structural constraint) but with a profunctor record
instead of a functor record. More compositional. Probably not worth
the conceptual surface area for eucalypt users.

#### H2d. Effect-tracked metadata

Track required metadata directly in the type:

```
Lens(a, b) @ { fmap, pure, ap } = (b -> f b @ {..}) -> (a -> f a @ {..})
```

Metadata as effects. Cute but conflates two ideas. Probably worse than
H2b.

**Recommendation.** H2a for users — indefinitely. Treat H2b as gated
on metadata typing, which is a substantial feature of its own and
should be justified on its own merits (it touches far more than
lenses). Until metadata typing exists, the lens library *body* stays
`any`-typed; only the *interface* (`Lens(a, b)`, `Traversal(a, b)`)
is typed, and that is enough for users. The realistic near-term
question is not "type the lens internals" but "is metadata typing
worth doing for its own sake?" — if yes, H2b follows cheaply
afterwards; if no, H2b stays parked.

**Resolved (2026-05-19).** Both H2b (bead TS-B4 — typed lens internals)
and metadata typing itself (bead TS-B3) are **won't-do**. The original
plan gated them together. TS-B4's one-file payoff does not justify the
HKT + constraints package; and with no other consumer that needs
metadata to *flow* through the type lattice, metadata typing has no
justification of its own. A fallback metadata-validation lint was
considered and also dropped — it overlaps validation the cook and
loader passes already perform. `Lens`/`Traversal` stay opaque for
users; the metadata channel stays untyped.

**Prior art.** Edward Kmett's `lens` and its profunctor cousin (van
Laarhoven 2007; Bartosz Milewski's lens posts), "Profunctor Optics:
Modular Data Accessors" (Pickering, Gibbons, Wu 2017), "Don't Fear the
Profunctor Optics" (Boisseau & Gibbons), Glassery library.

---

### H3. Finish row polymorphism

`Type::Record` already has a `row: Option<TypeVarId>`. The display code
prints `{k: T, ..r}`. The parser accepts the syntax. The unifier knows
how to handle row vars in one direction. What's missing is
*actually inferring through row variables* in user code.

**Why it matters.** Most block-processing functions are row-polymorphic
in their input. With named rows we can express:

```
` { type: "{..r} -> {..s} -> {..r, ..s}" }
merge: __MERGE

` { type: "Lens({k: a, ..r}, b) -> (a -> a) -> {k: a, ..r} -> {k: a, ..r}" }
over-at: ...
```

**What row polymorphism does *not* solve: `map-values`.** An earlier
draft tried `(symbol -> a -> b) -> {..r} -> {..r}[a/b]`. That is wrong.
`map-values` applies a function to every value, so for a *heterogeneous*
block like `{x: number, y: string}` there is no single `a`: each field
instantiates the value type differently, and the result row is "the
input row with each field type `T` rewritten to the function's result
on `T`". That is a *type-level map over a row* — a higher-order
operation on the row itself, which row polymorphism alone does not
give you (PureScript needs `RowToList` plus type families for it).

The clean way out is *not* row polymorphism but a **homogeneous block
type** — `map-values` types trivially as `(symbol -> a -> b) ->
Dict(a) -> Dict(b)` when the block is known to have uniformly-typed
values. See H19. Row polymorphism is the right tool for `merge` and
for row-*preserving* functions like `over-at`; homogeneous-block
typing is the right tool for `map-values`. They are complementary.

**Two row algebras to choose from.**

- **Wand / Rémy "absent" labels**. Each row variable carries a set of
  labels it *cannot* contain. Unification checks disjointness. Strong
  inference, used in OCaml polymorphic variants and original Wand papers.
- **Daan Leijen "scoped labels"**. Labels can repeat; the row is an
  ordered list. Subsumes left-biased merge directly. Used in Koka. Maps
  beautifully to eucalypt's "rightmost wins" merge.

Eucalypt's merge semantics make Leijen's scoped labels the natural fit.

**Cost.** Medium. Unifier extension is well-documented. Inference of
row vars at lambda boundaries is the new bit, but the bidirectional
machinery localises it.

**Prior art.** Wand (1987), Rémy "Type inference for records in a
natural extension of ML" (1994), Pottier "A versatile constraint-based
type inference system" (2000), Leijen "Extensible Records with Scoped
Labels" (2005), PureScript record-row, Koka effects-as-rows, MLstruct
(Parreaux et al. 2022).

**Recommendation.** Take this in Stage A. Without it the existing
`Lens` opaque types feel under-typed (e.g. `over` should preserve row
content).

**Scope split (per the A1 spec).** Stage A / 6.1 covers *annotated*
row-polymorphic types only: the unifier already propagates row content,
so annotating `merge` and the lens `over`-family activates it (the main
remaining task is freshening row variables per use). Inferring *fresh*
row variables for *unannotated* functions — so a generic block
combinator becomes row-polymorphic without an annotation — is deferred
to Phase B, bead **TS-B9**. See
[row-polymorphism-and-dict-spec.md](./row-polymorphism-and-dict-spec.md).

---

### H4. Restricted dependent types where they pay rent

Full dependent types are out — too heavy, too unfamiliar, no demand.
But four restricted forms each give significant power for small cost.

#### H4a. Indexed access on records (Π over `Symbol`)

`b.foo` where `b : {foo: T, ..}` returns `T`. Already done. The
*dependent* version: `lookup(k, b)` where `k : LiteralSymbol("foo")`
and `b : {foo: T, ..}` returns `T`. The result type depends on the
literal value of `k`.

A correction over an earlier draft: `lookup` does **not** return
`null` on a missing key — it raises an execution error. (The
existence of `lookup-or` with an explicit default is the tell.) That
makes the dependent type *more* valuable, not less:

```
` { type: "(k :: symbol) -> {..r} -> r[k]" }
lookup: __LOOKUP

` { type: "(k :: symbol) -> d -> {..r} -> r[k] | d" }
lookup-or: __LOOKUPOR
```

The three cases for `lookup`:

- **literal `k` present in the row** → result is exactly `r[k]`.
- **literal `k` absent from a closed/known row** → the checker can
  issue a *static warning* — `lookup(:naem, person)` becomes a typo
  caught before it errors at runtime. This is the real prize.
- **non-literal `k`** → result is `any`, and the call carries a
  latent runtime error (a `Partial` flavour — see H6b).

This is TypeScript's `T[K]` indexed access, but the missing-key story
is sharper because eucalypt errors rather than widening to
`undefined`. H4a therefore depends on literal types (H16) to be worth
much — together they turn block-key typos into warnings.

#### H4b. Indexed access on tuples and short lists

`tuple !! n` where `n` is a literal. Same idea, integer index.

#### H4c. Computing monad result types from metadata

The `monad: "[a]"` annotation on `for` is already a tiny dependent
type — the *value* of the metadata determines the *type* of the
desugared bindings. The desugarer reads the string and emits a type
hint (this is the mechanism in monad-type-checking-spec.md).

**Is this still valuable once we have HKT (H1)?** Mostly *not*, for
type-checking purposes. Once H1 types the monad namespaces properly,
`for.bind : [a] -> (a -> [b]) -> [b]`, `io.bind : IO(a) -> …`, etc.,
the desugared `bind` chain type-checks *directly* — a wrong binding
fails because `42` does not unify with `[a]`. The explicit
`__type_hint` injection becomes redundant for checking.

What remains after HKT is the **non-type role** of the `monad:`
field: it tells the *desugarer* a block is monadic and which
namespace's `bind`/`return` to use, and it drives LSP inlay hints and
hover. That role is unaffected by HKT. So: H4c's hint-injection is a
*bridge* — worth doing now because HKT is far off, superseded for
checking once HKT lands, with the metadata field itself living on for
desugaring and tooling.

#### H4d. Lens path types — not viable

An earlier draft floated typing `‹:items 0 :meta›` dependently on the
literal symbols and indices it contains. This does not work: path
elements are not restricted to literals. `to-lens` accepts
`if(x symbol?, at(x), if(x number?, ix(x), x))` — the third case is
"`x` is already a lens", so a path may contain arbitrary lens
*expressions* (`item(_.id = 1)`, `element(by-key(…))`, composed
optics). There is no literal vector to index over. The desugar-time
hint that composes the per-segment optic types is the only workable
approach, and it already handles the lens-function case by using that
segment's own type. Drop H4d.

**Recommendation.** Take H4a (with H16). H4b is a minor follow-on.
H4c is a bridge worth building now and retiring after H1. H4d is
dropped.

**Prior art.** TypeScript indexed access types and conditional types,
Idris 2 dependent records, F* refinement types, Liquid Haskell,
DependentHaskell proposal, Roy programming language.

---

### H5. Existential types

Existentials hide implementation. We use them all the time without
calling them that: `IO(T)`, `Lens(a, b)`, `Traversal(a, b)` are
existentials over their internal encoding. The user can't peek inside.

#### H5a. Module-like abstraction (already have)

Opaque type constructors give us exactly this. No syntactic addition
needed.

#### H5b. Heterogeneous lists

`[exists a. a]` for "list of any". Today `[any]`. Already there.

#### H5c. Bag-of-namespaces

Lists of monads each potentially different. Real use case unclear.

**Recommendation.** Don't add explicit existential syntax. Acknowledge
that opaque constructors *are* existentials. If a use case appears for
explicit `exists a. T(a)` in user code, revisit. Until then, this is
solved.

**Prior art.** Mitchell-Plotkin "Abstract Types Have Existential Type"
(1988), SML modules (Harper-Mitchell-Moggi), Haskell `forall a. Show a => a`,
F-ω.

---

### H6. Effect typing

We already split pure from IO via `IO(T)`. Where can finer-grained
effect typing earn its keep?

#### H6a. Pure/impure split — have it

Refusal to call IO-returning functions outside the monad. The opaque
`IO(T)` already enforces this.

#### H6b. Partial / "may fail" effect

A `Partial(T)` constructor for functions that can produce
`ExecutionError`s — `head`, `lookup` (if we want), `//=` assertions on
mismatch. Acts like Maybe at the type level. The runtime cost is zero
(errors propagate already); the benefit is documentation and a hint
for tooling. Probably write as `T?` in the DSL:
`head : [a] -> a?` (sugar for `a | execution-error`).

#### H6c. Determinism / capability tracking

Distinguish `Random(T)`, `Time(T)`, `Env(T)`, `Read(T)`. Useful for
*rendering pipelines* where reproducibility matters: a YAML render
that uses `time.now` is non-reproducible, and that's a static
property. Capability-passing in the eucalypt style: `io.random` is
already a capability you must thread.

#### H6d. Algebraic effects à la Koka

`int -> a {network, log}`. Massive change. The cost-benefit doesn't
pencil for a data-transformation language. Skip.

**Recommendation.** H6b (Partial) is the next IO-like opaque
constructor that earns its keep. It catches `head([])`-class bugs and
documents partial functions consistently. H6c is interesting for
render reproducibility; gather data before committing. H6d, skip.

**Prior art.** Koka (Leijen), Eff (Pretnar), Frank (Lindley), Unison
abilities, OCaml 5 effects, capability-based security (E, Pony).

---

### H7. Refinement types

Predicates over types: `{x : number | x > 0}`. Liquid Haskell, F*,
Refined TypeScript. Catches off-by-one, division by zero, key
presence, list bounds.

For eucalypt the question is the constraint algebra:

- **Lightweight**: a finite set of pre-declared refinements
  (`NonEmpty`, `Positive`, `Nonzero`, `Saturating`, …) as type
  constructors. Cheap to implement, useful, no solver needed.
- **Heavyweight**: arbitrary arithmetic predicates discharged by an
  SMT backend. Out of scope for a config-and-data tool.

*SMT* — Satisfiability Modulo Theories — means an automated solver
(Z3, CVC5, …) that decides logical formulas over built-in theories
such as integer/real arithmetic, arrays, and bit-vectors. Liquid
Haskell and F* lean on one to prove that refinement predicates (e.g.
`0 <= i && i < len(xs)`) hold at every call site. It is powerful but
heavy: a solver dependency, non-trivial latency, and predicates that
can be hard to debug when they fail. For eucalypt the lightweight
finite-refinement route avoids the solver entirely.

#### H7a. `NonEmpty([a])`

```
` { type: "NonEmpty([a]) -> a" }
head: __HEAD
```

Construction: `cons(_, xs)` results, or after a `nil?` narrowing
branch. Wide impact, small effort. The type is just a thin tag.

For **list literals** the cleaner route is via `Tuple`, not `NonEmpty`
directly. A non-empty list literal should synthesise as `Tuple` of its
element types at every arity (up to a practicality cap), because
`Tuple` is the most general principal type — it widens to `Tuple`
(same-arity tuple parameters), to `[T]`, *and* to `NonEmpty` via a
`Tuple <: NonEmpty` rule. This both feeds `NonEmpty` for free and
fixes a pre-existing cliff: the current synthesiser keeps only 2–4
element literals as tuples and downgrades the rest to `List`, so a
5+-element literal cannot be passed to a tuple parameter even though
its length is statically known. Beyond the cap the literal falls back
to `NonEmpty(<element union>)`. Precision carries through *unannotated*
functions by inference but stops — correctly — at `[T]` annotations
and length-erasing operations (`map`, `++`, recursion); see the
literal-types spec for the full treatment.

#### H7b. Constant-folded refinements

When the inliner can prove a literal value satisfies a refinement
(e.g. `42 != 0` for `Nonzero`), the warning is suppressed. This is
free if the inliner already knows about the value.

#### H7c. Branch-sensitive narrowing

After `if(xs nil?, ..., body)` the checker narrows `xs : [a]` to
`xs : NonEmpty([a])` in `body`. Pairs naturally with H15 — and shares
H15's "branching is not syntax" complication (see there).

**Recommendation.** H7a + H7c is a small package with disproportionate
value. Skip arithmetic refinements.

**Prior art.** Liquid Haskell (Vazou et al.), F* (Swamy et al.),
Refined TypeScript (Vekris), Granule (Orchard et al.), Datafun,
Dafny's preconditions.

---

### H8. Algebraic subtyping (MLsub) and bi-unification

The current core is "freshen, walk, unify with subtyping side checks".
Stephen Dolan's MLsub (2017) and Lionel Parreaux's simpler descendant
("Simple Algebraic Subtyping" ICFP 2020, then MLstruct) unify HM and
subtyping into a clean algorithm where:

- Every well-typed term has a principal type.
- Row polymorphism, structural constraints, and unions/intersections
  fall out for free.
- The algorithm is bi-unification — variables have a *bound* (upper
  for negative, lower for positive) rather than equating types.

For eucalypt the appeal is that several wished-for features (H3, H10,
H15 partial) become first-class outputs of the algorithm rather than
bolt-ons.

Caveats:

- MLsub does not extend cleanly to higher-rank or HKT. Recent work
  (MLstruct, MLscript) chips away at this but it is research-grade.
- Replacing the core checker is a year of careful engineering. The
  user-facing type *language* could stay the same; only the inference
  engine changes.

**Recommendation.** Keep this on the radar. Reassess in Stage C once
H1, H3, H10 are in. If complexity is creaking, look at MLstruct as a
rebuild target. If not, leave the current engine alone — it's
predictable.

**Prior art.** Dolan & Mycroft "Polymorphism, Subtyping, and Type
Inference in MLsub" (2017), Dolan's PhD thesis, Parreaux "The Simple
Essence of Algebraic Subtyping" (ICFP 2020), MLstruct (2022),
MLscript / Hegel (Parreaux et al.).

---

### H9. Per-module type summaries and persistence

Right now the checker runs per file with the prelude as a fixed base.
For a project of many files (a typical eucalypt application bundles
prelude + libraries + manifests), repeated re-checking is wasteful and
cross-file type information is not propagated except through annotated
exports.

**Sketch.** Per module:

1. Type-check, producing a *summary* file alongside (or in a cache):
   the inferred or annotated type of every exported binding.
2. Persist these summaries (content-addressed by the module hash).
3. Importers load summaries instead of re-checking.

This is the standard incremental-compilation pattern (rustc, Bazel,
Bloop). For the LSP, summaries are also the right unit of cross-file
inference: when an upstream module changes, only downstream summaries
need invalidation.

**Whole-program inference** (no annotations needed anywhere) is a
separate question. Cost is high; payoff in this gradual setting is
modest because unannotated code is already silently `any`. Skip it.

**Correction (2026-05-18).** The "cheap in-memory cache" first step is
not cheap *and* sound. `eliminate` (dead-binding pruning) runs **before**
the type checker, and prunes a different subset of the prelude for every
user file — so a prelude type environment cached from a *merged* check
is missing bindings for the next. A sound cache must check the prelude
*standalone* and *unpruned*. Bead **TS-A8** (the in-memory cache) is
**withdrawn** into **TS-B7**.

**Refinement (2026-05-19).** TS-B7 is scoped to the **prelude only**.
The prelude's types are stable (it is fixed, merged first, references
only itself); general per-*user*-module summaries are not guaranteed
stable under positional merge-override and offer a small payoff — out
of scope. B7 = check the prelude standalone once → cache a summary of
its binding schemes *and* type aliases → seed every user-file check.
LSP responsiveness is the motivation: for a tooling-first language the
checker exists to support writing eucalypt. See
[prelude-type-cache-spec.md](./prelude-type-cache-spec.md).

**Recommendation.** Phase B (TS-B7), prelude-scoped. Skip whole-program
inference and general per-user-module summaries.

---

### H10. Structural operator constraints

Already in the spec (gradual-typing-spec.md §14) as a future
direction. The shape:

```
` { type: "<(a, a) => a -> a -> a" }
min: ...
```

Read: "`a -> a -> a`, valid wherever `<` accepts `a, a`". The
constraint is keyed on the *operator name*, not a class. Concretely
this means at instantiation time the checker looks up the
declared overloads of `<` and finds one that matches.

This is closest to **type classes without classes**: the dictionary is
the operator's union of overloads, and resolution is "find a member of
the union that fits". For eucalypt namespaces it generalises:

```
` { type: "str.of(a) => [a] -> [string]" }
labels: map(str.of)
```

means "valid wherever `str.of` accepts `a`". The constraint references
the function in the prelude namespace, not a class membership.

**More than one constraint.** A function may need several operations
on the same variable — e.g. a numeric routine that both compares and
adds. The representation already allows this: `TypeScheme.constraints`
is a `Vec<Constraint>`, not a single slot. The DSL just needs the
constraints comma-separated before the `=>`:

```
` { type: "<(a, a), +(a, a) => a -> a -> a" }
clamp-step: ...
```

Read: "valid wherever *both* `<` and `+` accept `a, a`". Resolution
checks each constraint independently against the declared overloads;
all must succeed. There is no inter-constraint interaction to worry
about — they share only the type variable `a`, already unified by the
rest of the signature. The grammar addition is one production: a
comma-separated constraint list. No `forall`-level entanglement.

**Cost.** Modest after H1 lands — the resolution machinery is the
analogue of typeclass instance search, but with finite enumeration over
declared overloads. No backtracking nightmares; multiple constraints
are just a conjunction checked left-to-right.

**Forwards compatibility.** Union overloads (current) are a strict
specialisation of structural constraints. Existing annotations don't
break.

**Prior art.** Haskell type classes, Rust trait bounds, Scala
implicits, Wadler-Blott "How to make ad-hoc polymorphism less ad hoc"
(1989), Go's interface bounds, modular implicits in OCaml.

**Recommendation.** Stage B, paired with H1.

---

### H11. Type-directed compilation

The checker today is purely advisory. Types are erased before STG. With
better types and a willingness to use them, several optimisations
become available. These are the most concrete *runtime* payoff of the
whole exercise.

#### H11a. Unboxing for numeric pipelines

Functions annotated `number -> number -> number` (and proven called
with numbers) can be compiled to a primitive-arithmetic STG code path
that skips boxing/dispatch. The STG already supports `Native`
primitive values; what's missing is a way to pick them on the basis of
a type annotation. For tight numeric loops the speedup can be 5–10×.

#### H11b. Lens fusion

`at(:a) ∘ at(:b)` composed and then applied currently allocates an
intermediate function (with metadata) per use. With static types and an
inliner that recognises the composition, the composed lens can be
inlined to a direct two-step access. Lens-heavy programs (transform
pipelines using `over`) would benefit.

#### H11c. IO sequencing

A `{ :io ... }` block today builds an `IoBind`-chain in the heap. With
typing that says "every step is `IO(T)` and there is no escape into
pure", the chain can be flattened into a tagged direct-style sequence.
Saves allocation on every IO action.

#### H11d. Block field offset access

When `b : {x: T, y: U, z: V}` (closed record, known offsets), `b.x`
could compile to a direct fixed-offset read. This requires a parallel
block representation for typed-known blocks; the cost is a runtime
fork in the block type. Probably not worth it given how much of
eucalypt is dynamic block construction.

#### H11e. Dead branch elimination from literal types

`if(x = :foo, A, B)` where `x : LiteralSymbol(:foo)` reduces to `A` at
compile time. Tiny but free, and synergises with H16. Already half-done
by simple constant folding. Note the "branching is not syntax"
complication from H15 applies here too: the pass must recognise the
*set* of branch combinators (`if`, `then`, `cond`, `||`), not just
`if`.

#### H11f. Direct intrinsic dispatch for typed arithmetic

`+` today dispatches at runtime (tag check). Typed `+ : number -> number -> number`
can compile straight to the `__ADDPRIM__` intrinsic with no tag check.
Same for comparison, `length`, etc.

**Recommendation.** H11a, H11c, H11e, H11f together are a coherent
package: numeric/list-heavy and IO-heavy programs would see real
gains. H11b is elegant but lens-heavy code is rare enough that it can
wait. H11d, skip.

The right place to start is a *type-aware specialisation pass* in the
inliner: when a callee has a declared type and a call site has matching
concrete arguments, emit a specialised version.

**Prior art.** GHC SpecConstr, GHC strictness analyser, MLton
whole-program specialisation, Idris 2 erasure, Roc's type-driven
compilation, OCaml flambda, Stalin's whole-program optimisation.

---

### H12. Embedding types in the language (or not)

The current `type:` metadata holds a *string* containing a separate
type DSL. The trade-off is clear.

**For strings:**

- Zero syntactic conflict with eucalypt.
- Implementation simplicity — a single recursive-descent parser.
- Strings can be templated, generated, imported as data.

**Against strings:**

- No editor support inside the string (no rename, no go-to-def).
- No scope: alias references are resolved by the checker after parsing
  the string and don't participate in eucalypt's name resolution.
- Awkward escaping in nested types.

Three options for moving on, in order of conservatism:

#### H12a. Keep the string DSL; make alias references first-class to tooling

A first draft of this called for "string interpolation" of alias
names — `"{Person} -> string"`. That framing was muddled and needs
correcting. Aliases today are *not* eucalypt bindings: `Person` is
defined inside a `types:` metadata block or via `type-def:`, and it
lives in the type checker's *alias table*, not in eucalypt's value
scope. So `{Person}` cannot be resolved by eucalypt's ordinary name
resolution / interpolation machinery — there is no value named
`Person` to interpolate.

The honest version of H12a, then, is not interpolation but
**alias references resolved within the DSL**. The current DSL grammar
*already* treats a capitalised identifier as an alias reference
(`Person` in `"Person -> string"` resolves against the alias table).
So the bare-name reference already works. What's actually missing is
the *tooling*, and that does not require moving aliases out of
metadata at all:

1. The type-string parser must record **source spans** for the
   identifiers it tokenises (offsets within the string literal).
2. The LSP must **index the `types:` blocks and `type-def:`
   declarations** — they are already in the AST, fully visible — to
   build an alias-name → definition-site map.

With (1) and (2), go-to-definition, hover, and rename work for alias
references inside type strings, even though the alias and its uses
both live in metadata. No new binding form, no change to where
aliases are defined.

The only situation that *would* require aliases to become real
eucalypt entities is if we wanted them in eucalypt's value namespace
(usable in expressions, importable by name like ordinary bindings) —
that is H12c territory, a different and larger step. H12a as
corrected stays entirely within metadata.

This is the cheapest serious step. Recommended in Stage A.

#### H12b. Optional embedded form using symbols and blocks

Allow a *symbolic* alternative to strings. Each type form maps to
existing eucalypt syntax:

```
` { type: { :fun args: [:Person] result: { :list :Person } } }
member?(p, ps): ...

` { type: { :fun args: [:a (:fun args: [:a] result: :b)] result: [:b] } }
map: ...
```

Brutally verbose. Concedes nothing in expressiveness but loses on
readability. Probably nobody would use it. Skip.

#### H12c. First-class type values

Make types eucalypt values. `:number` is a symbol that *names* a type;
`type-arrow(:a, :b)` constructs a function type; `type-for([:a, :b], ...)`
builds a scheme. Then `type:` holds a regular eucalypt expression
evaluated at compile time.

```
` { type: type-arrow(:Person, type-list(:Person)) -> :bool }
member?(p, ps): ...
```

This is interesting from a meta-programming angle but loses the
readability that the string DSL has. It would also make types
computable at compile time, which raises termination questions.

#### H12d. Embedded type expressions via idiot brackets — and the phasing problem

The first draft proposed a new `⟨ ⟩` bracket pair and called it a
syntactic addition. That overstated the cost: eucalypt's **idiot
brackets** (`⟦ ⟧`, user-definable bracket pairs) already let you
introduce a bracketed construct *without* extending the grammar — a
bracket pair is bound to a function and collects its contents. So a
type-expression bracket need not be new syntax at all; it could be an
idiot-bracket pair whose contents build a type.

But there is a real obstacle, and it is a **phasing problem**. Idiot
brackets desugar to an ordinary function applied to the collected
items, and that function runs *at evaluation time* — in the VM, after
STG compilation. The type checker runs much earlier in the pipeline
(`… → simplify → [TYPE CHECK] → inline → STG`). A type expressed
through idiot brackets would therefore be a *runtime value*, produced
by a computation the checker cannot see the result of: the type isn't
known when the checker needs it.

To make idiot-bracket types work, the checker would have to
**statically evaluate** the bracket body itself — i.e. run a small
compile-time interpreter over the type-building sub-language. That is
feasible (the type DSL is small, total, and first-order) but it is
real work and it deliberately blurs the phase boundary the
architecture otherwise keeps clean. It also drags in H12c (types as
values) as a dependency, since the bracket body would be building
type values.

So H12d is *not* a free ride via existing bracket machinery. It is
"H12c + a compile-time evaluator for the type sublanguage". That may
still be worth it one day, but it is a bigger commitment than "just
add a bracket".

**Recommendation.** H12a (corrected) in Stage A — pure ergonomic
upgrade with no risk and no change to where aliases live. Defer
H12b/c/d unless evidence appears that the string DSL is genuinely
slowing users down. The string form is honest about being a separate
language and that honesty has value.

**Prior art.** Racket contracts (DSL inside `#:contract` annotations
that uses Racket syntax), TypeScript JSDoc types in strings vs `.ts`
embedded, Idris quoted types, Lean term/syntax distinction, Haskell
TH for embedded DSLs.

---

### H13. Bidirectional gradual — boundary casts and blame

In the gradual-typing literature, an `any` value flowing into a typed
position is supposed to elaborate to a runtime *cast* that fails (with
*blame*) if the value doesn't fit. Current eucalypt is silent at the
boundary — closer to TypeScript's "trust me" than Racket's "I'll
check".

Three positions:

#### H13a. Warnings only (current)

Cheapest. No runtime cost. No blame.

#### H13b. Boundary runtime checks

Insert a check at each `any → T` boundary. Maintain blame
information (which annotation lied). Useful for debugging mixed-typed
programs. The classic concern is performance: Takikawa et al.'s
"Is Sound Gradual Typing Dead?" (2016) measured order-of-magnitude
slowdowns in Racket for naïve insertion.

Optimised variants (transient gradual typing — Vitousek; concrete
types — Greenberg) target specific positions only.

#### H13c. Optional, opt-in casts

A `--strict-boundary` flag inserts checks. Off by default. Useful for
test runs and CI; off for production rendering. Avoids the perf hit
while keeping the option open.

**Recommendation.** H13a as default forever. H13c as an experiment if
runtime errors at boundaries become a real problem. Don't pursue H13b
unconditionally.

**Prior art.** Siek & Taha "Gradual Typing for Functional Languages"
(2006), Findler & Felleisen contracts (2002), Wadler & Findler
"Well-Typed Programs Can't Be Blamed" (2009), Takikawa et al. (2016),
Vitousek "Transient" gradual (2017), Greenberg "Space-Efficient
Manifest Contracts" (2015), Reticulated Python.

---

### H14. Recursive types

JSON-like data is recursive. `Tree`, `Json`, AST-shaped values can't
be written today. The spec mentions handling this via type aliases
that self-reference (`Tree = {value: number, left: Tree | null, ..}`),
provided the resolver doesn't unfold infinitely.

**Two algebras.**

- **Equirecursive**: a type and its unfolding are equal. Subtyping
  algorithm: coinduction on type-graph nodes. Amadio-Cardelli (1993),
  Brandt-Henglein (1998). What OCaml uses (`-rectypes`).
- **Isorecursive**: explicit `roll` / `unroll`. Familiar from System
  F-μ, used in many type theory papers, but awkward as a user
  feature.

Equirecursive is the right choice. The user writes a recursive alias;
the checker treats `Tree` as equal to its unfolding everywhere.

**A pragmatic representation.** Add `Type::Mu(TypeVarId, Box<Type>)`.
The alias resolver, on detecting self-reference, builds the `Mu`
form. Subtyping uses memoised pair-walking ("assumed subtyping" set)
to avoid infinite recursion.

**JSON without dependent types.**

```
{ types: { Json: "number | string | bool | null | [Json] | Dict(Json)" } }

` { type: "Json -> string" }
to-pretty: ...
```

The object case is `Dict(Json)` — the homogeneous-block type of H19
(bead A2). That is why A3 lands after A1/A2: the `Mu` core is
independent, but the motivating `Json` type wants `Dict`.

**Cost.** Medium. The representation is small; the subtyping algorithm
needs care; the printer needs μ-detection to avoid loops.

**Prior art.** Amadio-Cardelli (1993), Brandt-Henglein (1998), Pierce
TAPL chapter 20, OCaml `-rectypes`, Scala recursive bounded types,
F-μ.

**Recommendation.** Stage A or early Stage B. Without recursive types,
the type system has a glass ceiling for structural data.

---

### H15. Flow-sensitive narrowing and type subtraction

TypeScript shows that branch-sensitive narrowing is *the* feature that
makes a structural type system feel modern. Eucalypt has predicates
(`number?`, `string?`, `nil?`, `block?`) that are perfect narrowing
witnesses.

**Sketch.** When the checker sees `if(x p?, then-branch, else-branch)`
with `p?` a recognised type predicate, narrow `x` in each branch:

```
x : number | string | null

if(x string?,
   then,     # here x : string
   else)     # here x : number | null

if(x number?,
   then,     # here x : number
   else)     # here x : string | null
```

This requires:

- A table of recognised predicates → narrowing rules.
- A "subtraction" or "exclude" form in the type representation
  (`T - U`) computed by walking unions.
- Pattern recognition in the checker for the branching forms.

It also pairs with `match?` and `cond`, where the discriminant is a
literal symbol — the case selecting `:active` narrows `x : :active`.

**Complication: branching is not syntax.** This is the catch. `if` is
an ordinary prelude function, not a syntactic form. By the time the
checker sees core expressions, `if(x number?, A, B)` is just an
application of the `if` *function* to three arguments, both branches
evaluated as plain sub-expressions with no scoping of their own. And
`if` is far from the only branching form: code routinely uses the
pipeline cousin `then` (`cond then(A, B)`), the multi-way `cond`, and
short-circuiting `||` / `&&`. There is no single syntactic node to
hang narrowing on.

The consequence: narrowing cannot be a general mechanism keyed on
syntax. It is a *special case in `synthesise_app`* that fires when the
callee is a recognised brancher. The detailed design — settled in
[literal-types-and-narrowing-spec.md](./literal-types-and-narrowing-spec.md)
— makes recognition **structural**, not a table of prelude names:

- The branch *intrinsics* `__IF`/`__AND`/`__OR`/`__COND` are recognised
  as intrinsic nodes — unforgeable.
- Wrapper functions — `then`, and a user's own `my-if` — are recognised
  by a once-per-binding, memoised structural classification of the body
  (`then(t,f,c): if(c,t,f)` is a brancher because its body is one, with
  its parameters in the slots). No name table; a user-defined brancher
  narrows for free.
- Because recognition keys off intrinsics and definition shape, there
  is no name to spoof: a rebinding that keeps the shape still narrows,
  one that changes it simply stops. The earlier "defeated by rebinding"
  worry dissolves.
- The narrowed type is threaded into the branch sub-expressions via a
  parallel narrowing stack — the de Bruijn `scope_stack` must not be
  disturbed.

This is cleaner than the "fixed table of prelude names" this section
first assumed: it bottoms out on the intrinsics, so any wrapper works
structurally. Narrowing still only reaches branchers the checker can
classify; one assembled by higher-order composition gets none. The old
`foldr`-based `cond` is itself reworked into the `__COND` intrinsic as
part of this — see the spec §A5.7.

**Cost.** Small-to-medium. The narrowing logic is small; the structural
brancher classifier and its integration into `synthesise_app` are the
fiddly part. Needs a richer union representation. Big perceived
improvement.

**Prior art.** TypeScript control-flow analysis, Flow type
refinements, Ceylon union/intersection narrowing, Crystal's compiler,
"Logical Types for Untyped Languages" (Tobin-Hochstadt &
Felleisen, 2010, Typed Racket).

**Recommendation.** Stage A, with H16.

---

### H16. Literal types extended

`LiteralSymbol` is in. Extend the cheap way:

- `LiteralString("foo")` — **taken** (A4).
- `LiteralBool(true)` — **dropped**: a union of bool literals is just
  `bool`, so it adds no expressiveness, and literal-equality narrowing
  (its only consumer) is deferred. Not worth a permanently-inert variant.
- `LiteralNumber(42)` (optional, often overkill).

Subtype rule: `LiteralX(v) <: X`.

Inference: a literal expression has its literal type. The checker
widens to the base type at boundaries that demand it (annotations,
recursive function calls). Widening on use rather than on construction
keeps reasoning local.

This combines with H15 to give exhaustive-case awareness:

```
` { type: ":active | :pending | :failed -> string" }
describe(s): cond[s = :active => "go",
                  s = :pending => "wait",
                  "stop"]
# warning: no branch for :failed; default catches it but reader may not realise
```

(`cond` here is the reworked clause form — see the literal-types spec
§A5.7.)

And with H4a to give precise lookup results.

**Cost.** Small for symbol/string/bool. Larger for number because
arithmetic of literal numbers is itself a small type-level computation.

**Recommendation.** Stage A: take symbols (have it) and strings. Skip
bools (no expressiveness gain) and numbers (type-level arithmetic).

**Prior art.** Scala 3 literal types, TypeScript literal types, Haskell
`DataKinds` + `KnownSymbol`, Crystal, Closer-to-home: F#
discriminated unions with bare-tag cases.

---

### H17. GADT-flavoured discrimination without GADTs

Genuine GADTs let you refine type-level information in branches of a
pattern match. Eucalypt has no pattern matching as such — but flow
narrowing (H15) + literal types (H16) + opaque type constructors
cover a lot of GADT territory in practice.

**The motivating use case.** A discriminated union:

```
{ types: { Result: "{ok: a} | {err: string}" } }
```

To use this safely you want, in each branch, to know whether you have
the `ok` or `err` shape. With H15+H16, branching on `has(:ok, r)`
narrows `r` to one variant.

The remaining GADT power (e.g. `Vec n a` where `n` is a type-level
nat) is a dependent-type story, parked at H4.

**Recommendation.** Don't add GADTs. The combination of H15+H16
covers the demand without adding type-system depth.

---

### H18. Per-file type profiles

Different files want different rigour. A YAML manifest wants loose
type checking; the prelude wants strict. A per-file metadata switch:

```
{ type-profile: :strict }   # warnings → errors; exported decls must annotate
{ type-profile: :advisory } # current default
{ type-profile: :off }      # skip checking
```

Optionally also: per-file *type extensions* — opt into experimental
features file by file:

```
{ type-extensions: [:hkt, :rec, :flow] }
```

This is the GHC `LANGUAGE` pragma idea, kept minimal.

**Recommendation.** Cheap, deferrable. Add when the language has a
notion of "advanced features" worth gating.

---

### H19. Homogeneous block (dictionary) types

**The gap.** There is currently no way to type "a block, arbitrary
keys, *all values of the same type*" — a `Map(symbol, T)`. The record
type `{k: T, ..}` requires *named* keys; `{..}` means "some block,
values unknown". Neither expresses "a dictionary from symbols to
`number`". This is a real hole: it is the natural type of the result
of `group-by`, of config sections with uniform shape, of
`block(kvs)`-constructed blocks with a known value type, and of
JSON objects with uniform values.

**Sketch.** Add a type form for it. Eucalypt block keys are symbols,
so the key type is fixed and only the value type varies — a one-
parameter constructor suffices:

```
Dict(T)        — block, any symbol keys, every value of type T
```

In the DSL it spells `Dict(T)` — parsed like the existing `IO(T)`.
(The record-syntax alternatives `{*: T}` / `{symbol: T}` were
considered and dropped — one spelling, no ambiguity; see the A1/A2
spec.) `Dict(number)` is the type of `{a: 1, b: 2, c: 3}`.

Subtyping and relationships:

- `Dict(T)` is covariant in `T`: `Dict(A) <: Dict(B)` if `A <: B`.
- A closed record `{a: A, b: B}` is a subtype of `Dict(A | B)` —
  every value fits the union. So `{a: 1, b: 2} <: Dict(number)`.
- `Dict(T) <: {..}` — a dictionary is still a block.
- It is *not* a subtype of any record requiring a named key, since
  no individual key is guaranteed present.

This makes a family of functions that are currently `{..} -> {..}`
type honestly:

```
` { type: "(symbol -> a -> b) -> Dict(a) -> Dict(b)" }
map-values: ...

` { type: "(a -> any) -> [a] -> Dict([a])" }
group-by: ...

` { type: "symbol -> Dict(a) -> a" }
lookup: ...                # on a Dict, the result type is just a

` { type: "Dict(a) -> [a]" }
values: ...

` { type: "Dict(a) -> [symbol]" }
keys: ...
```

Note how `map-values` — the function H3 could *not* type cleanly with
row polymorphism — falls out trivially here. The two features are
complementary: row polymorphism for *named, heterogeneous, shape-
preserving* block operations (`merge`, `over`); `Dict(T)` for
*uniform, key-agnostic* block operations (`map-values`, `group-by`,
`values`). And on a `Dict(a)`, the dependent `lookup` of H4a collapses
to the simple `symbol -> Dict(a) -> a` — no indexing needed, every
value is already `a`.

**Inference.** A block literal `{a: 1, b: 2}` synthesises as a closed
record `{a: number, b: number}` as today; the checker widens to
`Dict(number)` on demand (when checked against a `Dict` annotation, or
when joining with another block of incompatible shape). Widening on
use, like literal types (H16).

**Cost.** Small. One new `Type` variant (`Dict(Box<Type>)`), a
covariance rule, two or three subtyping rules, one DSL production,
one display case. No interaction with HKT or constraints.

**Prior art.** TypeScript index signatures (`{ [k: string]: T }`),
Flow indexer properties, PureScript's `Foreign.Object` /
`Data.Map` (records stay fixed; dictionaries are a separate type —
exactly this split), Elm `Dict`, OCaml's distinction between records
and `Hashtbl`.

**Recommendation.** Stage A. It is small, it closes a real
expressiveness gap, it makes a cluster of prelude functions typeable,
and it removes the `map-values` awkwardness from H3. Pairs naturally
with row polymorphism — adopt both together.

---

## 2. A coherent path

If we want to avoid scattering effort, the hypotheses cluster
naturally into three stages.

### Stage A — Close the existing system (6–12 months)

These complete what's already half-done. Each is small individually;
together they make the type system feel finished for everyday work.

- H3: row polymorphism in inference and merges
- H19: homogeneous block (`Dict(T)`) types — adopt alongside H3
- H14: recursive types via equirecursion + alias self-reference
- H15: flow-sensitive narrowing via predicates (with the
  branch-combinator recognition table)
- H16: literal types for strings and booleans (symbols already)
- H7a + H7c: `NonEmpty` and branch-narrowing
- H12a: first-class alias references in DSL strings (parser spans +
  LSP indexing of `types:` blocks)
- H9 (cheap version): in-memory per-module summary cache

End state: a complete structural gradual checker that catches a much
wider set of real bugs and integrates with the IDE smoothly. No new
type-theoretic ambition.

### Stage B — Expressiveness (12–24 months)

The big-step features. Each depends on the unifier/checker being
solid. Each is genuinely new theory in the codebase.

- H1: higher-kinded type variables
- H10: structural operator constraints (incl. multiple constraints
  per scheme)
- H6b: `Partial(T)` opaque type
- H4a: indexed access on records (`r[k]` for literal `k`) — builds on
  H16 from Stage A
- H9 (full version): persistent summaries and LSP cross-file inference
- *Metadata typing* — a prerequisite if H2b is wanted; justify it on
  its own merits (fixity, `target`, `type:` itself), not just lenses

End state: the prelude can be honestly typed end to end, including
`monad()`, `min`/`max`, and `head`. User-defined monads inherit
correct types. Lens *internals* (H2b) remain gated on metadata
typing — the lens *interface* is already typed and that suffices for
users.

### Stage C — Radical options (24+ months)

These are mutually substitutable; pick at most one or two.

- H11a + H11c + H11e + H11f: type-directed compilation for measurable
  runtime gains
- H8: replace the inference core with an MLsub/MLstruct-style algorithm
- H6c: capability/determinism tracking for rendering
- H12d: lightweight embedded type bracket pair (only if string DSL is
  visibly costing)
- H18: per-file profiles

The choice between H8 and "keep extending the current core" is the
biggest fork. Reassess after Stage B.

---

## 3. Cross-cutting themes

A few principles emerged that should guide whichever subset of
hypotheses is taken.

**Structural over nominal — without exception.** Every extension above
respects this. Constraints reference functions, not classes;
interfaces are block shapes; "type classes" are records the user
constructs explicitly. This is what makes eucalypt's type system
distinct from a Haskell descendent.

**Opaque > transparent for complex abstractions.** `IO`, `Lens`,
`Traversal`, future `Partial` — all opaque. The user sees the
*interface* type, not the encoding. The implementation is free to
change. Where we want to type internals (H2b), the typing lives in the
library and doesn't leak to users.

**Inference-first, annotation as documentation.** Every feature must
deliver value when only the prelude is annotated. HKT-typed `monad()`
gives the user typed monads without forcing the user to write a single
type. Row polymorphism flows merges without annotation. Literal types
emerge from literals.

**Gradual boundary stays kind.** Each new constructor type needs a
sensible "what happens against `any`" rule. Opaque constructors get
the default — consistent with `any` in both directions. Refinement
types (H7) require explicit "widen on contact" semantics.

**Syntactic conservatism is a feature.** It forces every extension to
either reuse existing syntax (blocks, symbols, strings) or live in
metadata. This keeps the language small. The only marginal cost is
DSL friction for type annotations (H12a fixes most of it).

---

## 4. Use of types in the implementation

Beyond catching user errors, types could shape the rest of the
compiler. Worth pulling these out because they are the *non-obvious*
payoff.

| Use | Hypothesis | Payoff |
|---|---|---|
| Numeric specialisation | H11a, H11f | 5–10× on tight arithmetic |
| IO sequencing | H11c | Allocation reduction on IO-heavy code |
| Dead branch elimination | H11e | Free with H16 |
| Lens fusion | H11b | Smaller heaps for lens pipelines |
| LSP completions on records | H3, H16 | Better autocomplete |
| LSP cross-file inference | H9 | Faster incremental check |
| Render reproducibility lint | H6c | Catch non-determinism statically |
| Test-mode boundary checks | H13c | Find latent mistyping in mixed code |

The first three are runtime wins; the rest are tooling wins. Together
they justify keeping the type system more than advisory in the long
term.

---

## 5. Open questions for the reviewer

These should be decided before committing to a stage plan.

1. **Is "no nominal" inviolable** or could a small opt-in nominal
   newtype mechanism be tolerated for export contracts? (Affects H10,
   H17.)

2. **Is the metadata DSL acceptable indefinitely**, or is the friction
   visible enough to want an embedded form within a year? (Affects
   H12.)

3. **Should we encourage user-defined monads** (pulls in HKT proper)
   or accept that monads are a prelude-only thing the user assembles
   from `monad()` without further typing? (Affects H1, H2b.)

4. **How much do we care about runtime performance** of the
   interpreter? (Determines whether H11 is in or out.)

5. **Do we want to invest in algebraic subtyping** (H8) as a long-term
   core replacement, or stay with HM-with-subtyping forever? (Big
   fork at Stage C.)

6. **What is the policy on boundary unsoundness?** Forever silent
   (H13a), opt-in checks (H13c), or eventually mandatory checks
   (H13b)?

7. **Is typing the metadata channel worth doing for its own sake?**
   It is the hard prerequisite for H2b (lens internals) but also
   touches fixity/precedence metadata, `target`, and the `type:`
   field itself. If the answer is no, H2b stays parked indefinitely
   and that is fine — the lens *interface* is already typed. (Affects
   H2.)

---

## 6. Reading list

### Foundational gradual typing
- Siek & Taha, *Gradual Typing for Functional Languages* (2006)
- Wadler & Findler, *Well-Typed Programs Can't Be Blamed* (2009)
- Takikawa et al., *Is Sound Gradual Typing Dead?* (2016)
- Greenberg, *Space-Efficient Manifest Contracts* (2015)
- Vitousek et al., *Big Types in Little Runtime* / *Transient* (2017)
- Tobin-Hochstadt & Felleisen, *Logical Types for Untyped Languages*
  (2010, Typed Racket)

### Row polymorphism
- Wand, *Complete Type Inference for Simple Objects* (1987)
- Rémy, *Type Inference for Records in a Natural Extension of ML*
  (1994)
- Pottier, *A Versatile Constraint-Based Type Inference System* (2000)
- Leijen, *Extensible Records with Scoped Labels* (2005)
- PureScript record-row documentation
- Koka effect rows

### Algebraic subtyping
- Dolan & Mycroft, *Polymorphism, Subtyping, and Type Inference in
  MLsub* (2017); Dolan's PhD thesis
- Parreaux, *The Simple Essence of Algebraic Subtyping* (ICFP 2020)
- Parreaux et al., MLstruct / MLscript / Hegel papers

### HKT, classes, dictionaries
- Wadler & Blott, *How to Make Ad-hoc Polymorphism Less Ad Hoc* (1989)
- Brady, *Idris 2 Type-Driven Development*
- Scala 3 *Given/Using* reference
- Modular Implicits (OCaml) papers

### Optics
- van Laarhoven, *CPS-based lenses* blog series (2007)
- Pickering, Gibbons, Wu, *Profunctor Optics: Modular Data Accessors*
  (2017)
- Boisseau & Gibbons, *Don't Fear the Profunctor Optics*
- Kmett's `lens` documentation

### Refinement types
- Vazou et al., *Refinement Types for Haskell* / Liquid Haskell papers
- Swamy et al., F* papers
- Vekris et al., *Refined TypeScript*
- Orchard et al., *Granule*

### Recursive types
- Amadio & Cardelli, *Subtyping Recursive Types* (1993)
- Brandt & Henglein, *Coinductive Axiomatization of Recursive Type
  Equality and Subtyping* (1998)
- Pierce, *TAPL* chapters 20–21

### Effects
- Leijen, Koka effect papers
- Pretnar, *Eff*
- Lindley et al., *Frank*
- Unison abilities documentation
- OCaml 5 effects design notes

### Dependent (lite)
- TypeScript handbook on indexed access and conditional types
- Roy/Scala literal type docs
- Brady, *Idris 2 dependent records*

### Type-directed compilation
- Peyton Jones et al., GHC SpecConstr / strictness analyser papers
- MLton documentation on whole-program defunctorisation
- Idris 2 erasure paper
- Roc compilation strategy notes
