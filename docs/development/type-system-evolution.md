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
language — annotations live in metadata strings.

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

Four routes, increasing in ambition:

#### H2a. Stay opaque (current plan)

`Lens(a, b)` and `Traversal(a, b)` as opaque constructors with hand-
written `at`, `ix`, `item`, `each` signatures. We have this; it works
for users.

#### H2b. van Laarhoven internals with HKT + rank-2 + structural constraints

The honest type:

```
Lens(s, t, a, b) = forall (f :: * -> *). Functor f => (a -> f b) -> (s -> f t)
```

Simplified monomorphic-focus version (eucalypt usage):

```
Lens(a, b) = forall f. Functor f => (b -> f b) -> (a -> f a)
```

This needs three things we don't have:

1. **Higher-kinded variables** (H1).
2. **Rank-2 quantification** — `forall f` inside the alias body, used
   wherever the lens is *applied*.
3. **A way to spell `Functor f`** without introducing typeclasses.

For (3) the structural-constraint route fits eucalypt:

```
Functor(f) = exists fmap on f. (a -> b) -> f a -> f b
```

Phrased operationally: "`f` is a Functor when the block in scope
provides `fmap` with that type". Since our lens code *literally
passes a block carrying `fmap` as metadata*, this constraint is exactly
a row constraint on the metadata namespace. The type system can read
it as:

```
{ fmap: (a -> b) -> f a -> f b, .. }
```

— i.e. a record whose `fmap` field has the Functor signature. This
treats Functor not as a class but as a *namespace shape*. Lens checking
then becomes: when applied, the inner function must arrive carrying a
metadata block of this shape.

This is structurally what eucalypt's lens encoding already does at
runtime. We're just giving the same idea a static reading.

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

**Recommendation.** H2a for users; H2b as an *internal* kernel so the
lens library body can be type-checked. Users only see `Lens(a, b)` and
`Traversal(a, b)`. The internal type would only appear in
`lib/lens.eu` and would not propagate.

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

` { type: "(symbol -> a -> b) -> {..r} -> {..r}[a/b]" }
map-values: ...
```

The last is harder: `[a/b]` means "substitute `a` for `b` throughout
the row". This is what the *kind*-level `Row` mini-algebra of PureScript
provides.

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

---

### H4. Restricted dependent types where they pay rent

Full dependent types are out — too heavy, too unfamiliar, no demand.
But four restricted forms each give significant power for small cost.

#### H4a. Indexed access on records (Π over `Symbol`)

`b.foo` where `b : {foo: T, ..}` returns `T`. Already done. The
*dependent* version: `lookup(k, b)` where `k : LiteralSymbol("foo")`
and `b : {foo: T, ..}` returns `T`. The result type depends on the
literal value of `k`.

```
` { type: "(k :: symbol) -> {..r} -> r[k] | null" }
lookup: __LOOKUP
```

This is exactly what TypeScript's `T[K]` *indexed access type* gives.
For non-literal `k`, the result falls back to `any` (the union over the
row, in principle). For literal `k`, we get precision.

#### H4b. Indexed access on tuples and short lists

`tuple !! n` where `n` is a literal. Same idea, integer index.

#### H4c. Computing monad result types from metadata

The `monad: "[a]"` annotation on `for` is already a tiny dependent
type — the *value* of the metadata determines the *type* of the
desugared bindings. The desugarer reads the string and emits a type
hint. We don't call this dependent, but it is.

A full version: a monadic block `{ :name k: v ... }` would have its
type computed by substituting the binding values into the monad's
declared wrapper type. The infrastructure is already half there.

#### H4d. Lens path types

`‹:items 0 :meta›` has a type that depends on the literal symbols and
indices in it. Today the desugarer composes lens types and emits a
hint. A real dependent encoding —
`path : ∀ ks. (ks :: [Symbol|Number]) -> Lens(walk(ks, s), t)` —
would be expressive but probably not worth the complexity. The
desugar-time hint is enough.

**Recommendation.** Take H4a, defer the rest. H4a is the most useful
single dependent-type feature for a data-transformation language. The
rest are nice-to-have.

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
  constructors. Cheap to implement, useful, no SMT.
- **Heavyweight**: arithmetic predicates with an SMT backend. Out of
  scope for a config-and-data tool.

#### H7a. `NonEmpty([a])`

```
` { type: "NonEmpty([a]) -> a" }
head: __HEAD
```

Construction: lists with at least one literal element, `cons(_, xs)`,
or after a `nil?` narrowing branch. Wide impact, small effort. The
type is just a thin tag.

#### H7b. Constant-folded refinements

When the inliner can prove a literal value satisfies a refinement
(e.g. `42 != 0` for `Nonzero`), the warning is suppressed. This is
free if the inliner already knows about the value.

#### H7c. Branch-sensitive narrowing

After `if(xs nil?, ..., body)` the checker narrows `xs : [a]` to
`xs : NonEmpty([a])` in `body`. Pairs naturally with H15.

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

**Recommendation.** Stage B or C. Cheap to start small (in-memory
cache); persistence and the LSP wiring are the real work.

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

**Cost.** Modest after H1 lands — the resolution machinery is the
analogue of typeclass instance search, but with finite enumeration over
declared overloads. No backtracking nightmares.

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
by simple constant folding.

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

#### H12a. Keep the string DSL; interpolate alias names

Reuse eucalypt's string interpolation inside the type string:

```
` { type: "{Person} -> [{Person}] -> bool" }
member?(p, ps): ...
```

The `{Person}` is resolved as an alias reference at parse time. LSP
can offer rename, go-to-def, hover. Implementation is trivial — the
string parser already tokenises identifiers.

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

#### H12d. Light embedded type-only mini-syntax (radical)

Add a single new bracket pair for types, e.g.:

```
` { type: ⟨Person -> [Person] -> bool⟩ }
member?(p, ps): ...
```

Inside `⟨ ⟩` the parser switches to type-DSL mode. Syntactically
self-contained, no conflict. But it *is* a syntactic addition and the
brief is to avoid those. Skip unless H12a proves insufficient.

**Recommendation.** H12a in Stage A — pure ergonomic upgrade with no
risk. Defer H12b/c/d unless evidence appears that the string DSL is
genuinely slowing users down. The string form is honest about being a
separate language and that honesty has value.

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
{ types: { Json: "number | string | bool | null | [Json] | {symbol: Json}" } }

` { type: "Json -> string" }
to-pretty: ...
```

Note `{symbol: Json}` requires row polymorphism + a way to say "a row
where all values have type Json". This is exactly the
`map-values`-shaped row constraint of H3, and the natural unifying
point.

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

if(x null?,
   then,     # here x : null
   else)     # here x : number | string

if(x number?,
   then,     # here x : number
   else)     # here x : string | null
```

This requires:

- A table of recognised predicates → narrowing rules.
- A "subtraction" or "exclude" form in the type representation
  (`T - U`) computed by walking unions.
- Pattern recognition in the checker for the `if` form.

It also pairs with `match?` and `cond`, where the discriminant is a
literal symbol — the case selecting `:active` narrows `x : :active`.

**Cost.** Small after literal types (H16) and a slightly richer union
representation. Big perceived improvement.

**Prior art.** TypeScript control-flow analysis, Flow type
refinements, Ceylon union/intersection narrowing, Crystal's compiler,
"Logical Types for Untyped Languages" (Tobin-Hochstadt &
Felleisen, 2010, Typed Racket).

**Recommendation.** Stage A, with H16.

---

### H16. Literal types extended

`LiteralSymbol` is in. Extend the cheap way:

- `LiteralString("foo")`
- `LiteralBool(true)`
- `LiteralNumber(42)` (optional, often overkill)

Subtype rule: `LiteralX(v) <: X`.

Inference: a literal expression has its literal type. The checker
widens to the base type at boundaries that demand it (annotations,
recursive function calls). Widening on use rather than on construction
keeps reasoning local.

This combines with H15 to give exhaustive-case awareness:

```
` { type: ":active | :pending | :failed -> string" }
describe(s): cond([[s = :active, "go"],
                   [s = :pending, "wait"]],
                  "stop")
# warning: no branch for :failed; default catches it but reader may not realise
```

And with H4a to give precise lookup results.

**Cost.** Small for symbol/string/bool. Larger for number because
arithmetic of literal numbers is itself a small type-level computation.

**Recommendation.** Stage A: take symbols (have it), strings, bools.
Skip numbers.

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

## 2. A coherent path

If we want to avoid scattering effort, the hypotheses cluster
naturally into three stages.

### Stage A — Close the existing system (6–12 months)

These complete what's already half-done. Each is small individually;
together they make the type system feel finished for everyday work.

- H3: row polymorphism in inference and merges
- H14: recursive types via equirecursion + alias self-reference
- H15: flow-sensitive narrowing via predicates
- H16: literal types for strings and booleans (symbols already)
- H7a + H7c: `NonEmpty` and branch-narrowing
- H12a: alias interpolation in DSL strings
- H9 (cheap version): in-memory per-module summary cache

End state: a complete structural gradual checker that catches a much
wider set of real bugs and integrates with the IDE smoothly. No new
type-theoretic ambition.

### Stage B — Expressiveness (12–24 months)

The big-step features. Each depends on the unifier/checker being
solid. Each is genuinely new theory in the codebase.

- H1: higher-kinded type variables
- H10: structural operator constraints
- H2b: typed lens internals as an internal kernel
- H6b: `Partial(T)` opaque type
- H4a: indexed access on records (`r[k]` for literal `k`)
- H9 (full version): persistent summaries and LSP cross-file inference

End state: the prelude can be honestly typed end to end, including
`monad()`, `min`/`max`, `head`, and lens internals. User-defined
monads inherit correct types.

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
