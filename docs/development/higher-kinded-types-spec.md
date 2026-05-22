# Spec: Higher-Kinded Type Variables (Bead B1)

**Status**: Specification — ready to implement.
**Date**: 2026-05-19
**Branch**: `type-system-exploration`
**Companions**: [type-system-evolution.md](./type-system-evolution.md) (H1),
[type-system-bead-plan.md](./type-system-bead-plan.md) (TS-B1),
[row-polymorphism-and-dict-spec.md](./row-polymorphism-and-dict-spec.md),
[recursive-types-spec.md](./recursive-types-spec.md).

B1 is the keystone of Phase B. It adds a real kind system so type
variables can range over type *constructors* (`* -> *`), so that
`monad()` and the monad namespaces can be typed honestly (B8). B1 is the
*machinery*; B8 *applies* it.

## Decisions taken

1. **Uniform applied representation.** The dedicated `List`/`IO`/`Dict`/
   `NonEmpty`/`Lens`/`Traversal` `Type` variants are removed and
   replaced by a named constructor `Type::Con` and constructor
   application `Type::App`. `List(a)` *is* `App(Con("List"), a)`. This
   is a real representation rewrite — every `match` on `Type` is touched
   — but it is mechanical, and it *removes* special cases rather than
   adding them: a kind-`*→*` variable can then bind to `Con("List")`,
   `Con("IO")`, … uniformly, which is the whole point of HKT.
2. **Explicit kinds, with a convenience convention.** The kind system is
   real (`Kind` representation, kind-aware unification) — not the
   "head-position variables are silently `*→*`" hack. Explicit kinds
   (`forall (m :: * -> *)`) are the foundation; the head-position
   convention is a *convenience* layered on top for the common case.
3. **B2 is decoupled from B1.** Structural operator constraints (B2)
   are value-level and share no essential machinery with kinds; B2 may
   proceed in parallel.
4. **B4 is won't-do.** Typing the lens-library internals is not pursued;
   `Lens`/`Traversal` stay opaque for users indefinitely.

## Background — what exists today

- `Type` (types.rs) is a flat enum with dedicated variants for each
  constructor: `List(Box<Type>)`, `IO(Box<Type>)`, `Function(Box<Type>,
  Box<Type>)`, `Lens(Box<Type>, Box<Type>)`, `Traversal(…)`,
  `Tuple(Vec<Type>)`, `Record{…}`, `Union(Vec<Type>)`, `Var(TypeVarId)`,
  `LiteralSymbol`, primitives. Phase A adds `Dict(Box<Type>)` (A2),
  `NonEmpty(Box<Type>)` (A6), `Mu(TypeVarId, Box<Type>)` (A3),
  `LiteralString` (A4).
- There is **no kind system**. Every type is implicitly kind `*`. A
  type variable cannot stand for a constructor.
- `TypeScheme` carries outer quantification (`quantified`,
  `constraints`, `body`). Quantification is *prenex* (rank-1) — there is
  no nested `forall`.
- The DSL (`parse.rs`) has `[T]`, `IO(T)`, `(A,B)`, `{…}`, `A -> B`,
  type variables, alias references. No application syntax, no `forall`
  keyword, no kinds.

## B1.1 Representation — `Con` and `App`

Replace the data-constructor variants with two new ones:

```rust
/// A named type constructor: List, IO, Dict, NonEmpty, Lens, Traversal,
/// and any user/abstract constructor. Carries no arguments itself.
Con(String),
/// Constructor application. `App(App(Con("Lens"), a), b)` is Lens(a,b).
App(Box<Type>, Box<Type>),
```

Removed variants: `List`, `IO`, `Dict`, `NonEmpty`, `Lens`, `Traversal`.
Their meaning becomes:

| Was | Now |
|-----|-----|
| `List(a)` | `App(Con("List"), a)` |
| `IO(a)` | `App(Con("IO"), a)` |
| `Dict(a)` | `App(Con("Dict"), a)` |
| `NonEmpty(a)` | `App(Con("NonEmpty"), a)` |
| `Lens(a, b)` | `App(App(Con("Lens"), a), b)` |
| `Traversal(a, b)` | `App(App(Con("Traversal"), a), b)` |

**Kept as dedicated variants** (deliberately *not* `Con`-ified):

- `Function(Box<Type>, Box<Type>)` — the arrow is pervasive and central
  to the bidirectional checker; currying it through `App(App(Con("->"),
  _), _)` would touch every function-type match for no gain (no HKT
  variable ever needs to range over `(->) a`). Standard practice.
- `Tuple(Vec<Type>)` — variadic; not a fixed-arity constructor.
- `Record{…}`, `Union(Vec<Type>)`, `Mu(…)`, `Var`, `LiteralSymbol`,
  `LiteralString`, the primitives — structural / nullary; unchanged.

So B1 re-does, in uniform form, the representation of constructors that
Phase A introduced as variants (`Dict`, `NonEmpty`). Phase A ships first
with variants; B1 converts them. The Phase A specs need no change — they
are written against the variant form and remain correct for 6.1; B1's
file-by-file (below) lists the conversion.

A **smart-constructor layer** (`Type::list(t)`, `Type::io(t)`,
`Type::lens(a,b)`, …) builds the applied form, so call sites read
normally and a future representation change is localised.

## B1.2 Kinds

```rust
enum Kind {
    Star,                         // *  — the kind of ordinary types
    Arrow(Box<Kind>, Box<Kind>),  // k1 -> k2 — a constructor
}
```

- `Con("List")`, `Con("IO")`, `Con("Dict")`, `Con("NonEmpty")` ::
  `* -> *`.
- `Con("Lens")`, `Con("Traversal")` :: `* -> * -> *`.
- A fixed table maps each built-in `Con` to its kind. User/abstract
  constructors get their kind from the annotation that introduces them.
- Ordinary types (`number`, `[a]`, a fully-applied `App`) :: `*`.

**Variables carry a kind.** `Type::Var` becomes `Var(TypeVarId, Kind)`
(or a parallel kind environment keyed on `TypeVarId` — implementer's
choice; the field is simpler). A fresh variable allocated for a `*→*`
position is allocated *at* kind `*→*`. The unifier (B1.5) only unifies
same-kind things.

## B1.3 Higher-rank quantification — `Type::Forall`

`monad()`'s signature quantifies `a, b` *independently inside each
combinator* (each combinator is separately polymorphic) while sharing
`m` (the monad). That is rank-2: `forall` nested inside the result
record's field types. Add an explicit quantifier node:

```rust
/// Explicit (possibly nested / higher-rank) quantification.
Forall(Vec<(TypeVarId, Kind)>, Box<Type>),
```

`monad()`'s result record then has `Forall`-typed fields:
`{bind: Forall([a,b], …), map: Forall([a,b], …), …}` with `m` bound by
an outer `Forall`.

**Checking-only higher-rank.** Rank-N types are **checked, never
inferred** — the annotation always carries the quantifiers. The checker:

- *Instantiates* a `Forall` on use — freshens its quantified variables
  (the existing `freshen` mechanism, extended to walk `Forall`).
- *Skolemises* a `Forall` when checking a value against it — replaces
  the quantified variables with fresh rigid constants and checks the
  body. A rigid constant unifies only with itself.

This is the standard "predicative, annotation-driven higher-rank"
discipline (Peyton Jones et al., *Practical type inference for
arbitrary-rank types*). No inference horror because nothing rank-N is
inferred.

`TypeScheme` (the existing outer scheme with `constraints`) **coexists**
with `Type::Forall`: a `TypeScheme.body` may now contain `Forall` nodes.
A later cleanup could fold `TypeScheme` into a top-level `Forall` + a
constraint list; that is out of scope for B1 — noted, not done.

## B1.4 Kind checking

When a type annotation is parsed and resolved, the checker **kind-checks**
it before use:

- `App(f, x)` is well-kinded iff `f :: k₁ -> k₂` and `x :: k₁`; the
  application has kind `k₂`.
- A type in a position requiring kind `*` (a function argument/result, a
  record field, a list element) must *be* kind `*` — a bare `Con("IO")`
  or a partially-applied `App(Con("Lens"), a)` there is a **kind error**
  → a type warning (`"IO is a constructor; it needs an argument"`).
- `Forall`-quantified variables get their kind from the binder
  (`forall (m :: * -> *)`) or, absent an explicit kind, from the
  convention (B1.7).

Kind checking is a small pass over the resolved annotation type; it runs
where annotations are parsed (`register_aliases_from_meta`, annotation
extraction).

## B1.5 Unification — kind-aware

- `unify(Con(a), Con(b))` → succeeds iff `a == b`.
- `unify(App(f₁, x₁), App(f₂, x₂))` → `unify(f₁, f₂)` then
  `unify(x₁, x₂)`. (Decomposition; the head positions unify, so a
  `*→*` variable head unifies with `Con("List")` etc.)
- Binding a variable: a `Var` of kind `k` binds only to a type of kind
  `k`. Kind mismatch → unification failure. The occurs check is
  unchanged but must descend through `App`.
- `unify(Forall(…), …)` — instantiate the `Forall` (freshen) and unify
  the body; two `Forall`s instantiate both. (Higher-rank unification is
  only reached for annotation-driven checking, never general inference.)

The existing row-variable and `Mu` unification arms are unaffected —
they operate on `Record`/`Mu`, which remain variants.

## B1.6 Subtyping — variance over `App`

`is_subtype` over `App` needs a variance story:

- For a **known** built-in `Con`, variance is declared: `List`, `IO`,
  `Dict`, `NonEmpty` are *covariant* in their argument; `Lens`/
  `Traversal` follow their existing rules (carried over verbatim from
  the removed variants). So `App(Con("List"), a) <: App(Con("List"), b)`
  iff `a <: b` — list covariance, preserved.
- For an **abstract** head — `App(Var(m), a)` where `m` is a `*→*`
  variable — variance is unknown, so require **invariance**:
  `App(Var(m), a) <: App(Var(m), b)` iff `a` and `b` are equivalent.
  Sound and simple.
- `is_consistent` mirrors this, relaxing to `true` where `any` is
  involved as today.

Declared variance for *user* constructors is a possible later extension;
B1 ships built-in variance + invariant-abstract.

## B1.7 DSL — application, `forall`, kinds

The type DSL (`parse.rs`) gains, all *inside `type:` strings* — no
eucalypt-surface syntax changes:

- **Application by juxtaposition**: `m a`, `m a b` →
  `App(App(m, a), b)`. Inside a type string there is no catenation
  operator, so juxtaposition is unambiguous.
- **`forall`**: `forall a b. T`, with optional kinds
  `forall (m :: * -> *) a b. T` → a `Type::Forall`. `forall` may appear
  nested (rank-N). The currently-implicit prenex quantification stays
  the default when no `forall` is written.
- **Kind syntax**: `*`, `* -> *`, `(* -> *) -> *`.
- The existing sugar is retained and still produces the applied form:
  `[T]` → `App(Con("List"), T)`, `IO(T)` → `App(Con("IO"), T)`,
  `Dict(T)`, `NonEmpty([T])` likewise.
- **Convention** (convenience): a variable that appears in head position
  of an application and is not given an explicit kind is inferred to
  have kind `* -> … -> *` with arity from its uses. Genuinely ambiguous
  cases (e.g. `a a`) require an explicit kind or are a kind error.

## B1.8 Display

The uniform representation must **not** uglify output. `Display` and
`humanise` re-sugar the applied form:

- `App(Con("List"), x)` → `[x]`
- `App(Con("IO"), x)` → `IO(x)`; `App(Con("Dict"), x)` → `Dict(x)`;
  `App(Con("NonEmpty"), x)` → `NonEmpty([x])`
- `App(App(Con("Lens"), a), b)` → `Lens(a, b)`; `Traversal` likewise
- `App(Con(c), x)` for any other `c` → `c x` (constructor juxtaposition)
- `Con(c)` alone → `c`
- `Forall(vars, body)` → `forall …. body`, kinds shown only when not `*`

So a list still prints `[number]`, and a user monad prints `m a`.

## B1.9 Scope

B1 delivers the *machinery* — kinds, `Con`/`App`/`Forall`, kind-aware
unification, kind checking, the DSL, display. It does **not** annotate
`monad()` or the monad namespaces; that is **B8**, which is pure
application of B1. B1's acceptance is that a HKT signature *can be
written, kind-checks, and freshens correctly* — exercised by a test
annotation, not by changing the prelude.

## Sequencing

B1 lands in Phase B, **after** all of Phase A. It rewrites the
representation of constructors that A2 (`Dict`) and A6 (`NonEmpty`)
introduce as variants — so it must follow them. It is independent of
B2; it blocks B8 (and, were B4 not won't-do, B4). The representation
rewrite should be one self-contained commit (mechanical variant →
`Con`/`App` conversion, behaviour-preserving), with the kind system,
`Forall`, and DSL changes layered on top.

## Test plan

Harness tests in `tests/harness/typecheck/`; Rust unit tests per module.

- **Representation rewrite** — regression: the full existing
  `cargo test` suite and `eu check lib/prelude.eu` are *unchanged* after
  the variant → `Con`/`App` conversion (it is behaviour-preserving);
  display still prints `[number]`, `IO(a)`, `Lens(a,b)`.
- **Kinds** — unit: `Con` kinds; `App` well-kindedness; a partially
  applied constructor in a `*` position is a kind error; kind-mismatched
  unification fails.
- **Higher-rank** — a rank-2 annotation (a record field that is
  `forall a. …`) kind-checks; instantiation freshens per use;
  skolemisation rejects a non-polymorphic value supplied where a
  `Forall` is expected.
- **DSL** — `m a`, `forall (m :: * -> *) a. m a -> m a`, kind syntax all
  round-trip; the `[T]`/`IO(T)` sugar still parses to applied form.
- **End-to-end** — a hand-written HKT annotation in a `type:` string
  (a `monad()`-shaped signature) kind-checks and a correct/incorrect use
  is accepted/warned. (The real `monad()` annotation is B8.)
- Full `cargo test` green; clippy clean.

## Open questions

1. **`Var` kind storage** — kind as a field on `Type::Var(TypeVarId,
   Kind)`, or a side environment keyed on `TypeVarId`? The field is
   simpler and self-contained; a side environment avoids touching every
   `Var` construction site. Recommend the field.
2. **`TypeScheme` / `Forall` unification** — B1 lets them coexist.
   Folding `TypeScheme` into a top-level `Forall` + constraints is a
   tidy follow-up but a wide refactor; left out of B1. Worth a Phase B
   cleanup bead later?
3. **`->` as a constructor** — kept as `Type::Function`. If a future
   need arises to abstract over the function constructor (a reader
   monad `(->) r`), revisit; not expected.

## File-by-file change summary

| File | Change |
|------|--------|
| `types.rs` | remove `List`/`IO`/`Dict`/`NonEmpty`/`Lens`/`Traversal` variants; add `Con`, `App`, `Forall`, `Kind`; `Var` carries a `Kind`; smart constructors; `Display`/`humanise` re-sugar applied form |
| `subtype.rs` | `App` variance (built-in covariance table; invariant abstract head); `Con`/`App`/`Forall` arms; carry over `Lens`/`Traversal`/`List`/`IO`/`Dict`/`NonEmpty` rules onto the applied form |
| `unify.rs` | `Con`/`App` decomposition; kind-aware variable binding; `Forall` instantiate-and-unify; occurs check through `App` |
| `check.rs` | kind-checking pass on resolved annotations; `freshen` walks `Forall`; instantiation/skolemisation of `Forall`; synthesis builds applied form via smart constructors |
| `parse.rs` | application by juxtaposition; `forall` + kind syntax; head-position kind convention; existing `[T]`/`IO(T)`/… sugar emits applied form |
| `tests/harness/typecheck/` | kind, higher-rank, HKT-annotation tests; representation-rewrite regression coverage |
