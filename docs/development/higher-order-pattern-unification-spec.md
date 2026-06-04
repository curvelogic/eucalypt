# Higher-Order Pattern Unification for HKT Type Constructors

## Status

Design spec — replaces the monad-metadata approach from PR #766.

## Problem

The type checker supports higher-kinded type variables (`m :: * → *`)
via `Type::Con`, `Type::App`, and `Type::Forall`.  HKT unification
currently only works when both sides of a unification are in `App`
form — e.g. `App(Var(m), Var(a))` against `App(Con("List"), Number)`
decomposes structurally and binds `m = Con("List")`.

This fails for type constructors that aren't named `Con` entries.  For
example, `monad()` has type:

```
forall (m :: * → *). {bind: m a → (a → m b) → m b, return: a → m a} → {…}
```

When called with the random monad's `bind`:

```
(stream → {value: a, rest: stream}) → (a → stream → {value: b, rest: stream}) → stream → {value: b, rest: stream}
```

The unifier needs to match `m a` against
`stream → {value: a, rest: stream}` and infer
`m = λa. stream → {value: a, rest: stream}`.  This is a type-level
function — the current first-order unifier cannot represent or
construct it.

PR #766 worked around this by deriving monad combinator types from
`monad:` metadata strings — a brittle, domain-specific mechanism that
bypasses HKT entirely.  This spec replaces that approach with
general-purpose higher-order pattern unification.

## Design

### 1. Type::Lam variant

Add a type-level lambda to the `Type` enum:

```rust
/// Type-level lambda: `Lam(x, body)` represents `λx. body`.
///
/// Constructed by higher-order pattern unification when a `* → *`
/// variable is unified against a concrete type.  Reduced eagerly
/// via beta reduction when applied: `App(Lam(x, body), arg)` →
/// `structural_subst(body, x, arg)`.
Lam(TypeVarId, Box<Type>),
```

**Kind**: `kind_of(Lam(x, body))` = `Kind::Arrow(kind_of(x), kind_of(body))`.

**Display**: `λa. stream → {value: a, rest: stream}` (or omit in
user-facing messages — users see the applied form, not the lambda).

### 2. Higher-order pattern unification rule

In `unify()`, when one side is `App(Var(m, k), x)` and `m` is
unsolved, and the other side is a concrete type `T` that is NOT an
`App`:

1. Unify `x` against its occurrence in `T` (if `x` is already bound
   in the substitution, apply that first).
2. Let `x_resolved` be the resolved form of `x`.
3. If `x_resolved` is a variable: construct `Lam(x_resolved, T)` and
   bind `m = Lam(x_resolved, T)`.
4. If `x_resolved` is concrete: this is first-order application —
   the unifier should attempt to match `T` against the applied form
   (deferred to consistency checking at the gradual boundary).

**Kind check**: `Lam(x, body)` has kind `* → kind_of(body)`.  This
must match `m`'s declared kind.

**Occurs check**: standard — `m` must not appear free in `T`.

**The pattern restriction**: this rule applies when `m` is applied to
**distinct variables** — not arbitrary terms.  `App(Var(m), Var(a))`
qualifies; `App(Var(m), Number)` does not (that's first-order).  This
is the "Miller pattern fragment" which guarantees decidability and
most-general unifiers.

### 3. Beta reduction

Wherever `App(Lam(x, body), arg)` appears, reduce eagerly to
`structural_subst(body, x, arg)`.  This should happen in:

- `apply_subst` — after substituting, check if the result is a
  beta-redex and reduce
- `is_subtype_co` / `is_consistent` — reduce before comparing
- `humanise` / display — show the reduced form, not `(λa. ...) number`

The existing `structural_subst` function (from PR #766) performs
exactly the right one-pass substitution without re-applying to the
replacement.  Retain it; remove the monad-specific code that calls it.

### 4. Free variable identification

The parameter abstracted over is NOT hardcoded as `a`.  When the
unifier constructs a `Lam`, the parameter is whatever variable `x`
was in `App(Var(m), x)`.  The concrete type `T` may contain zero or
one free variable:

- **One free variable**: that variable is the parameter position.
  The unifier abstracts over it.
- **Zero free variables**: constant constructor (e.g. `Lam(a, string)`
  — ignores its argument).  Valid but unusual.

For `* → *` constructors, one free variable is sufficient.  Multi-
parameter constructors (`* → * → *`) would use nested `Lam` via
curried application — a natural extension not required now.

### 5. Changes to monad typing

With general HKT unification, `monad()` works for any monad without
special-casing:

1. `monad()` has type
   `forall (m :: * → *). {bind: m a → (a → m b) → m b, return: a → m a} → {…}`
2. When called with `{bind: list-bind, return: list-ret}`, the
   unifier matches `m a` against `[a]` → decomposes via existing
   first-order rule → `m = Con("List")`
3. When called with `{bind: random-bind, return: random-ret}`, the
   unifier matches `m a` against `stream → {value: a, rest: stream}`
   → applies the new higher-order rule → `m = Lam(a, stream → {value: a, rest: stream})`
4. The return type of `monad()` substitutes the inferred `m` into all
   nine combinator types automatically

**Removals:**
- `derive_monad_block_type` and all `monad:` metadata type derivation code (PR #766)
- Explicit `!type:` annotations on `io`, `for`, `random`, `let` in `lib/prelude.eu`
- Explicit `!type:` annotation on `state` in `lib/state.eu`
- `Random` and `State` entries from the constructor kind table (already removed in PR #766)

**Retained:**
- `monad:` metadata for its original purpose: A10 element-type hints
  for LSP inlay display (e.g. showing `x: number` instead of
  `x: [number]` in monadic blocks).  This is a UI concern, not a
  type-checking mechanism.

### 6. Subtyping and consistency

- `App(Lam(x, body), arg)`: beta-reduce before subtype/consistency
  checking.  The reduced form is a concrete type; existing rules apply.
- `Lam` vs `Lam`: two lambdas are subtypes if their bodies are
  subtypes with the parameter treated as invariant (same variable).
  In practice this rarely arises — lambdas are typically applied
  immediately.
- `Lam` in isolation (not under `App`): treat as opaque — consistent
  with `any` at the gradual boundary.

### 7. Interaction with existing features

**Forall instantiation**: unchanged.  `Forall` freshens variables
before unification proceeds; the new rule fires during unification
of the freshened types.

**NonEmpty ↔ List widening**: unchanged.  This fires for
`App(Con("NonEmpty"), _)` vs `App(Con("List"), _)` — both in `App`
form, handled before the new rule.

**Prelude cache**: the `PreludeSummary` stores type schemes.  `Lam`
types in schemes are fine — they serialise/deserialise like any other
`Type` variant.

**Operator constraints**: unchanged.  Constraints are checked at
instantiation sites; the new rule operates at unification time.

## Process lesson

PR #766 implemented a domain-specific mechanism (monad-metadata type
derivation) where a general one (higher-order pattern unification) was
needed.  This happened because:

1. The coordinator's dispatch was imprecise — suggested metadata as
   the primary mechanism rather than as a fallback
2. The implementing agent did not push back on an architecturally
   unsound approach
3. The reviewing agent (Wicket) checked correctness but not design
   quality

**Mitigations for future work:**

- Significant type-system design changes must go through brainstorm →
  spec → review before implementation
- Agents should challenge instructions that feel architecturally
  wrong, with reasoning
- Wicket gains an "architectural smell" check: if a PR introduces a
  domain-specific mechanism where a general one exists or should
  exist, flag for coordinator/owner review

## Acceptance criteria

1. `Type::Lam(TypeVarId, Box<Type>)` variant exists
2. Unifier constructs `Lam` when matching `App(Var(m, *→*), Var(a))`
   against a non-App concrete type
3. `App(Lam(x, body), arg)` beta-reduces eagerly in `apply_subst`,
   subtyping, and consistency
4. `monad({bind: random-bind, return: random-ret})` infers
   `m = Lam(a, stream → {value: a, rest: stream})` via unification
   alone — no metadata involvement
5. `{ :for x: [1,2,3] }.(x * 2)` type-checks correctly
6. `{ :for x: 42 }` produces a type warning
7. `{ :random ... }` type-checks correctly
8. User-defined monads via `monad()` get correct derived combinator
   types without any `monad:` type annotation
9. No `derive_monad_block_type` or monad-metadata type derivation
   code remains
10. No explicit `!type:` annotations on `io`/`for`/`random`/`let`/`state`
11. `monad:` metadata retained only for A10 LSP element-type hints
12. All existing harness tests pass
13. `eu check lib/prelude.eu` exits 0
14. New harness tests exercise: List monad, IO monad, random/state
    monad (non-App constructor), identity monad, user-defined monad
