# Spec: Structural Operator Constraints & HKT Monad Types (Beads B2 / B8)

**Status**: Specification — ready to implement.
**Date**: 2026-05-19
**Branch**: `type-system-exploration`
**Companions**: [type-system-evolution.md](./type-system-evolution.md) (H10,
H1), [higher-kinded-types-spec.md](./higher-kinded-types-spec.md) (B1),
[type-system-bead-plan.md](./type-system-bead-plan.md) (TS-B2, TS-B8).

Two beads. **B2** adds *structural operator constraints* — "type classes
without classes" — so functions like `min` can be typed `valid wherever
< accepts a`. **B8** *applies* B1's HKT to the monad namespaces so
desugared `bind` chains type-check directly. They share a PR because
both are about giving the prelude honestly polymorphic types; they are
otherwise independent — B2 does **not** depend on B1.

## B2 — Structural operator constraints

### B2.1 Goal

`min`/`max`/`clamp` and namespace-generic functions cannot be typed
today without an exhaustive union of overloads. A constraint expresses
the polymorphism abstractly:

```
` { type: "<(a, a) => a -> a -> a" }
min: …
```

Read: "`a -> a -> a`, valid wherever `<` accepts `a, a`". The dictionary
is the operator's own set of declared overloads; resolution is "find a
declared overload that fits" — finite enumeration, no class hierarchy,
no implicit dictionary passing.

### B2.2 Representation

The `Constraint` type **already exists** (types.rs ~25) and
`TypeScheme.constraints` is **already** a `Vec<Constraint>` — verified;
the field is documented "reserved for future constraint support — always
empty for now", set by `mono()`/`poly()` and never read. B2 makes it
live; no new type is needed. The existing definition is:

```rust
struct Constraint {
    /// The operator / function name — note the field is `function`,
    /// not `name`: e.g. "<", "+", "str.of".
    function: String,
    /// The argument types the operation must accept.
    args: Vec<Type>,
}
```

`<(a, a)` is `Constraint { function: "<", args: [Var(a), Var(a)] }`. A
scheme may carry several; they are an unordered conjunction sharing only
the type variables already unified by the rest of the signature.

### B2.3 DSL

One grammar production: a comma-separated constraint list before `=>`.

```
<(a, a) => a -> a -> a
<(a, a), +(a, a) => a -> a -> a
str.of(a) => [a] -> [string]
```

`name` is an operator glyph or a dotted namespace path. Parse the
prefix up to `=>`, then the body type. No `forall`-level entanglement —
the constraint variables are just the body's type variables.

### B2.4 Resolution

A constraint is **discharged** when the type variables in its `args`
become concrete, and **propagated** otherwise.

- **Declared overloads.** The "overload set" of `name` is the set of
  function types `name` is annotated with: a single `Function` type is
  one overload; a `Union` of `Function` types is several. The checker
  reads `name`'s type from its binding.
- **Discharge.** When a constrained scheme is instantiated at a call
  site and `args` resolve to concrete types `(T₁, …)`, check that some
  overload of `name` accepts `(T₁, …)` — i.e. its parameter types are
  consistent (`is_consistent`) with the `args`. Success → discharged.
  Failure → a type warning (`"min: < does not accept (block, block)"`).
- **Propagation.** If an `args` variable is still unbound — the call is
  itself polymorphic — the constraint is *carried* into the enclosing
  binding's scheme (its `constraints` vector gains the constraint with
  the variable renamed). Constrained polymorphism composes.
- **Gradual.** If an `args` type is `any`, the constraint is vacuously
  satisfied — the gradual boundary stays silent.

Resolution is finite enumeration over a small overload set, conjunction
checked left-to-right, no backtracking.

### B2.5 Forwards compatibility

A union-overload annotation (`(number -> number -> bool) | (string ->
string -> bool)`) is a strict *specialisation* of a constraint — it
names the overloads inline. Existing union-overload annotations keep
working unchanged; constraints are the more abstract spelling for new
ones.

### B2.6 Scope

B2 is independent of B1 — constraints are value-level (operators),
kinds are type-level. It may land before, after, or alongside B1.
6.1's `min`/`max`/comparison stay union-typed; B2 retypes them with
constraints in Phase B.

---

## B8 — Type the monad namespaces with HKT

### B8.1 Goal

With B1's kinds, `App`/`Con` and `Forall`, give the monad namespaces
and `monad()` their real polymorphic types, so a desugared monadic
block's `bind` chain type-checks **directly** — a wrong binding fails
because its type does not unify, with no `__type_hint` needed.

### B8.2 The monad namespaces

The monad namespaces are **`io`, `for`, `random`, `let`** (in
`lib/prelude.eu`) and **`state`** (in `lib/state.eu` — a separate
library, imported on demand, not the auto-loaded prelude). All five are
`* -> *` monads:

```
io     — Con("IO"),     IO actions
for    — Con("List"),   the list monad ([a])
random — Con("Random"), a new opaque * -> * constructor
let    — the identity monad (m a ≡ a; bind(m,f) = f(m))
state  — Con("State"),  a state action  s -> {value: a, state: s}
```

Crucially, **`for`, `random`, `let` and `state` are each defined via
`monad()`** (`for: monad({bind(m,f): m mapcat(f), return(v): [v]})`;
`state: monad{bind: state-bind, return: state-ret}` in `state.eu`;
etc.). So once `monad()` carries its HKT signature (§B8.3) those four
namespaces *inherit* typed combinators with no separate annotation.
B8's hand-written annotation work is therefore essentially **`monad()`
itself** plus the *primitive* `bind`/`return` each namespace passes to
it — `io`'s (if `io` is not itself built via `monad()` — confirm during
implementation), and `state.eu`'s `state-bind`/`state-ret`. `Random` is
a new opaque `* -> *` `Con`; `let`'s constructor is the identity
(`m a` ≡ `a`); `State` is `* -> *` over the value type. Note `state.eu`
*already* carries a `!`-asserted `type:` annotation on `state` — B8
should reconcile its HKT signature with that.

### B8.3 `monad()`

`monad()` gets the full rank-2 HKT signature (H1): it takes a record
providing `bind`/`return` over some `m :: * -> *` and returns a record
of its **nine** derived combinators — `bind`, `return`, `map`, `then`,
`and-then`, `join`, `sequence`, `map-m`, `filter-m` (verified against
the prelude `monad(m)` definition) — each separately polymorphic in its
own `a`, `b` (so each combinator field is a `Type::Forall` — B1.3)
while sharing `m`. B1 supplies every piece — `Con`, `App`, `Forall`,
kinds; B8 simply writes the annotation. The result is that a user monad
built with `monad()` *inherits* correct types for all derived
combinators — and so do `for`/`let`/`random`/`state`.

Note the monad `then` (an `m`-sequencing combinator, `monad()`'s
output) is distinct from the top-level branch `then` of A5 — different
bindings, no conflict.

### B8.4 Relationship to A9 / A10

A9 (`monad-type-checking-spec.md`) injects `__type_hint` nodes on
monadic block bindings so the checker can warn before HKT exists. Once
B8 lands, the desugared `bind` chain checks directly — so the
`__type_hint`'s **checking role is superseded**. The `monad:` metadata
field itself **lives on**: it tells the *desugarer* a block is monadic
and which namespace's `bind`/`return` to use, and it drives LSP inlay
hints and hover — none of which HKT affects.

Whether to *stop injecting* `__type_hint` once B8 lands is a minor
cleanup, not a correctness issue: redundant hints are harmless (they
check the same thing the direct typing already does). Recommend leaving
the injection in place initially and removing it as separate tidy-up
once B8 is proven; A9/A10 need no rework for B8.

### B8.5 Scope

B8 **depends on B1** and is otherwise small — it is annotation work plus
the `Con("Random")` and `Con("State")` kind-table entries (`state` lives
in `lib/state.eu`, so B8 touches that file too). Its acceptance is
that a monadic block (`{ :for x: [1,2,3], … }`) type-checks via direct
unification and a wrong binding (`{ :for x: 42 }`) warns without the
hint mechanism.

---

## Sequencing

One PR. B2 and B8 are independent; B8 depends on B1, B2 does not. Order
within the PR: B2 (constraint representation, DSL, resolution; retype
`min`/`max`) then B8 (monad-namespace and `monad()` annotations). If B1
slips, B2 still ships; B8 waits for B1.

## Test plan

- **B2** — unit: constraint discharge against a single and a union
  overload set; propagation when an arg is unbound; vacuous satisfaction
  on `any`. Harness: `min(3, 4)` checks; `min(blockA, blockB)` warns;
  a user function carrying a propagated constraint type-checks; an
  existing union-overload annotation still works.
- **B8** — harness: a `{ :for x: […] }` block checks via direct
  unification; `{ :for x: 42 }` warns; a user monad from `monad()` gets
  correct derived-combinator types; `io`/`for`/`let`/`random`/`state` chains
  check. Confirm A9's existing acceptance tests still pass (hints
  redundant, not wrong).
- Full `cargo test` green; `eu check lib/prelude.eu` clean; clippy clean.

## File-by-file change summary

| File | B2 | B8 |
|------|----|----|
| `types.rs` | `Constraint` fields live | `Con("Random")`/`Con("State")` kind-table entries (per B1) |
| `parse.rs` | constraint-list prefix before `=>` | — |
| `check.rs` | constraint discharge / propagation at instantiation; overload enumeration | — |
| `lib/prelude.eu` | retype `min`, `max`, comparison-generic functions with constraints | HKT annotation for `monad()` (and `io`'s primitive `bind`/`return`); `for`/`let`/`random` inherit via `monad()` |
| `lib/state.eu` | — | reconcile the existing `state` annotation with the HKT `monad()` signature (`state` inherits via `monad()`) |
| `tests/harness/typecheck/` | constraint tests | monad-namespace / `monad()` tests |
