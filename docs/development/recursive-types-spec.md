# Spec: Recursive Types (Bead A3)

**Status**: Specification — ready to implement.
**Date**: 2026-05-18
**Branch**: `type-system-exploration`
**Companions**: [type-system-evolution.md](./type-system-evolution.md) (H14),
[type-system-bead-plan.md](./type-system-bead-plan.md) (TS-A3),
[row-polymorphism-and-dict-spec.md](./row-polymorphism-and-dict-spec.md).

Structural data is recursive — `Json`, `Tree`, AST-shaped values cannot
be typed today. A3 adds **equirecursive** types: a self-referential type
alias resolves to a `μ` form that the checker treats as equal to its
unfolding everywhere.

## A latent bug this also fixes

`resolve_aliases_in_type` (check.rs) walks a type replacing each
`Type::Var` that names an alias with the alias's registered type — with
**no cycle detection**. A self-referential `type-def` therefore makes
the checker **infinite-loop**: resolving `Json` → its body → which
contains `Json` → resolved again → … forever. A3's resolver change is
the fix; until A3 lands, a recursive alias is a hang. (If A3 slips, a
standalone cycle-detection guard — resolve to `any` on a detected cycle,
with a warning — should land first as a hang-stopper.)

## Decisions taken

1. **Equirecursive, not isorecursive.** The user writes a recursive
   alias; the checker treats the type as equal to its unfolding —
   no explicit `roll`/`unroll`. Isorecursive's explicit coercions are
   the wrong ergonomics for a config-and-data language.
2. **No new DSL syntax.** Recursive types arise *only* from
   self-referential aliases in `types:` / `type-def:` metadata. The DSL
   grammar is unchanged; the **alias resolver** is what builds the `μ`
   form. There is no surface `μ` syntax.

## Background — what exists today

- `Type` (types.rs) has **no** recursive variant — no `Mu`, `Fix` or
  equivalent.
- `Type::Var(TypeVarId)` is currently overloaded for two roles:
  unification variables (lowercase, allocated fresh) and alias
  references (capitalised, resolved against the alias map *before*
  checking proper). After alias resolution no alias-reference `Var`s
  survive — only unification variables.
- `resolve_aliases_in_type` recurses structurally, substituting alias
  `Var`s; **no visited-set, no cycle guard** (the loop above).
- Aliases are a `HashMap<String, Type>` (`AliasMap`), populated from
  `types:` blocks and `type-def:` declarations during the checker walk.

## A3.1 Representation

Add to `Type` (types.rs):

```rust
/// Equirecursive type: `Mu(x, body)` ≡ `body[x := Mu(x, body)]`.
/// References to `x` inside `body` are `Type::Var(x)`.
Mu(TypeVarId, Box<Type>),
```

The recursion variable is a `Type::Var(x)` *bound* by the enclosing
`Mu`. This is a third role for `Var` — but a safe one: alias-reference
`Var`s are resolved away before checking, so the only `Var`s an
operation meets are unification variables (free) and µ-bound variables
(bound by an enclosing `Mu` on the current path). Every operation that
recurses into types and must tell them apart already has, or can
cheaply carry, the set of in-scope µ-binders (subtyping carries an
assumed-pairs set anyway — §A3.3). A `Var` whose id matches an
enclosing `Mu` binder is the recursion variable; any other `Var` is a
unification variable.

`TypeVarId` for the binder: reuse the **alias name** (`Json`). Then the
binder, the recursion variable, and the printer all agree, and display
is trivially finite (§A3.4).

`humanise`, `apply_subst`, `resolve_aliases_in_type`'s structural recursion,
and any other `Type`-walking code gain a `Mu` arm that recurses into the
body. `apply_subst` must **not** substitute the µ-bound variable (it is
bound, not a unification variable) — skip the binder's id.

## A3.2 Building `μ` — the alias resolver

`resolve_aliases_in_type` gains cycle detection and µ-construction.
Resolving an alias name `N`:

1. Track a `resolving: Vec<TypeVarId>` stack of alias names currently
   being expanded.
2. To resolve `Type::Var(N)` where `N` is an alias:
   - If `N` is already on `resolving` → this is a back-edge. Emit
     `Type::Var(N)` *unchanged* — it becomes the recursion variable,
     captured by the `Mu(N, …)` that the outer expansion of `N` will
     wrap.
   - Otherwise push `N`, resolve its registered body, pop `N`. If the
     resolved body *contains* `Type::Var(N)` (a back-edge fired), wrap
     the result in `Mu(N, body)`. If it does not, `N` was not actually
     recursive — return the body unwrapped.

So `Json = "number | string | bool | null | [Json] | Dict(Json)"`
resolves to `Mu(Json, number | string | bool | null | [Mu-var Json] |
Dict(Mu-var Json))`. Mutually recursive aliases (`A` mentions `B`
mentions `A`) resolve to nested `Mu`s — the same back-edge logic
handles them; document the nesting order.

## A3.3 Equirecursive subtyping, consistency, unification

Equirecursive operations terminate by **coinduction**: assume the goal,
unfold once, and succeed on a repeat.

`is_subtype` gains an internal form `is_subtype_co(s, t, assumed)` where
`assumed` is a set of `(Type, Type)` pairs already in progress:

- `(S, T)` already in `assumed` → **true** (coinductive hypothesis).
- `is_subtype_co(Mu(x, a), t, assumed)` → add `(Mu(x,a), t)` to
  `assumed`, recurse on `(a[x := Mu(x,a)], t)`.
- Symmetric for `Mu` on the right.
- All existing arms unchanged, threaded with `assumed`.

The public `is_subtype(s, t)` calls `is_subtype_co(s, t, ∅)`. Same
treatment for `is_consistent`. `unify` gains the analogous unfold —
unifying `Mu(x,a)` with `T` unfolds the `Mu` once, guarded by an
assumed-pairs set to terminate; two `Mu`s unify by unfolding both.

Termination: the assumed-pairs set is bounded by the (finite) set of
distinct sub-term pairs of the two finite µ-types, so the coinduction
closes. This is the standard Amadio-Cardelli / Brandt-Henglein
algorithm.

## A3.4 Display

The printer must not unfold a `Mu` infinitely. Because the binder reuses
the alias name, this is free: print `Mu(Json, _)` as **`Json`**, and the
recursion variable `Var(Json)` inside as **`Json`**. The printer never
descends into a `Mu` body for top-level display — a recursive type
prints as its alias name. (`humanise` of a *scheme* containing an
anonymous `Mu` with no alias name — should not arise, since µ-types only
come from named aliases — but as a fallback print `μX. body` with the
body printed one level deep and inner recursion variables as `X`.)

## A3.5 Scope and the `Dict` interaction

The motivating `Json` type wants `Dict(Json)` for its object case (A2)
and `[Json]` for its array case — so A3 lands **after** A1/A2. The `Mu`
core (representation, resolver, coinductive subtyping, display) is
independent of A1/A2 and could be built in parallel; only the
end-to-end `Json` acceptance test needs `Dict`.

`Mu` composes with every other constructor: `[Mu …]`, `Dict(Mu …)`,
`Union` containing a `Mu`, `Record` fields of `Mu` type. The coinductive
arms handle all of them — the `assumed` set short-circuits any cycle
regardless of which constructors the cycle threads through.

## Test plan

Harness tests in `tests/harness/typecheck/`; Rust unit tests per module.

- **Resolver**: a self-referential `type-def` / `types:` entry resolves
  to a `Mu` and **does not hang** (this is the regression test for the
  latent bug); a non-recursive alias still resolves flat (no spurious
  `Mu`); mutually recursive aliases resolve to nested `Mu`s.
- **Subtyping**: `is_subtype` over two recursive types terminates;
  `Mu(x, a)` is mutually a subtype of its one-step unfolding
  (equirecursive equality); a recursive `Tree` subtypes a wider
  recursive `Tree`.
- **End-to-end**: a recursive `Json` alias type-checks a
  `Json -> string` function; round-trip display of a `Json`-typed value
  is finite and prints `Json`.
- **Regression**: `eu check lib/prelude.eu` stays warning-free; full
  `cargo test` green; clippy clean.

## File-by-file change summary

| File | Change |
|------|--------|
| `types.rs` | `Mu(TypeVarId, Box<Type>)` variant; `Display`/`humanise` print the binder name, no unfold; `apply_subst` recurses the body, skips the bound id |
| `subtype.rs` | `is_subtype_co` / consistency with an assumed-pairs set; `Mu` unfold arms |
| `unify.rs` | `Mu` unfold arms with an assumed-pairs guard |
| `check.rs` | `resolve_aliases_in_type` — cycle detection + `Mu` construction; structural `Mu` arm in every other `Type` walk |
| `tests/harness/typecheck/` | recursive-alias / `Json` tests, including the no-hang regression |
