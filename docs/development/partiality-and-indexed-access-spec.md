# Spec: Partial Types & Dependent Indexed Access (Beads B5 / B6)

**Status**: Specification — ready to implement.
**Date**: 2026-05-19
**Branch**: `type-system-exploration`
**Companions**: [type-system-evolution.md](./type-system-evolution.md)
(H6b, H4a, H4b), [type-system-bead-plan.md](./type-system-bead-plan.md)
(TS-B5, TS-B6), [literal-types-and-narrowing-spec.md](./literal-types-and-narrowing-spec.md).

Two small Phase B beads. **B5** adds `Partial(T)` / `T?` — a type-level
mark for functions that can raise an `ExecutionError`. **B6** makes
`lookup` on a record with a *literal* key return the exact field type
and warn on a key typo. They share a PR; they interact (a non-literal
`lookup` is partial) but neither depends on the other.

## B5 — `Partial(T)` / `T?`

### B5.1 Goal

Mark fallible functions — those that can produce an `ExecutionError` —
so signatures and tooling are honest about partiality. Zero runtime cost
(errors already propagate); the payoff is documentation, hover, and a
warning when a fallible result is used where a total value is expected.

### B5.2 Representation — union sugar, not a new constructor

`Partial(T)` is **sugar for `T | ExecutionError`**, not a fresh opaque
constructor. Add one nullary type, `ExecutionError`; everything else
reuses the existing `Union` machinery. The DSL spells it `T?`:

```
T?            ≡  Partial(T)  ≡  T | ExecutionError
head : NonEmpty([a]) -> a            # total — A6 refined the input
nth  : number -> [a] -> a?           # partial — index may be out of range
```

Reusing unions means subtyping, consistency and display all work with
no new rules: `T | ExecutionError` displays as `T?` (a display
re-sugaring, like B1's `[a]`).

### B5.3 Checking behaviour — self-limiting under gradual typing

Because `Partial(T)` *is* a union, `is_subtype(T | ExecutionError, U)`
requires `ExecutionError <: U` — false unless `U` itself admits errors.
So a partial result flowing into a position typed as total `T` **warns**
— exactly the intended "used where a total value is expected" lint.

This is **not** noisy, because the checker is gradual: a partial result
flowing into unannotated (`any`) code is silent (`ExecutionError <: any`).
The warning fires only where someone has *annotated* a total type and
fed it a fallible value — which is precisely where the warning is
wanted. The noise is self-limiting by construction; no separate strict
mode is needed (though `eu check --strict` promotes it to an error like
any other warning).

### B5.4 Annotations

Annotate genuinely partial prelude functions with `T?`: `nth`,
`lookup`/`lookup-in` *with a non-literal key* (B6 handles the literal
case precisely), the `parse`-family, `panic`-adjacent helpers, integer
division by a possibly-zero divisor where statically unknown. Functions
A6 already made total by refinement (`head`/`tail : NonEmpty([a]) -> …`)
are **not** partial and keep their total types — B5 and A6 are
complementary: A6 *removes* partiality a refinement can express, B5
*documents* the residue.

## B6 — Dependent record indexed access

### B6.1 Goal

`lookup(:name, person)` on `person : {name: string, age: number}`
should return `string`, and `lookup(:naem, person)` should be a
**static warning** — a key typo caught before it errors at runtime.
This needs literal types (A4 — `LiteralSymbol`, shipped).

### B6.2 Mechanism — a call-site special case on `__LOOKUP`

`lookup(s, b): __LOOKUP(s, b)`. The checker special-cases the `__LOOKUP`
intrinsic, recognised **structurally** (an intrinsic node — the same
pattern as A5's `__COND`). On `App(__LOOKUP, [key, block])`:

| `key` | `block` | Result |
|-------|---------|--------|
| `LiteralSymbol(k)` | `Record` with field `k` | that field's type |
| `LiteralSymbol(k)` | **closed** `Record` *without* `k` | `any` + **warning** (key typo) |
| `LiteralSymbol(k)` | open / row-variable `Record` without known `k` | `any`, no warning (k may be in the tail) |
| `LiteralSymbol(k)` | `Dict(v)` | `v` |
| non-literal `symbol` | any block | `any` + the call is `Partial` (B5) |

The literal-key/closed-record/absent case is the prize. No new `Type`
variant is needed: the indexed access `r[k]` is resolved **eagerly** at
the call site whenever `k` is a known literal, and falls back to `any`
otherwise — there is never a deferred `r[k]` type to represent. (The
fully dependent `(k :: symbol) -> {..r} -> r[k]` form is not built; the
eager resolution covers every case that pays rent — "no complexity for
its own sake".)

This composes with A2's `Dict` annotation of `lookup` (`symbol ->
Dict(a) -> a`): on a `Dict` the special case yields `a`, consistent with
the annotation; on a named record it is more precise.

### B6.3 H4b — literal index on tuples and short lists

The same treatment for the list-index intrinsic (`!!` / its `__LOOKUP`-
analogue for integers): on a `Tuple` (a short list literal carries
per-position types — see the list-synthesis rework in
literal-types-and-narrowing-spec.md §A6.3) with a **literal** integer
index in range → the element type at that position; literal index out
of range → `any` + warning; non-literal index → `any` + `Partial`. A
minor follow-on to B6.2, same call-site-special-case shape.

### B6.4 Non-literal keys are partial

A `lookup` with a non-literal key can fail at runtime (the key may be
absent). Its result is `any?` (`= any | ExecutionError`, B5). So B6's
non-literal row and B5's `Partial` annotation of `lookup` agree: the
non-literal `lookup` *is* the partial one.

## Sequencing

One PR. B6 depends on A4 (literal types — Phase A, shipped). B5 is
independent. Order within the PR: B5 (`ExecutionError`, `T?` sugar,
display, annotations) then B6 (the `__LOOKUP` special case, which uses
B5's `Partial` for the non-literal row). Neither depends on B1; if B1
has landed, `ExecutionError` is a nullary `Con`, otherwise a nullary
variant — immaterial to this spec.

## Test plan

- **B5** — unit: `T?` parses to `T | ExecutionError`, displays back as
  `T?`; a partial result warns against an annotated total parameter and
  is silent against `any`. Harness: an annotated partial prelude
  function; a total context catching a partial value.
- **B6** — harness: `lookup(:name, person)` yields the field type;
  `lookup(:naem, person)` warns on a closed record; an open record
  yields `any` with no warning; `lookup` on a `Dict` yields the value
  type; a non-literal-key `lookup` is `any?`. Tuple literal-index
  resolution and out-of-range warning.
- `eu check lib/prelude.eu` clean; full `cargo test` green; clippy clean.

## File-by-file change summary

| File | B5 | B6 |
|------|----|----|
| `types.rs` | nullary `ExecutionError`; display re-sugars `T \| ExecutionError` → `T?` | — |
| `parse.rs` | `T?` postfix sugar → `Union([T, ExecutionError])` | — |
| `subtype.rs` | — (union machinery suffices) | — |
| `check.rs` | — | `__LOOKUP` (and list-index intrinsic) call-site special case; literal-key resolution; missing-key warning |
| `lib/prelude.eu` | `T?` annotations on partial functions | — |
| `tests/harness/typecheck/` | partiality tests | indexed-access tests |
