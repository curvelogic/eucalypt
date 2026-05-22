# Spec: Metadata-Channel Typing (Bead B3)

**Status**: Specification — ready to implement.
**Date**: 2026-05-19
**Branch**: `type-system-exploration`
**Companions**: [type-system-evolution.md](./type-system-evolution.md) (H2),
[type-system-bead-plan.md](./type-system-bead-plan.md) (TS-B3, TS-B4).

Every eucalypt value can carry a *metadata channel* (the backtick form
attaches a metadata block). The type system has no way to describe it.
B3 closes that — but, with B4 won't-do, at a deliberately modest scope.

## Scope decision — validation, not a lattice feature

The original case for metadata typing (H2) had two halves:

1. **Downstream** — B4, typing the van Laarhoven lens kernel, needs
   metadata to be a first-class *type* that flows through the lattice
   (a function value whose metadata has shape `{fmap, pure, ap}`).
2. **Standalone** — fixity/precedence metadata, `target`, `doc:`,
   `import:`, `type:`, `monad:` are fixed-shape metadata that is
   currently unvalidated by the type checker.

**B4 is won't-do.** With its downstream consumer gone, metadata no
longer needs to *flow* through the type system — nothing reads a value's
metadata type and propagates it. What remains is half (2): **validating
metadata blocks against the known shape of their keys**. That does not
need a new `Type` variant or metadata subtyping; it is a **checker pass
over `Meta` nodes against a fixed schema**.

So B3 ships as **metadata schema validation**. The heavier,
lattice-integrated form (`Type::WithMeta`, metadata subtyping, metadata
types flowing through inference) is described in §B3.5 as the deferred
alternative — it should only be built if a future feature genuinely
needs metadata to *flow*, and B4 was the only such feature. This is the
"no complexity for its own sake" cut: deliver the validation that pays
rent, not the lattice machinery that does not.

## Background

- The checker sees `Expr::Meta(smid, value, meta)` for every
  metadata-annotated value. `synthesise_meta` (check.rs) already
  inspects the metadata block — but only to *extract type information
  from it* (`type:`, `__type_hint`, `types:`, `type-def:`). It does not
  *check the metadata block itself*.
- Metadata is consumed by various passes: cook reads fixity
  (`associates:`, `precedence:`); the loader reads `import:`, `target:`;
  tooling reads `doc:`; the desugarer reads `monad:`. Malformed metadata
  is, today, either silently ignored or fails late in the consuming
  pass with a poor diagnostic.

## B3.1 The metadata schema

A fixed table of **known metadata keys** and their expected types:

| Key | Expected type | Notes |
|-----|---------------|-------|
| `doc` | `string` | |
| `precedence` | `number` | operator fixity |
| `associates` | `:left \| :right \| :none` | operator fixity |
| `target` | `string` | |
| `format` | `string` | target output format |
| `import` | `string \| [string]` | |
| `type` | `string` | the type annotation itself |
| `type-def` | `string` | alias name |
| `types` | `Dict(string)` | alias block |
| `monad` | `string` | monad namespace name |
| `export` | `:suppress \| …` | per the loader's accepted values |
| `main` | `bool` | |

The table lives beside the checker (it is checker-adjacent
configuration, not user-facing). It is the authority on what a
well-formed metadata block looks like; the consuming passes (cook,
loader) already encode the same expectations informally — B3 makes one
declarative source of truth.

## B3.2 The validation pass

In `synthesise_meta`, after the existing type-extraction logic: for each
key in the metadata block that appears in the schema, synthesise the
value's type and check it is consistent with the schema's expected type.
On mismatch, emit a type warning (`"precedence: expected number, found
string"`). Keys **not** in the schema are left alone — user-defined
metadata keys are legitimate and unconstrained.

The pass is shallow and local: it does not change synthesis of the
annotated value (the `Meta` node's result type is still the value's
type, exactly as today), it only adds warnings for malformed known
metadata. It is sound by construction — it only ever *adds* a warning,
never alters a type.

## B3.3 Interaction with the gradual boundary

A metadata value that synthesises `any` (computed metadata, an
interpolated string) satisfies any schema entry — gradual, silent.
Metadata is frequently literal (`` ` { doc: "…" } ``), so the common
case is precise.

## B3.4 What B3 does *not* do

- No `Type` variant for metadata. The metadata channel is *validated*,
  not *represented in the type lattice*.
- No metadata subtyping, no metadata types flowing through inference,
  no DSL syntax for metadata-carrying types.
- No typing of the lens kernel (B4, won't-do).

## B3.5 The deferred heavier form (not in B3)

For the record, the lattice-integrated version — should a future
feature ever need metadata to *flow* — would be:

- `Type::WithMeta(Box<Type>, Box<Type>)` — a value type paired with a
  metadata (record) type.
- Subtyping: `WithMeta(T,M) <: WithMeta(T',M')` iff `T<:T'` ∧ `M<:M'`;
  `WithMeta(T,M) <: T`; `T <: WithMeta(T, {..})`.
- `synthesise_meta` produces `WithMeta` for significant metadata;
  metadata types flow through application and lookup.
- DSL syntax `T @ M`.

This is real type-system machinery and is justified only by a consumer
that reads metadata types — i.e. B4. With B4 won't-do it is **not
built**. If a metadata-flow need re-emerges, this section is the design
sketch; it would be a fresh bead, not a silent expansion of B3.

## Test plan

- Unit: schema lookup; a malformed known key (`precedence: "high"`)
  warns; an unknown user key is untouched; a computed (`any`) metadata
  value is accepted.
- Harness: malformed `precedence`/`associates`/`doc` warn; well-formed
  metadata is silent; user-defined metadata keys never warn.
- `eu check lib/prelude.eu` clean (the prelude's own metadata is
  well-formed — if the pass flags anything, that is a real prelude bug
  to fix); full `cargo test` green; clippy clean.

## File-by-file change summary

| File | Change |
|------|--------|
| `check.rs` | the metadata-key schema table; the validation pass in `synthesise_meta` |
| `tests/harness/typecheck/` | metadata-validation tests |
