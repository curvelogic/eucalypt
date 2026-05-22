# Spec: Full Row-Variable Inference (Bead B9)

**Status**: Specification — ready to implement.
**Date**: 2026-05-19
**Branch**: `type-system-exploration`
**Companions**: [type-system-evolution.md](./type-system-evolution.md) (H3),
[type-system-bead-plan.md](./type-system-bead-plan.md) (TS-B9),
[row-polymorphism-and-dict-spec.md](./row-polymorphism-and-dict-spec.md).

B9 completes row polymorphism. A1 (Phase A) made *annotated*
row-polymorphic types work; B9 adds the *inference* deferred out of it.

## B9.1 Goal

A1 (shipped, Phase A) made *annotated* row-polymorphic types work — the
unifier propagates rows, and `merge`/`over` are annotated. A1 explicitly
deferred **inference**: an *unannotated* generic block function such as
`f(a, b): a merge(b)` leaves its block parameters `any`. B9 closes that
— the checker allocates fresh row variables at lambda boundaries and
generalises over them, so `f` infers
`{..r} -> {..s} -> {..r,..s}` with no annotation.

## B9.2 Mechanism

- **Allocation.** When the checker synthesises a lambda and a parameter
  is *used as a block* (projected, merged, passed to a block function),
  give that parameter an open record type with a **fresh row variable**
  rather than `any` — `Record { fields: {known projections…}, open:
  true, row: Some(fresh) }`. The known projections seed the `fields`;
  the row variable carries the unknown remainder.
- **Flow.** The unifier (already row-capable since A1) propagates the
  fresh row variable through the body — `merge`'s `{..r} -> {..s} ->
  {..r,..s}` binds the parameters' row variables and concatenates them
  into the result.
- **Generalisation.** When the lambda's scheme is generalised, the
  fresh row variables are **quantified** alongside ordinary type
  variables (A1's freshening already handles row variables per use —
  B9 adds them to the *generalisation* step, the dual operation).

## B9.3 The hard part — when to allocate

The judgement is "is this parameter used as a block?". Options:

- **Use-driven**: scan the lambda body; if a parameter is projected or
  passed where a record is expected, allocate a row-variable record for
  it. Precise but a second pass.
- **Optimistic + retract**: give every parameter a fresh row-variable
  record, let unification either confirm it (the body used it as a
  block) or unify it with a non-record (retract to that). Simpler;
  relies on the unifier degrading gracefully.

The use-driven approach is recommended — it is predictable and avoids
spurious row variables on non-block parameters. This judgement is the
substance of B9 and the reason it is its own bead rather than part of
A1.

## B9.4 Scope

B9 **depends on A1** (the row machinery and per-use freshening). It is
independent of the B1 HKT spine. P3 — the smallest, last Phase B bead;
its absence only means unannotated generic block combinators stay
`any`-typed, which is sound.

## Test plan

- An unannotated `f(a, b): a merge(b)` infers a row-polymorphic type.
- Row content flows through unannotated block code end to end.
- A non-block parameter does *not* acquire a spurious row variable.
- `eu check lib/prelude.eu` clean; full `cargo test` green; clippy clean.

## File-by-file change summary

| File | Change |
|------|--------|
| `src/core/typecheck/check.rs` | fresh row-variable allocation at lambda boundaries; row variables in scheme generalisation |
| `tests/harness/typecheck/` | row-inference tests |
