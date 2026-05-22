# Spec: Per-Module Type Summaries & Full Row Inference (Beads B7 / B9)

**Status**: Specification — ready to implement.
**Date**: 2026-05-19
**Branch**: `type-system-exploration`
**Companions**: [type-system-evolution.md](./type-system-evolution.md) (H9,
H3), [type-system-bead-plan.md](./type-system-bead-plan.md) (TS-B7,
TS-B9), [row-polymorphism-and-dict-spec.md](./row-polymorphism-and-dict-spec.md).

Two independent Phase B beads, paired only for delivery convenience.
**B7** restructures checking around per-module type summaries (it
absorbed the withdrawn A8). **B9** completes row polymorphism with
fresh-row-variable *inference* (deferred out of A1).

## B7 — Per-module type summaries

### B7.1 Goal and why it is a restructure

Today `type_check_path_full` *merges* the prelude and every user file
into one expression, prunes it, and checks it once from scratch
(driver/check.rs). There is no module identity, no cache; the prelude
is fully re-walked on every check.

A naive "cache the prelude's type environment" is **unsound**:
`eliminate` (dead-binding pruning) runs *before* the checker, and prunes
a different subset of the prelude for every user file — a cached
environment from one run is missing bindings for the next (see
evolution-doc H9; this is why A8 was withdrawn). The sound design is a
**per-module-checking restructure**:

> Check the prelude **standalone** (its own load → translate → cook,
> *without* prune — standalone, every binding is a root), producing a
> complete export type summary. Check each user unit **standalone**,
> with the prelude's summary seeded as a name-keyed outer scope. Prelude
> references resolve by name via the existing `lookup_bound` →
> `lookup_name` fallback (which the code already provides "for
> references to bindings outside the checker's root expression").

This sidesteps pruning entirely (the prelude is checked unpruned; user
units are checked without the prelude merged in). It is the foundation
both the in-memory cache and the persistent store build on, so the
former A8/B7 "in-memory vs persistent" split is dropped — B7 is the
whole feature.

### B7.2 Type summaries

A **module type summary** is the name → `TypeScheme` map of a unit's
exported bindings — exactly the `HashMap` the checker already produces
in `TypeCheckResult.types`, lifted to schemes so polymorphism is
preserved. Keying is **by name**, which is sound here: a unit's exports
are a flat, uniquely-named namespace, and importers resolve prelude/
imported names by name (no `Smid` needed — `Smid` keying matters only
for cross-module name collisions or binding-granularity invalidation,
neither of which arises for whole-module summaries).

### B7.3 The cache

- Content-address each summary by the **hash of the module source**
  (and, transitively, the hashes of its imports — a module's summary is
  valid only if its dependencies' summaries are).
- In-memory first: a `hash → summary` map; the prelude's summary is
  computed once per process and reused for every user file.
- Then persistent: the same summaries content-addressed on disk, so
  type information survives across `eu` invocations and LSP sessions.
- **Invalidation**: an import graph; when a module's source hash
  changes, its summary and every downstream summary are evicted.

### B7.4 Seeding a standalone check

The `Checker` gains the ability to be **seeded** with a pre-built
outer-scope environment (a summary). `check_unit(unit, seed)` checks one
translation unit's core with `seed` populating the outermost scope
frame; references not bound within the unit resolve against `seed` by
name. This is a modest extension — the `lookup_bound` → `lookup_name`
fallback path already exists; B7 makes the pre-seeded outer scope a
first-class input rather than an empty default.

### B7.5 LSP cross-file inference

With per-module summaries and the invalidation graph, the LSP
re-checks only what changed: editing a downstream file reuses upstream
summaries; editing an upstream file invalidates and re-checks its
dependents. Re-check latency drops from "re-walk prelude + all files"
to "re-check the edited unit against cached summaries". This is the
latency win that motivated the whole bead.

### B7.6 Scope boundaries

- **No whole-program inference.** Unannotated exports keep synthesising
  whatever the checker infers (often `any`); B7 caches that, it does not
  improve it. Whole-program inference is explicitly out (H9).
- B7 is independent of B1–B6; it touches the driver's check pipeline
  and the LSP, not the type lattice.

## B9 — Full row-variable inference at lambda boundaries

### B9.1 Goal

A1 (shipped, Phase A) made *annotated* row-polymorphic types work — the
unifier propagates rows, and `merge`/`over` are annotated. A1 explicitly
deferred **inference**: an *unannotated* generic block function such as
`f(a, b): a merge(b)` leaves its block parameters `any`. B9 closes that
— the checker allocates fresh row variables at lambda boundaries and
generalises over them, so `f` infers
`{..r} -> {..s} -> {..r,..s}` with no annotation.

### B9.2 Mechanism

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

### B9.3 The hard part — when to allocate

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

### B9.4 Scope

B9 **depends on A1** (the row machinery and per-use freshening). It is
independent of the B1 HKT spine. P3 — the smallest, last Phase B bead;
its absence only means unannotated generic block combinators stay
`any`-typed, which is sound.

## Sequencing

B7 and B9 are independent and may land in either order, after their
respective dependencies (B7: none; B9: A1). Paired in one PR only for
convenience; split if scheduling prefers.

## Test plan

- **B7** — a multi-file project re-check reuses unchanged module
  summaries (assert the prelude is checked once, not per file);
  standalone unit checking against a seeded environment produces the
  same warnings as the old whole-merged check (behaviour-preserving);
  changing an upstream module invalidates downstream summaries; LSP
  re-check latency measurably drops.
- **B9** — an unannotated `f(a, b): a merge(b)` infers a row-polymorphic
  type; row content flows through unannotated block code; a non-block
  parameter does *not* acquire a spurious row variable.
- `eu check lib/prelude.eu` clean; full `cargo test` green; clippy clean.

## File-by-file change summary

| File | B7 | B9 |
|------|----|----|
| `src/driver/source.rs`, `check.rs` | standalone per-unit checking; module source hashing; the summary cache + invalidation graph | — |
| `src/core/typecheck/check.rs` | `Checker` accepts a seeded outer-scope environment | fresh row-variable allocation at lambda boundaries; row variables in generalisation |
| `src/driver/lsp/` | cross-file incremental re-check off the summary cache | — |
| `tests/` | multi-file summary-reuse and invalidation tests | row-inference tests |
