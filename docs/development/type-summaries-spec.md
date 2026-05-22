# Spec: Per-Module Type Summaries (Bead B7)

**Status**: Specification — ready to implement.
**Date**: 2026-05-19
**Branch**: `type-system-exploration`
**Companions**: [type-system-evolution.md](./type-system-evolution.md) (H9),
[type-system-bead-plan.md](./type-system-bead-plan.md) (TS-B7).

B7 restructures type checking around per-module type summaries — check
each module once, cache its export types, reuse them. It absorbed the
withdrawn A8 (an in-memory-only cache), because a *sound* cache turns
out to require the same per-module-checking restructure that the
persistent version needs.

## B7.1 Goal and why it is a restructure

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

## B7.2 Type summaries

A **module type summary** is the name → `TypeScheme` map of a unit's
exported bindings — exactly the `HashMap` the checker already produces
in `TypeCheckResult.types`, lifted to schemes so polymorphism is
preserved. Keying is **by name**, which is sound here: a unit's exports
are a flat, uniquely-named namespace, and importers resolve prelude/
imported names by name (no `Smid` needed — `Smid` keying matters only
for cross-module name collisions or binding-granularity invalidation,
neither of which arises for whole-module summaries).

## B7.3 The cache

- Content-address each summary by the **hash of the module source**
  (and, transitively, the hashes of its imports — a module's summary is
  valid only if its dependencies' summaries are).
- In-memory first: a `hash → summary` map; the prelude's summary is
  computed once per process and reused for every user file.
- Then persistent: the same summaries content-addressed on disk, so
  type information survives across `eu` invocations and LSP sessions.
- **Invalidation**: an import graph; when a module's source hash
  changes, its summary and every downstream summary are evicted.

## B7.4 Seeding a standalone check

The `Checker` gains the ability to be **seeded** with a pre-built
outer-scope environment (a summary). `check_unit(unit, seed)` checks one
translation unit's core with `seed` populating the outermost scope
frame; references not bound within the unit resolve against `seed` by
name. This is a modest extension — the `lookup_bound` → `lookup_name`
fallback path already exists; B7 makes the pre-seeded outer scope a
first-class input rather than an empty default.

## B7.5 LSP cross-file inference

With per-module summaries and the invalidation graph, the LSP
re-checks only what changed: editing a downstream file reuses upstream
summaries; editing an upstream file invalidates and re-checks its
dependents. Re-check latency drops from "re-walk prelude + all files"
to "re-check the edited unit against cached summaries". This is the
latency win that motivated the whole bead.

## B7.6 Scope boundaries

- **No whole-program inference.** Unannotated exports keep synthesising
  whatever the checker infers (often `any`); B7 caches that, it does not
  improve it. Whole-program inference is explicitly out (H9).
- B7 is independent of every other type-system bead; it touches the
  driver's check pipeline and the LSP, not the type lattice.

## Sequencing

B7 depends on nothing and may land any time. Its own internal order:
the standalone-checking restructure first (behaviour-preserving — see
the test plan), then the in-memory cache, then persistence, then the
LSP incremental re-check.

## Test plan

- A multi-file project re-check reuses unchanged module summaries
  (assert the prelude is checked once, not per file).
- Standalone unit checking against a seeded environment produces the
  **same warnings** as the old whole-merged check — the restructure is
  behaviour-preserving.
- Changing an upstream module invalidates downstream summaries.
- LSP re-check latency measurably drops.
- `eu check lib/prelude.eu` clean; full `cargo test` green; clippy clean.

## File-by-file change summary

| File | Change |
|------|--------|
| `src/driver/source.rs`, `src/driver/check.rs` | standalone per-unit checking; module source hashing; the summary cache + invalidation graph |
| `src/core/typecheck/check.rs` | `Checker` accepts a seeded outer-scope environment |
| `src/driver/lsp/` | cross-file incremental re-check off the summary cache |
| `tests/` | multi-file summary-reuse and invalidation tests |
