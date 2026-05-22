# Spec: Prelude Type-Summary Cache (Bead B7)

**Status**: Specification — ready to implement.
**Date**: 2026-05-19
**Branch**: `type-system-exploration`
**Companions**: [type-system-evolution.md](./type-system-evolution.md) (H9),
[type-system-bead-plan.md](./type-system-bead-plan.md) (TS-B7).

B7 caches the type information of the **prelude** so it is computed once
rather than re-walked on every check. It absorbed the withdrawn A8.

## B7.1 Scope — prelude only

For eucalypt, the type checker exists to *support writing eucalypt* —
it is tooling, and LSP responsiveness is its main job. The LSP today
re-checks the whole world (prelude + open files) on every change; the
prelude (~2200 lines) dominates that cost. B7 removes it from the hot
path.

B7 is deliberately scoped to the **prelude alone**:

- The prelude is **fixed**, **merged first** as the base, and its
  bindings reference only the prelude. Nothing merged on top can rebind
  what the prelude's own definitions see, so the prelude's types are
  **stable** — independent of any user file.
- General per-*user*-module summaries are **out of scope**. A user
  unit's exported types are not guaranteed stable under positional
  merge (a later-merged unit can override a name an earlier unit's
  bindings reference — see §B7.9), and the payoff is small (user files
  are short). Prelude-only is the sound subset *and* ~all the cost
  saving.

## B7.2 The summary — types **and** aliases

A prelude type summary is **two** maps, not one:

```rust
struct PreludeSummary {
    /// Exported binding name → its type scheme.
    bindings: HashMap<String, TypeScheme>,
    /// Type aliases the prelude registers (from its `types:` /
    /// `type-def:` metadata).
    aliases: AliasMap,
}
```

The `bindings` map is what the checker already produces in
`TypeCheckResult.types`, lifted to `TypeScheme` so polymorphism
survives. The `aliases` map matters because user code can reference
prelude-defined type aliases in its annotations — a summary of binding
types alone would lose them. (This is the unit-metadata point: the only
*type-relevant* unit metadata is the `types:` / `type-def:` blocks; the
loader's merging of `import:` / `target:` metadata does not affect
types and needs nothing from B7.)

## B7.3 Producing the summary

Check the prelude **standalone**: load → translate → cook → `type_check`
on the prelude *alone*, with **no `eliminate`/prune step** — standalone,
every prelude binding is a root, so the whole prelude is checked (this
also means the prelude is checked *completely*, unlike the
merged-then-pruned path which only ever checks the subset a given user
file reaches). The resulting binding types and the checker's alias map
become the `PreludeSummary`.

## B7.4 The cache

- The prelude is **embedded in the binary** — `src/driver/resources.rs`
  does `include_bytes!("../../lib/prelude.eu")` and registers it as the
  `"prelude"` resource, loaded by a hardcoded `Locator::Resource`. It is
  fixed for a given build: there is **no** mechanism to substitute a
  different base prelude (the only related control is `-Q` /
  `--no-prelude`, which *suppresses* the prelude — a check run that way
  has an empty seed and B7 is simply inert). So the cache key is
  effectively constant per build, and B7 is in practice "compute the
  summary once per process".
- Key the cache on the **hash of the prelude source** even so — it
  costs nothing, makes the cache self-invalidating should the embedded
  prelude change between builds, and is the right key if the summary is
  later persisted across binary versions.
- **In-memory first**: compute the summary on the first check of a
  process and reuse it for every subsequent file — the dominant LSP and
  CLI win.
- **Then optionally persistent / build-time**: because the prelude
  ships with the binary, its summary could be computed once at build
  time (alongside `build-meta.yaml`) and embedded, or persisted
  content-addressed on disk. Either is a follow-on; the in-memory cache
  delivers the responsiveness.

## B7.5 Seeding a user-file check

The `Checker` gains the ability to be **seeded** with a `PreludeSummary`:
its `bindings` populate the outermost scope frame, its `aliases`
pre-populate the alias map. A user file is then checked **standalone** —
its own translated/cooked core, *without* the prelude merged in.
References to prelude names are unbound within the user unit and resolve
against the seed by name, via the existing `lookup_bound` →
`lookup_name` fallback ("for references to bindings outside the
checker's root expression"). A user file that shadows a prelude name or
redefines a prelude alias does so in its own scope — innermost wins,
exactly as in the merged check.

## B7.6 The pipeline change

`type_check_path_full` changes from

> load(prelude + file) → translate → **merge** → cook → eliminate →
> check-once

to

> *(once, cached)* load/translate/cook(prelude) → check(prelude) → summary
> load/translate/cook(file) → check(file, seed = summary)

The change is **bounded** — two checks (prelude once; the file
standalone), not a general module DAG. It is the modest restructure
prelude-only caching needs, no more.

## B7.7 Behaviour preservation

Seeding must produce **the same warnings** for a user file as the
current merged-then-pruned check. This is the load-bearing correctness
property and the primary regression test (§Test plan). It holds because
prelude references resolve to the same types either way (the prelude is
unaffected by the user file); any discrepancy — including any surprise
from unit-metadata merging — surfaces here as a failing test.

## B7.8 LSP

With the prelude summary cached, the LSP re-check of an edited file
drops from "re-walk prelude + re-check file" to "re-check file against
the cached seed". For a small file that is the difference between a
visible pause and an imperceptible one — the responsiveness B7 exists
for.

## B7.9 Out of scope

- **General per-user-module summaries.** A user unit's export types are
  not guaranteed stable under positional merge-override; caching them
  soundly would need the composition model pinned down and offers a
  small payoff. Not done. If multi-file user projects later need it,
  it is a fresh, larger bead gated on that analysis.
- **Explicitly imported preludes.** B7's seed is the *implicit base*
  prelude — the unit merged in automatically at the bottom of the
  stack, its bindings at top level. A unit that instead brings the
  prelude in via an explicit `import:` — in particular a namespaced
  import such as `import: "pre=prelude.eu"`, exposing it as `pre.map`
  etc. — is a *different construct*: an ordinary imported unit, checked
  as one, **not served from the cache**. This is a deliberate scope
  choice, not an impossibility: because the cache is content-addressed
  by source hash, a hash-matching import *could* be served the same
  summary re-shaped into a namespaced block type — but that needs the
  cache lookup to fire on imports and the summary to be re-namespaced,
  for a rare case, so B7 does not do it. Documentation of the prelude
  cache should state plainly that an explicitly-imported prelude is not
  pre-checked.
- **Whole-program inference.** Unannotated prelude exports keep
  whatever the checker infers (often `any`); B7 caches that, it does
  not improve it (H9).

## Test plan

- **Behaviour preservation** (primary): for a corpus of user files,
  the seeded standalone check produces *exactly* the warnings the
  current merged check does.
- The prelude is checked **once** per process, not once per file.
- A user file shadowing a prelude binding / redefining a prelude alias
  behaves identically under seeding.
- Editing the prelude source invalidates the cache (hash changes);
  editing a user file does not.
- LSP re-check latency measurably drops.
- `eu check lib/prelude.eu` clean; full `cargo test` green; clippy clean.

## File-by-file change summary

| File | Change |
|------|--------|
| `src/driver/check.rs`, `src/driver/source.rs` | standalone prelude check producing a `PreludeSummary`; the hash-keyed in-memory cache; the standalone seeded user-file check path |
| `src/core/typecheck/check.rs` | `Checker` accepts a `PreludeSummary` seed (outer-scope bindings + aliases) |
| `src/driver/lsp/` | re-check the edited file against the cached seed |
| `tests/` | behaviour-preservation corpus; cache-reuse and invalidation tests |
