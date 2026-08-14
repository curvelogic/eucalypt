# Assessment: BV5-cache — content-hash whole-program unit cache (eu-lb0r, spec PR #953)

> Owner-accepted 2026-08-11. Produced for the ROADMAP §9 re-dating decision.

Date: 2026-08-11 · Assessor: Fable assessment agent · For: owner 0.15 scope decision
Basis: bd eu-lb0r / eu-y7le / eu-2sa6.18; spec `docs/superpowers/specs/2026-07-03-bv5-unit-cache-design.md`
(merged as **PR** #953 — there is no *issue* #953); ROADMAP §7/§9; code inspection of
`src/driver/eval.rs`, `src/driver/prepare.rs`, `src/eval/stg/blob.rs`, `src/eval/stg/wire_format.rs`;
measurements on the furnace-oufc release build (post-collapse, built 2026-08-06).

## 1. What it is & spec currency

Cache the **whole merged user program's compiled artifact** on disk, keyed by
`SHA-256(inputs ‖ compile-affecting options ‖ compiler identity)`, injected at the same
post-STG-compile seam the prelude blob uses (`eval.rs` ~line 508–560). A hit skips
parse → desugar → merge → cook → inline → DCE → demand → STG-compile. Fail-closed envelope
(format-version + memoised compiler binary self-hash + key echo); default-on per the July
owner decision; `--no-cache` escape hatch; XDG cache dir with LRU eviction.

**Currency: the spec is materially stale in four places and improved-by-events in one.**

1. **Dual-engine machinery is dead.** §4's engine-keyed entries, the ArenaStgSyn-vs-BytecodeProgram
   dual-form question, and every `EU_HEAPSYN` reference are obsolete since the Phase-4 collapse
   (PR #1121, merged 2026-08-06). This *simplifies* the design: one engine, one artifact
   (the encoded `BytecodeProgram` chunk the loader appends to the prelude image, or the
   post-STG arena). A net simplification, but §4 needs rewriting.
2. **Typecheck-always-on postdates the spec.** Type checking now runs unconditionally on every
   invocation (warnings to stderr). A cache hit that skips parse→desugar also skips warning
   emission — the spec never mentions the type checker. Two options: (a) cache the warnings
   themselves (deterministic given the keyed inputs, so cacheable — but `--strict` exit
   behaviour must replay too); (b) store desugared unit cores in the entry and re-run the
   merged check on hit, the pattern the blob already uses (`desugared_unit_cores` +
   `run_type_checker_from_blob_core`). Option (b) costs ~5–25 ms (measured `eu check` wall
   ≈ 30–48 ms including the ~21 ms process floor), eating much of the small-program win.
   This is a real, undesigned correctness/UX surface.
3. **Wire format churn.** `BYTECODE_WIRE_FORMAT_VERSION` is at **8** (bumped 2026-07-27),
   eight versions in roughly a month. Every bump wholesale-invalidates the cache — harmless
   (fail-closed) but value-eroding while BV2 side tables / BV3 register frames / CG unboxing
   are still landing. The cache's hit economics improve once the format settles.
4. **"Dispatch-ready" no longer strictly holds.** The design decisions are still sound at the
   architecture level (key composition, envelope, §8 structural completeness guard, §12 test
   matrix all survive), but §4 (artifact/engines), the §6 key's engine selector component,
   and the missing typecheck story mean a **spec refresh pass is required before build** —
   half a day, not a redesign. The bead itself already says "DESIGN PASS REQUIRED".
5. **Improved by events — `__args`/`__io`.** The spec keys the `__args`/`__io` pseudo-inputs
   into the hash (so changed CLI args ⇒ miss). On today's blob path those are **per-invocation
   global-slot overrides** compiled fresh at runtime (`eval.rs:444–485`), *outside* the merged
   user program — so the cached artifact can be args/env/seed-independent, widening cache
   reach beyond what the spec assumed. Worth capturing in the refresh.

**eu-n8c5e does not taint the cache.** The blob's un-inlined-prelude defect comes from the
xtask *per-binding peel* compiler skipping inline/demand/reflatten. BV5-cache captures the
user program compiled by the **full normal pipeline** at the normal seam — it inherits none
of that. (If anything, the cache stores a *better-optimised* artifact than the blob stores
for the prelude.) Conversely the cache does nothing to fix n8c5e's residual ~31–63 % blob tax.

## 2. Measured ceiling — where cold-run time actually goes

All figures **measured-single** (hyperfine mean of 5–10 runs, warmed; loadavg ≈ 3–4 during the
session — an active multi-agent machine, so treat as indicative, not protocol-grade). Binary:
`/Users/greg/dev/curvelogic/eucalypt-worktrees/furnace-oufc/target/release/eu` (v0.0.0.dev,
post-collapse master of 2026-08-06). All runs under `timeout` with `--heap-limit-mib`.

| Program | Wall (mean) | Pipeline compile* | Eval + render | Cache ceiling |
|---|---|---|---|---|
| `eu --version` / `-e 1` (process floor) | 20–21 ms | ~0.5 ms | ~0 | — |
| `tests/harness/001_ski.eu` (trivial) | 21.7 ms | ~3.3 ms | ~0.05 ms | **≤ 15 %** |
| `build.eu` (real small script) | 26.6 ms | ~4.9 ms | ~2.0 ms | **~18 %** |
| `tests/harness/133_lens_import.eu` (imports) | 41.4 ms | ~13.4 ms | ~0.6 ms | **~32 %** |
| `bench/016_import_export_yaml.eu` (265 KB YAML import, compute-heavy) | 633 ms | ~140 ms | ~239 ms eval (+ ~250 ms unattributed: source-load/typecheck/heap teardown, outside `-S`) | **~22 %** |

\* compile = `-S` pipeline total minus bytecode-eval/render. Note the `-S` table does **not**
time the type checker (it runs in `bin/eu.rs` after `prepare()`), so "compile" slightly
understates what a hit could skip — but `eu check` timings bound typecheck at ≲ 25 ms even
on 016.

**Reading:** the ceiling is **15–32 % of wall on runs that are already 20–650 ms**. In absolute
terms a hit saves **3–14 ms on typical scripts, ~140 ms on the heaviest import-bench in the
repo**. The ~20 ms process floor (dyld + init + blob decode) and evaluation are untouched.
For calibration, the shipped prelude blob — the same mechanism applied to the one unit big
enough to matter — saves ~63 ms/run (source-prelude 83 ms vs blob 20 ms, measured-single).
User programs are one to two orders of magnitude smaller than the prelude; the measurements
bear that out.

**Reach compounds the modest ceiling.** A whole-program cache hits only when the *entire*
input set (including imported data files) is byte-identical. The edit-run development loop —
the loop that actually hurts — misses on every run by construction. Real hit populations:
CI re-runs, unchanged-template re-renders in pipelines, repeated scripted invocations over
static data. Those exist but are not today's pain reports (the perf beads are all about
eval-side costs: env-walk depth, blob inline tax, HOF quadratics).

## 3. Relation to CU1/CU2/eu-y7le — complement or duplicate?

**Complement by explicit design, not duplicate — but the dependency is one-way and weak.**

- **CU2 (eu-y7le, per-unit incremental, 0.15+)** is the actual "edit one file → fast recompile"
  payoff. It `depends on` eu-lb0r in beads, and both spec §14 and ROADMAP §7 say the cache's
  format/version/build-id envelope "carries over". CU2 also needs **CU1 (eu-nzps, defer-merge,
  currently frozen)** — pillar-scale compiler restructuring that BV5-cache does nothing to advance.
- **What CU2 actually reuses** from eu-lb0r is the *envelope and invalidation discipline*
  (~30 % of the work: format-version stamp, compiler self-hash, fail-closed reject, key
  hygiene, completeness guard test) — not the whole-program key computation, the cache-hit
  driver plumbing, or the eviction/CLI surface (~70 %, specific to the whole-program
  deliverable). The envelope could equally be built *as CU2's first milestone* when CU1 lands.
- **eu-2sa6.18** (generalise blob-form per-binding compilation to arbitrary units) is a
  different artefact at a different granularity — no prerequisite relationship in either
  direction (its own 2026-07-25 note confirms it's off every current critical path). One
  coordination point: the ratified naming/staging convention (CoreStage/StagedCore typed-stage
  wrappers, "core = faithful stage snapshot") must govern whatever a cache entry stores.
- **W19 (`eu watch`/REPL, 0.15+)** is the named consumer of cached startup — and what it wants
  is CU2, not the whole-program cache (watch triggers on *change*, which is a guaranteed miss).

So: no duplication, but the standalone deliverable's unique content (whole-program key + hit
plumbing + eviction) serves only the narrow identical-inputs population, while its reusable
content (envelope) is not time-critical until CU1/CU2 are real.

## 4. Effort & risk

**Effort: ~1–2 focused weeks** including the spec refresh, `eu cache` subcommand, and the §12
correctness matrix. Genuinely dispatch-friendly parts: serialisation machinery exists
(postcard, `BytecodeProgram: Serialize`), the injection seam exists, the fail-closed pattern
is proven in the blob. The hard 30 %: the §8 structural options-completeness guard (a
reflection-style CI test over `EucalyptOptions`/`StgSettings`), the typecheck-on-hit design
(§1.2 above), and the differential test matrix.

**Risks:**
- **Default-on invalidation correctness is the whole game.** A single forgotten
  compile-affecting option ⇒ silently wrong output for every user. The §8 guard mitigates
  structurally, but this is the highest-consequence failure and it is a *permanent* liability
  every future flag must respect. (Also process: cache/wire-format work triggers the
  recorded-review-before-merge rule.)
- **Wire churn erodes value now** (v8 and climbing; BV2/BV3/CG still queued). Fail-closed, so
  never wrong — just frequently cold.
- **Typecheck warnings on hit** — undesigned; option (b) (re-check from cached desugared
  cores) is safest but surrenders 5–25 ms of the ≤ 15–30 ms win on small programs.
- **Operational surface** (cache dir, eviction, concurrent writers, Windows paths) is
  boring-but-real maintenance for a feature with modest reach.

## 5. Recommendation

**DEFER — out of 0.15; fold the reusable part into the CU track.**

- The measured ceiling (15–32 % of 20–650 ms wall; 3–14 ms absolute on typical scripts) times
  the narrow hit population (byte-identical whole input set — never the edit-run loop) does
  not justify a correctness-critical, default-on, permanently-maintained feature in 0.15,
  which is themed "single engine, CI integrity, diagnostics completion" and already carries
  eu-lb0r only as an inherited parent link.
- Re-scope eu-lb0r to "**serialisation envelope + invalidation discipline** (format-version
  stamp, compiler self-hash, fail-closed reject, options-completeness guard)" and schedule it
  as **milestone 1 of CU2 (eu-y7le), gated with CU1 (eu-nzps), 0.16+** — where the same
  machinery serves the cache population that actually matters (edit one file → fast
  recompile, feeding W19 watch/REPL and W18 modules). Update ROADMAP §9's BV5-cache row
  (currently still "0.13; dispatch-ready", both wrong) accordingly.
- Precondition for whenever it builds: **spec refresh** (post-collapse single-engine §4,
  typecheck-on-hit, `__args`/`__io` override-based reach widening, wire-format v8) — half a
  day, and the bead already mandates a design pass.
- Cheap interim alternative if CI cold-start cost ever registers as real pain: revisit then
  with a hit-rate measurement from an actual CI trace, not a build-first commitment.

No spike needed — the value question was the spike, and it's answered above.
