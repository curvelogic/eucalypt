# Pre-decoded IR — ticks-first measurement (eu-2sa6.13)

- **Date:** 2026-07-13
- **Commit:** 39f9731c (branch `feat/predecode-branch-pool`)
- **Host:** Darwin-25.5.0-arm64
- **Toolchain:** rustc 1.97.0 (2d8144b78 2026-07-07)
- **Prelude config:** blob (fresh `lib/prelude.blob`, 269176 bytes)
- **Protocol:** `docs/superpowers/engine-ab/PROTOCOL.md`
- **Status:** TICKS-FIRST only. Wall median-of-N runs are **deferred** to a quiet
  window (the measurement box was under concurrent build/review load); no wall
  ratio is quoted here, per PROTOCOL §2. The `results.jsonl` rows will be
  appended once the wall medians exist, using the deterministic figures below
  unchanged.

> **Correction (2026-07-13, post-review).** The first version of this report
> measured with `eu run <file>` and **no `-t <target>`** (and, for `021`, no
> `--allow-io`). That renders the *whole document*, which for some benches
> evaluates extra work the bench target does not: `020_lookup_curve` defines
> `total40`/`total250` as top-level bindings that the `bench-lookup-curve`
> target *also* references, so the whole-document render evaluated them twice
> (Wicket's "2× + 682" signature — thank you); `015` rendered extra top-level
> blocks (+35 M ticks); and `021_io_loop` ran with no `--allow-io`, so its io
> loop never executed (1,518 ticks instead of 3.97 M). Every figure below is
> now measured the way the canonical suite / `cargo xtask engine-ab` measures
> (`base_cmd`, `xtask/src/engine_ab.rs`): `EU_HEAPSYN=1 eu -S --heap-limit-mib
> 12288 [--allow-io] -t <target> <file>`, parsing the `-S` `Ticks`/`Allocs`/
> `Collections` lines. Rows corrected: **015, 020, 021** (others moved by
> ≤~1,200 ticks — the small whole-document render overhead). The step-neutral
> conclusion in §2 is unchanged.

## 1. Deterministic reference (HeapSyn `-S -t <target>`) — the load-independent layer

`--heap-limit-mib 12288` (0 collections, matching production). These are the
`hs_ticks`/`hs_allocs`/`gc` columns of the eventual ledger rows. **Confidence:
measured-verified** (deterministic; `020` independently reproduced by review).

| Bench | Class | Target | hs_ticks | hs_allocs | gc |
|---|---|---|---|---|---|
| 015_block_merge | D | bench-block-merge | 98,788,746 | 472,734 | 0 |
| 016_import_export_yaml | I | bench-import-yaml | 59,894,595 | 1,922,737 | 0 |
| 017_import_export_toml | I | bench-import-toml | 59,721,271 | 1,286,587 | 0 |
| 018_string_scale | G | bench-string-scale | 76,814,993 | 364,176 | 0 |
| 019_list_scale | H | bench-list-scale | 55,803,488 | 156,182 | 0 |
| 020_lookup_curve | E | bench-lookup-curve | 1,721,105 | 5,553,215 | 0 |
| 021_io_loop | L | bench-io-loop | 3,971,300 | 545,675 | 0 |
| 022_hof_fold | C | bench-hof-fold | 52,225,448 | 180,163 | 0 |

## 2. Engine dispatch-step counts (bytecode `-S -t <target>` Ticks) — byte vs pre-decode

The bytecode engine's own `Ticks` counts dispatch steps; deterministic, so it
characterises whether the pre-decoded representation changes *how many* steps
run (vs. the per-step cost). Compute benches only (io/import excluded).
**Confidence: measured-verified** (`020` independently reproduced by review:
byte 164,630,267 / predecode 164,589,685).

| Bench | byte steps | predecode steps | Δ |
|---|---|---|---|
| 015_block_merge | 98,669,543 | 98,633,467 | −0.04% |
| 018_string_scale | 76,730,983 | 76,688,977 | −0.05% |
| 019_list_scale | 55,719,473 | 55,707,469 | −0.02% |
| 020_lookup_curve | 164,630,267 | 164,589,685 | −0.02% |
| 022_hof_fold | 52,125,436 | 52,105,432 | −0.04% |

**Reading this honestly:** on the canonical suite the pre-decoded engine is
**step-count-neutral** — it runs essentially the same number of dispatch steps
as the byte engine. This is expected and correct:

- Operand pre-decode (the design's headline) reduces the *per-step CPU cost*
  (typed field reads instead of byte re-decode + `Op::from_u8` + bounds checks),
  **not the number of steps**. That is a wall/CPU-time effect — the **deferred**
  measurement — and cannot show up in a step count.
- BV2 Ann-elimination *does* remove steps, but only where `Op::Ann` nodes sit in
  the hot path. The canonical compute benches have very few (their hot loops are
  prelude combinators over data), so the step reduction here is ~0.0–0.05%. A
  heavily-annotated hot path shows more: `fib(30)` drops 722,435→623,927 bc
  steps (−13.6%) — a workload-dependent figure, not representative of this suite.

So the pre-decoded engine's value on this suite is entirely in per-step wall
cost, which this report deliberately does **not** quote (noisy box). The
`022_hof_fold`/`019_list_scale` measured-single wall spot-check taken during
implementation (ratio 0.982 / 0.991, predecode ≤ byte) is **context only**, not
a protocol figure.

## 3. What lands in `results.jsonl`, and when

Nothing is appended yet. When the quiet-window wall runs complete (via `cargo
xtask engine-ab` on an unloaded machine, interleaved bc/hs, median-of-N per
PROTOCOL §3), one row per bench will be appended carrying the §1 `hs_ticks`/
`hs_allocs`/`gc` above verbatim plus the fresh `bc_wall_med`/`hs_wall_med`/
`ratio`. Until then no wall-derived number may gate a release or enter
CHANGELOG/ROADMAP (PROTOCOL §5).
