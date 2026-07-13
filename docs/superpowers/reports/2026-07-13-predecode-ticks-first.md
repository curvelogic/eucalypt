# Pre-decoded IR — ticks-first measurement (eu-2sa6.13)

- **Date:** 2026-07-13
- **Commit:** 39f9731c (branch `feat/predecode-branch-pool`)
- **Host:** Darwin-25.5.0-arm64
- **Toolchain:** rustc 1.97.0 (2d8144b78 2026-07-07)
- **Prelude config:** blob (fresh `lib/prelude.blob`, 269176 bytes)
- **Protocol:** `docs/superpowers/engine-ab/PROTOCOL.md`
- **Status:** TICKS-FIRST only. Wall median-of-N runs are **deferred** to a quiet
  window (the measurement box was under concurrent build/review load); no wall
  ratio is quoted here, per PROTOCOL §2 ("if the machine is loaded, say so").
  The `results.jsonl` rows will be appended once the wall medians exist, using
  the deterministic figures below unchanged.

## 1. Deterministic reference (HeapSyn `-S`) — the load-independent layer

Read from `EU_HEAPSYN=1 eu -S`, `--heap-limit-mib 12288` (0 collections, matching
production). These are the `hs_ticks`/`hs_allocs`/`gc` columns of the eventual
ledger rows. **Confidence: measured-verified** (deterministic).

| Bench | Class | hs_ticks | hs_allocs | gc |
|---|---|---|---|---|
| 015_block_merge | D | 134,179,383 | 645,066 | 0 |
| 016_import_export_yaml | I | 59,895,722 | 1,922,943 | 0 |
| 017_import_export_toml | I | 59,722,384 | 1,286,727 | 0 |
| 018_string_scale | G | 76,815,495 | 364,229 | 0 |
| 019_list_scale | H | 55,803,922 | 156,222 | 0 |
| 020_lookup_curve | E | 3,442,892 | 11,107,135 | 0 |
| 021_io_loop | L | 1,518 | 552 | 0 |
| 022_hof_fold | C | 52,225,882 | 180,203 | 0 |

## 2. Engine dispatch-step counts (bytecode `machine_ticks`) — byte vs pre-decode

The bytecode engine's own `machine_ticks` counts dispatch steps. This is
deterministic and characterises whether the pre-decoded representation changes
*how many* steps run (as opposed to the per-step cost). **Confidence:
measured-verified** (deterministic).

| Bench | byte steps | predecode steps | Δ |
|---|---|---|---|
| 015_block_merge | 134,020,092 | 133,968,976 | −0.0% |
| 018_string_scale | 76,731,485 | 76,689,477 | −0.1% |
| 019_list_scale | 55,719,907 | 55,707,901 | −0.0% |
| 020_lookup_curve | 329,261,216 | 329,180,052 | −0.0% |
| 022_hof_fold | 52,125,870 | 52,105,864 | −0.0% |

**Reading this honestly:** on the canonical suite the pre-decoded engine is
**step-count-neutral** — it runs essentially the same number of dispatch steps
as the byte engine. This is expected and correct:

- Operand pre-decode (the design's headline) reduces the *per-step CPU cost*
  (typed field reads instead of byte re-decode + `Op::from_u8` + bounds checks),
  **not the number of steps**. That is a wall/CPU-time effect — the **deferred**
  measurement — and cannot show up in a step count.
- BV2 Ann-elimination *does* remove steps, but only where `Op::Ann` nodes sit in
  the hot path. The canonical compute benches have very few (their hot loops are
  prelude combinators over data), so the step reduction here is ~0.0–0.1%. A
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
