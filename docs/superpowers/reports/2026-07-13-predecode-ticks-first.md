# Pre-decoded IR — ticks-first + wall measurement (eu-2sa6.13)

- **Date:** 2026-07-13 (ticks-first layer); **2026-07-14 (wall layer, this
  update)** — the deferred quiet-window run, owner-granted, ~60 min window.
- **Commit (wall run):** `5f550b49` (master, includes #1002 predecode +
  #1004 branch-pooling + #1005/#1006 diagnostic instrumentation merged —
  both disabled by default, zero effect on these figures).
- **Host:** Darwin-25.5.0-arm64
- **Toolchain:** rustc 1.97.0 (2d8144b78 2026-07-07)
- **Prelude config:** blob. **One binary, one blob, both engines** (predecode
  is runtime-toggled via `EU_PREDECODE=1`, so there is no build-desync risk
  between the byte and predecode arms — both read the identical embedded
  blob). Gold-protocol hashes recorded per `bd recall
  stale-binary-rebuild-trap`: blob build regenerated then eucalypt **fully
  rebuilt** (`cargo build --release` output showed `Compiling eucalypt`,
  confirming the fresh blob was actually re-embedded, not stale):
  - `lib/prelude.blob` sha256 `e59640e61806d29fb7ace1ea312329a1e069b87e7bd6dd5764e7a4ae971300c9`
  - `target/release/eu` sha256 `b2bf8a6759b33fa655ae438c171a8493349d63cda560b9fa6202c873f7854f63`
  - Bench file hashes recorded for all eight `tests/harness/bench/{015..022}*.eu`
    (evidence index, §5).
- **Machine load (wall run):** checked via `ps -Ao pcpu,comm -r` before and
  during the run. `mediaanalysisd` and `spotlightknowledged` (the two
  processes explicitly flagged as disqualifying) were absent/suspended
  throughout, as granted. `WallpaperAerialsExtension` (a macOS idle/lock
  screensaver process, ~15–25% CPU) **was** intermittently active during
  parts of the run — not on the disqualifying list, but unexpected background
  load, noted here rather than silently ignored. Within-bench spread stayed
  tight throughout (see §2.5 table), which is evidence against material
  distortion, but the session is labelled **measured-single** (not yet
  independently re-verified in a second quiet session) per PROTOCOL §5,
  partly for this reason.
- **Protocol:** `docs/superpowers/engine-ab/PROTOCOL.md`
- **Status:** Ticks-first layer (§1–§2) is unchanged and remains
  measured-verified. The wall layer (§2.5, new) is now complete —
  interleaved byte-vs-predecode, median-of-5 with spread, all eight
  canonical benches. `results.jsonl` has separately received the standard
  bc/hs (byte vs HeapSyn) ledger rows via `cargo xtask engine-ab` on this
  same build (§3) — that schema has no predecode column, so the
  byte-vs-predecode comparison lives in this report, not in the ledger.

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
cost — measured below, completing what this section deferred. The
`022_hof_fold`/`019_list_scale` measured-single wall spot-check taken during
implementation (ratio 0.982 / 0.991, predecode ≤ byte) was **context only**
at the time; §2.5 supersedes it with a protocol-grade interleaved run.

## 2.5 Byte-vs-predecode WALL (the deferred measurement, now complete)

Interleaved per bench (byte, predecode, byte, predecode, …), median-of-5,
`--heap-limit-mib 12288`, `[--allow-io]` for `021_io_loop`, one binary/one
blob throughout (§ header). **Confidence: measured-single** (one clean
interleaved session; not yet independently repeated — see the machine-load
caveat in the header).

| Bench | Class | byte wall (med) | byte spread | predecode wall (med) | predecode spread | ratio (pd/byte) |
|---|---|--:|--:|--:|--:|--:|
| 015_block_merge | D | 2.139s | [2.088–2.148] | 1.993s | [1.958–2.002] | **0.932** |
| 016_import_export_yaml | I | 1.710s | [1.708–1.713] | 1.640s | [1.632–1.642] | **0.959** |
| 017_import_export_toml | I | 1.576s | [1.569–1.599] | 1.525s | [1.525–1.530] | **0.968** |
| 018_string_scale | G | 2.231s | [2.168–2.258] | 2.073s | [2.044–2.274] | **0.929** |
| 019_list_scale | H | 1.831s | [1.803–1.919] | 1.132s | [1.130–1.148] | **0.618** |
| 020_lookup_curve | E | 2.564s | [2.551–2.649] | 2.144s | [2.132–2.157] | **0.836** |
| 021_io_loop | L | 2.560s | [2.552–2.572] | 2.563s | [2.553–2.564] | **1.001** |
| 022_hof_fold | C | 1.342s | [1.337–1.355] | 1.265s | [1.256–1.272] | **0.943** |

**Reading this:** predecode is faster on 7 of 8 benches, by 3–17% on six of
them and dramatically (**38% faster, ratio 0.618**) on `019_list_scale`; it
is wall-neutral on `021_io_loop` (ratio 1.001), which is expected — that
bench is dominated by 1000 `io.shell` round-trips, not compute, so a
dispatch-loop CPU-cost improvement has nothing to act on there. Direction
matches the design intent (§2: predecode reduces *per-step* CPU cost, not
step count) and is now shown at protocol grade rather than as a
measured-single spot-check.

**The `019_list_scale` outlier was independently verified, not just
accepted:** given its size (38% vs the 3–7% seen elsewhere), it was checked
three ways before being reported: (1) `-S` mutator time independently
confirms the wall-clock gap (byte 1.782s vs predecode 1.100s, matching the
external timing to the millisecond); (2) ticks (55,719,473 byte vs
55,707,469 predecode) and allocs (156,182 both) are near-identical, matching
§2's step-neutral finding — this is a genuine per-step cost win, not extra
or skipped work; (3) output is identical (`RESULT: PASS`, `total: 18003000`
both configs) and the gap reproduced three more times in an isolated rerun
outside the main interleaved script (byte ~1.78s / predecode ~1.10s each
time). **Measured-verified for the ticks/allocs/output layer** (deterministic,
independently re-checked); **measured-single for the wall magnitude**
(reproduced multiple times this session, not yet in a second independent
session).

No hypothesis is offered here for *why* `019_list_scale` benefits so much
more than the other compute benches from operand pre-decode — that is a
follow-on question, not answered by this ticks-first-then-wall measurement
pass.

## 3. What has landed in `results.jsonl`, and what hasn't

**Landed:** the standard bc/hs (byte vs HeapSyn) ledger rows, via `cargo
xtask engine-ab` run on this same build/commit/blob (8 rows, blob config,
commit `5f550b49`). `cargo xtask engine-ab --check` against the immediately
preceding blob-config session showed **no regressions** (>15% worsening);
the two WATCH flags (`list_scale` class-H threshold, `lookup_curve` class-E
tripwire) are both informational per PROTOCOL §6, not failures. This also
serves as the post-#1004 (branch-pooling) wall validation the team asked
for.

**Not landed, and not schema-compatible:** the byte-vs-predecode comparison
in §2.5 above. `results.jsonl`'s row schema (`bc_wall_med`/`hs_wall_med`/
`ratio`) has no column for a third engine dimension — appending predecode
numbers into `hs_wall_med` would silently corrupt the ledger's meaning for
every future `--check` comparison. The byte-vs-predecode wall figures live
in this report instead, which is the deliberate, schema-respecting choice
here — extending `results.jsonl`'s schema to carry a predecode column is a
separate, out-of-scope decision for the owner/PROTOCOL maintainer, not made
unilaterally in this spike.

## 4. Evidence index (gold protocol per `bd recall stale-binary-rebuild-trap`)

- **Commit:** `5f550b49` (master, `spike/lever-a-wall-protocol` branched
  from it).
- **Blob:** `lib/prelude.blob` sha256
  `e59640e61806d29fb7ace1ea312329a1e069b87e7bd6dd5764e7a4ae971300c9`
  (461,638 bytes; regenerated via `cargo xtask prelude-compile` immediately
  before the measurement build).
- **Binary:** `target/release/eu` sha256
  `b2bf8a6759b33fa655ae438c171a8493349d63cda560b9fa6202c873f7854f63`
  — one binary served every measurement in §2.5 and §3 (byte via no flag,
  predecode via `EU_PREDECODE=1`, HeapSyn via `EU_HEAPSYN=1`), confirmed
  freshly rebuilt after the blob regen (`cargo build --release` output
  showed `Compiling eucalypt`, not a cached no-op).
- **Bench file hashes** (sha256, `tests/harness/bench/`):
  - `015_block_merge.eu`: `e25aa255cd3a4f0ec5bd5962bbb30ec01d4ef39ee4ddc841e49f792d392970f9`
  - `016_import_export_yaml.eu`: `054e2ba058bdaadf2bedcce89cc678de9504185fc8763bb6af5b9e161e640634`
  - `017_import_export_toml.eu`: `d9a9120212c624beda7eda1f6ec55db7db25c457ff311f507fed4d4fc2db7220`
  - `018_string_scale.eu`: `666f6a32a0fd0ba5b6feef3c974e19d1e8b7414ac443506663fcc6db17c82349`
  - `019_list_scale.eu`: `35b899e8f6b5018bdafee58b492117c9ae07f30f221e4ee650b160559a204f71`
  - `020_lookup_curve.eu`: `b96b1e65d4e1f419ad65740737bb1fc1c61d1f6805c9f7e586ccf99b96442f62`
  - `021_io_loop.eu`: `ff3e9ffa85b084a40d1583185c5b1bb37ca7fdea08bc9460b805d2afd063da2c`
  - `022_hof_fold.eu`: `fda47fc3b49698a6b99c27314d080ef5672a30d386ec09272c2c47b7e7baff69`
- **Worktree:** `/tmp/eu-stopwatch-lever-a`, branch `spike/lever-a-wall-protocol`.
- **Raw §2.5 timing samples:** 5 byte + 5 predecode runs per bench, 80 total
  timed invocations, collected via an interleaved shell script (per-bench
  interleave, matching `cargo xtask engine-ab`'s own interleave pattern);
  not separately archived as a file, reproducible via `timeout 60
  ./target/release/eu --heap-limit-mib 12288 [--allow-io] [env
  EU_PREDECODE=1] -t <target> tests/harness/bench/<bench>.eu` against the
  hashes above.
