# Engine A/B perf study: is bytecode at parity with HeapSyn everywhere? (eu-7oshh)

**Date:** 2026-08-03. **Commit:** `717a7127` (branch `perf/eu-7oshh-engine-ab`).
**Toolchain:** rustc 1.97.1, Darwin 25.5.0 arm64. **Author:** Stopwatch.
**Status:** measurement + tooling fix only. HeapSyn, `GcScannable` and the
engine-select default are untouched.

**Recommendation: (a) READY.** Bytecode is at parity or better than HeapSyn on
every program this bead is gated on, including the four that failed the last
study (day03, day08, day09, fib). eu-oufc (Phase 4 collapse) can go to the
owner for authorisation, subject to the caveats in §5.

---

## 1. Tooling prerequisites

**eu-lhai (fixed, this branch).** `--check` compared each bench's last two
ledger rows regardless of `prelude_config`, so a blob row could be read
against a source row and synthesise a false REGRESSED/WATCH verdict. Fixed by
grouping rows into lineages keyed by `(bench, prelude_config, dispatch)` and
comparing only within a lineage. 5 new unit tests in `xtask/src/engine_ab.rs`;
fault-injection verified (reverted the lineage key to bench-only, confirmed
all 5 fail, restored, confirmed all pass).

**eu-hxu6 (fixed, this branch).** The ledger had no way to express which
bytecode dispatch strategy was measured (pre-decoded, the default since
eu-vcr8 Phase 2, vs byte-dispatch, `EU_PREDECODE=0`, soaking under eu-1hcw).
Added a `dispatch` ledger field and a `cargo xtask engine-ab --dispatch
predecoded|byte` flag. Absent on legacy rows, defaulted to `"predecoded"` —
the only path measured before this field existed, so historical comparisons
are unaffected. This measurement session used **predecoded** throughout
(the shipped default); byte-dispatch is out of scope for eu-7oshh (that's
eu-1hcw's question, on a different axis).

**eu-n8c5e (confirmed, not fixed — out of scope).** `xtask/src/main.rs:180-186`
explicitly skips `inline()` when compiling the prelude blob ("NOTE: We skip
`inline()` deliberately: the inline pass aggressively folds internal Let
bindings into their single call site..."). This is a real, already-tracked
confound: blob mode runs the prelude less optimised than source mode. §4 shows
its effect directly — source-mode `import_export_yaml`/`toml`/`hof_fold` beat
blob-mode on the *same* engine by ~2-7%. eu-n8c5e's own prior investigation
(2026-07-24/25, PR #1065) already found this is a bounded **constant tax**
(31-63% on 016/017-shaped work), not the complexity-class bug (that was
eu-e3c3i, since fixed) — consistent with what this session measured. It biases
blob mode's bc/hs ratio slightly upward on combinator-heavy code but does not
change this bead's verdict.

## 2. Machine conditions (protocol §2 quiet-machine rule)

Load average was **5-18** throughout the session (protocol requires <1),
driven by unrelated concurrent activity on the machine (another project's
`rustc` build, an unrelated `eu` diagnostics process). This is a genuine
protocol violation, not a formality — **wall-clock figures below are labelled
`measured-single`, not `measured-verified`**, even where I ran two interleaved
sessions, per the "if the machine is loaded, say so" rule. Two sessions did
agree closely (ratios within ~2-4% of each other on every bench and all four
named AoC programs), which is corroborating but does not substitute for a
quiet machine. Deterministic figures (ticks, allocs, GC) are
**measured-verified** regardless of load — they were identical across repeat
runs of the same commit, as expected.

All `eu` invocations used `timeout` and `--heap-limit-mib 12288`. One release
binary, one blob, built clean (`cargo clean && cargo build --release`) before
measuring; `EU_HEAPSYN=1` selects HeapSyn on the same binary.

## 3. The four programs that failed the last study (eu-mhjz, 2026-07-03)

Blob mode (the shipped path). Two interleaved sessions, N=7, `--heap-limit-mib
12288`. bc/hs ratio > 1 means bytecode is slower.

| Program | Session 1 ratio | Session 2 ratio | hs ticks | hs allocs | Confidence |
|---|--:|--:|--:|--:|---|
| day03 part-1 | 1.086 | 1.088 | 14,766,749 | 1,448,700 | measured-single (wall); ticks measured-verified |
| day03 part-2 | 1.103 | 1.102 | 32,896,596 | 3,168,140 | measured-single (wall); ticks measured-verified |
| day08 part-1 | 1.045 | 1.026 | 168,854,686 | 18,384,494 | measured-single (wall); ticks measured-verified |
| day09 part-1 | 1.021 | 1.037 | 104,634,899 | 11,581,750 | measured-single (wall); ticks measured-verified |
| fib(30) (bench-naive-fib) | 0.861 | 0.791 | 118,471,655 | 18,847,763 | measured-single (wall); ticks measured-verified |

Last study (eu-mhjz): day03 1.51/1.67, day08 **>52×** (HeapSyn didn't finish in
500s), day09 1.36, fib **2.14×**. Every one of the four is now within, or
better than, the ±15% protocol noise band — day08's extreme HeapSyn GC
pathology is gone (both engines now ~3.3-3.5s), and fib is now bytecode-*faster*
(0.79-0.86). This is the headline result: the gap-closing work cited in the
bead (BV4 superinstructions, `ExecutionError` boxing, pre-decoded dispatch)
did what it set out to do.

## 4. Canonical suite (`tests/harness/bench/{015..022}`), blob vs source

Two interleaved sessions per config, N=7. `dispatch=predecoded` throughout.
Full rows appended to `docs/superpowers/engine-ab/results.jsonl`
(commit `717a7127`, 24 new rows: 16 blob across 2 sessions, 8 source).

| bench (class) | blob ratio (s1/s2) | source ratio (s1) | note |
|---|--:|--:|---|
| 015_block_merge (D) | 1.050 / 1.098 | 1.032 | **below 200ms floor (81-115ms) — excluded from ratio analysis** |
| 016_import_export_yaml (I) | 1.077 / 1.079 | 1.062 | valid; mild bc-slower, consistent with eu-n8c5e blob tax |
| 017_import_export_toml (I) | 1.136 / 1.110 | 1.090 | valid; over class-I threshold (1.05) but inside the 15% noise band |
| 018_string_scale (G) | 1.099 / 1.106 | 1.078 | **below 200ms floor (79-122ms) — excluded** |
| 019_list_scale (H) | 1.005 / 1.008 | 1.007 | **below 200ms floor (50-85ms) — excluded** |
| 020_lookup_curve (E) | 2.709 / 2.714 | 2.161 | documented tripwire exception; magnitudes now near the floor (bc ~0.2s, hs ~0.07-0.11s) — direction preserved, ratio itself is now less load-bearing than when the protocol was written |
| 021_io_loop (L) | 0.982 / 0.964 | 0.983 | valid; parity |
| 022_hof_fold (C) | 1.050 / 1.045 | 0.982 | valid; blob mild bc-slower, source parity — matches eu-n8c5e |

**New tooling finding (report only, not fixed under this bead):** ticks
dropped 5-40× across most benches since the last ledger entry (2026-07-16,
`921f3102`) — e.g. 015's hs ticks 96M→2.6M, 018's 76M→2.8M, 019's 55M→1.5M.
Bench source files are unchanged (`git log` shows one commit ever touching
015/018/019/020); the drop tracks unrelated complexity-class fixes shipped
since (eu-e3c3i, PR #1068, closed 2026-07-25 — foldl-based prelude HOFs
quadratic→linear). Consequence: **015, 018, 019 have fallen below the
protocol's ~200ms floor** and no longer satisfy §6's "every canonical bench is
>1s on the faster engine" design intent; 020 is close to the floor on both
sides. This doesn't change today's verdict (the excluded benches were near
1.0 anyway) but the suite needs re-sizing (larger N) to stay inside the
protocol's validity envelope — worth a follow-up tooling bead, not something
I've actioned here (no bench-file changes are in scope for eu-7oshh).

## 5. Wider AoC-2025 corpus (breadth, blob mode, single session)

| Program | ratio | hs ticks | Confidence | Note |
|---|--:|--:|---|---|
| day01 part-1 | — | 1,287,304 | n/a | now 117ms — below floor, excluded |
| day04 part-1 | 1.045 | 28,464,618 | measured-single | |
| day05 part-1 | 1.080 | 23,125,537 | measured-single | |
| day06 part-2 | 1.025 | 12,762,617 | measured-single | |
| day07 part-1 | 1.058 | 15,761,985 | measured-single | |
| day07 part-2 | 1.021 | 14,024,234 | measured-single | |
| day11 part-2 | 1.164-1.201 | 184,290,317 | measured-single (2 checks agree) | bc 16-20% slower — a real, moderate finding, not in the four named programs; worth a future look but does not gate this bead |
| **day10 part-1** | bc 8.1-8.4s / **hs did not finish in 180s** (checked twice) | n/a | measured-verified (qualitative: reproduced twice) | **New finding**: HeapSyn has its own severe tail-risk case, the same shape as the old day08 GC pathology. This is evidence *for*, not against, retiring HeapSyn — it is not a case of bytecode falling behind. |

All benchmarks ran correctly on both engines (byte-identical rendered output
where compared); no bytecode panics encountered (the known >1900-record TOML
opcode-overflow defect from the protocol was not triggered by anything in this
session).

## 6. Verdict

**READY.** The bead's deciding evidence — day03, day08, day09, fib in blob
mode — shows bytecode at parity or better on all four, a dramatic reversal
from the last study's 1.36-52× findings. The canonical suite is parity
(`io_loop`, `hof_fold`) or mildly bc-slower within the noise band
(`import_export_*`, plausibly the tracked eu-n8c5e blob-inline tax). Two
caveats for the owner, neither blocking: (1) day11-p2 is a genuine ~16-20%
bc-slower outlier outside the "big four" that may be worth a targeted look;
(2) the canonical suite's `015/018/019` benches have outrun their intended
scale and should be re-sized in a follow-up before the next A/B session.

Machine load violated the quiet-machine rule throughout (§2); all wall figures
are `measured-single` accordingly even though two sessions agreed closely.
This weakens the strength of the individual wall numbers slightly but not the
qualitative direction — every relevant ratio is far inside the noise band that
would need to close, or already inverted in bytecode's favour, and the
deterministic ticks confirm the workloads are unchanged from prior sessions.

## Repro

```bash
cd eucalypt-worktrees/stopwatch-7oshh
cargo clean && cargo build --release
cargo xtask prelude-compile   # regenerate lib/prelude.blob
cargo xtask engine-ab --runs 7                 # canonical suite, blob
cargo xtask engine-ab --check                  # per-lineage regression check
# source config: mv lib/prelude.blob aside, `cargo build --release`, re-run,
# then restore the blob and rebuild.
cd examples/aoc25
timeout 60 ../../target/release/eu --heap-limit-mib 12288 -t part-1 day03.eu
EU_HEAPSYN=1 timeout 60 ../../target/release/eu --heap-limit-mib 12288 -t part-1 day03.eu
```
