# AoC 2025 re-baseline on 0.16 (eu-oaz72.38)

Date: 2026-11-18. Scope: every day/part of `examples/aoc25` on master
(`9914d7b`), after the 0.16 solution revisions (#1273, #1280, #1283) and the
prelude `split-on` fix (#1296). No solution code changed. The list-retention
fix (#1268) was not merged at the time and is **not** in these figures.

The summary table is in `examples/aoc25/PERFORMANCE.md` ("Summary (0.16
re-baseline, 2026-11-18)"); this report records the method, deviations and
side findings.

## Method

- Inputs: the real puzzle inputs, present locally in the gitignored
  `examples/aoc25/inputs/dayNN.txt` (never committed).
- Binary: `cargo xtask prelude-compile && cargo clean && cargo build
  --release` on master, rustc 1.99.1. One binary and one blob for every arm.
- Runs: `timeout 600 eu --statistics-file … run/dayNN.eu -t part-N` from
  `examples/aoc25/`, wall timed around the whole process. Seven rounds per
  part. Parts with a `par-*` site (days 04, 08, 09, 10, 11) ran as
  interleaved pairs per round: *seq* (`EU_PP_THRESHOLD=999999999`, so every
  `par-*` call takes the sequential fallback) then *par* (the default:
  `par-*` forks at two or more elements; 4 cores).
- Reported: wall median-of-7 with min–max range; ticks, allocations, GC
  count and peak stack from the median sequential run (ticks are
  deterministic: a repeat run reproduced them exactly). The median run's
  statistics JSON replaces `examples/aoc25/stats/dayNN-part-N.json`.
- Answers: identical across all runs and both arms for every part, and equal
  to the pre-revision solutions' answers for the 19 parts those solve within
  the timeout. The four formerly timed-out parts were checked against an
  independent reference in the revision PRs (`2026-11-17-aoc25-slow-parts.md`).
- Same-machine "before": the solutions as of `cb93459a^` (pre-revision),
  run on the same binary, median-of-5, seq and par arms where they had a
  `par-*` site (par forced with `EU_PP_THRESHOLD=2`, which equals the current
  default). The four parts that timed out in every earlier baseline were not
  re-run.

## Confidence and deviations

Every figure is **measured-single**. Deviations from
`docs/superpowers/engine-ab/PROTOCOL.md`:

- A 4-core cloud container, not a quiet machine; load average 1.3–2.5 during
  the run (the runner itself contributes about 1).
- One session; not reproduced on a second machine.
- The 0.14 figures in the "Previous" column came from a 14-core M3 Max, so
  cross-machine wall comparisons are indicative only. The same-machine
  pre-revision column is the like-for-like comparison.
- Many parts are under ~200 ms, dominated by startup (`eu -e 1` takes about
  0.058 s with the blob); they are labelled as such and not used in ratios
  (the table gives no parallel speedup where either arm is under 200 ms).

## Headline changes (same machine, sequential)

- All four former >600 s parts finish: day04-p2 2.67 s, day08-p2 4.58 s,
  day09-p2 4.63 s, day10-p2 5.64 s (par 2.72 / 2.20 / 3.44 / 2.88 s).
- day10-p1 6.79 s → 1.25 s; day08-p1 5.07 s → 3.14 s; day09-p1 1.97 s →
  1.04 s; day06-p2 0.71 s → 0.26 s; day03-p2 0.42 s → 0.25 s.
- From above 200 ms to startup-dominated (under 200 ms, so no ratio is
  given): day11-p2 10.42 s → 0.16 s, day05-p1 0.51 s → 0.11 s, day07-p1/p2
  0.67/0.72 s → 0.19 s.
- Unchanged within noise: day01, day02, day04-p1, day06-p1, day12-p1.
- Parallel speedups on 4 cores: 1.3–2.5× where the work is heavy
  (day04-p1 2.47×, day08-p2 2.08×, day10-p1 2.06×, day10-p2 1.96×).

## Side findings (not investigated here)

1. **No blob on a plain build.** `lib/prelude.blob` is gitignored, so a fresh
   clean build silently takes the source-prelude fallback. A first full run
   made that way was discarded: startup was 0.116 s rather than 0.058 s, and
   small parts read about 0.06–0.10 s slower. This is documented in
   `docs/development/prelude-blob.md`; noted because it is easy to miss when
   re-running this baseline.
2. **Ticks are higher with the blob than without** on several parts (e.g.
   day09-p1 30.3M → 35.0M, day04-p1 17.5M → 19.4M, day10-p1 lower: 27.2M →
   26.0M), while wall is lower or equal. The prelude-blob doc describes a
   source-prelude tick *handicap*; on this corpus the sign varies. Worth a
   look under eucalypt-performance; not a correctness issue.
3. No part was wrong or unexpectedly slow. The slowest is day10-p2 at 5.6 s
   sequential (133.5M ticks).

## Re-running after #1268 lands

From the repository root, with the real inputs in `examples/aoc25/inputs/`:

```sh
cargo xtask prelude-compile && cargo clean && cargo build --release
python3 scripts/aoc25-bench.py --runs 7 --out /tmp/aoc25-bench --copy-stats
```

It prints the sequential/parallel table (wall median-of-N with range, ticks,
allocs, GCs, max stack), fails if any run exits non-zero or any answer
differs between runs or arms, and with `--copy-stats` refreshes
`examples/aoc25/stats/`. The script runs non-`par` days with the same
sequential setting; they have no `par-*` call, so this matches the default.
