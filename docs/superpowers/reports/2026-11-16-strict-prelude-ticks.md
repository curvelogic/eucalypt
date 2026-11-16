# Tick cost of the strict prelude sites (eu-oaz72.165)

Owner decisions 40 and 41 converted `max-of`, `min-of`, `max-of-by`,
`min-of-by`, `all-true?`, `reduce`, `range`, `iterate` and `scanl` to
constant-stack strict forms (audit: `2026-11-16-seq-audit.md`).

## Method

Ticks (`eu -S`) on the eight canonical benches `tests/harness/bench/015..022`,
two release binaries differing only in `lib/prelude.eu` (before: the base commit's prelude; after: the branch's), each with its own freshly compiled `lib/prelude.blob`, same machine, one run each. Ticks are deterministic, so a single run is
exact; wall time was not measured, so no wall claim is made. Label:
**measured-single** (ticks only; not a PROTOCOL.md interleaved wall study).

## Ticks

| Bench | Before | After | Change | Max Stack before → after |
|-------|-------:|------:|-------:|--------------------------|
| 015 block_merge | 1,991,884 | 2,021,908 | +1.5% | 3,031 → 3,033 |
| 016 import_export_yaml | 5,604,048 | 5,604,242 | +0.0% | 25 → 25 |
| 017 import_export_toml | 5,557,799 | 5,557,993 | +0.0% | 25 → 25 |
| 018 string_scale | 2,359,814 | 2,401,814 | +1.8% | 14,017 → 14,017 |
| 019 list_scale | 1,019,241 | 1,055,241 | +3.5% | 16 → 18 |
| 020 lookup_curve | 7,889,245 | 7,893,645 | +0.1% | 47 → 47 |
| 021 io_loop | 716 | 716 | 0.0% | 26 → 26 |
| 022 hof_fold | 46,500,719 | 48,900,719 | +5.2% | 800,006 → 800,006 |

022 folds `range(0, 400000)` with the lazy `foldl`, so its stack stays deep (the
`foldl` accumulator chain) while the `range` counter chain is gone; the +5.2% is
the per-cell `seq` on the counter (the audit's indicative figure was +8%).

An earlier draft of this table compared a before binary without a prelude blob
(prelude compiled from source on every run) with an after binary with one, and
overstated the cost; this version holds the blob constant.

## Stack depth per function

Walks of 300,000 elements, `Max Stack` from `eu -S`, before → after:
`range(0, n) last` 600,003 → 16; `max-of` 900,003 → 14; `max-of-by` 1,500,002 → 15;
`all-true?` 600,005 → 12; `reduce` 600,005 → 14; `iterate nth` 600,003 → 9;
`scanl last` 600,008 → 20. Gating checks: `tests/harness/300_strict_prelude_sites.eu`.
