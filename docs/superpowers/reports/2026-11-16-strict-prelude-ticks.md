# Tick cost of the strict prelude sites (eu-oaz72.165)

Owner decisions 40 and 41 converted `max-of`, `min-of`, `max-of-by`,
`min-of-by`, `all-true?`, `reduce`, `range`, `iterate` and `scanl` to
constant-stack strict forms (audit: `2026-11-16-seq-audit.md`).

## Method

Ticks (`eu -S`) on the eight canonical benches `tests/harness/bench/015..022`,
one release binary built from `master` (before) and one from the branch
(after), same machine, one run each. Ticks are deterministic, so a single run is
exact; wall time was not measured, so no wall claim is made. Label:
**measured-single** (ticks only; not a PROTOCOL.md interleaved wall study).

## Ticks

| Bench | Before | After | Change | Max Stack before → after |
|-------|-------:|------:|-------:|--------------------------|
| 015 block_merge | 1,903,517 | 2,021,908 | +6.2% | 3,031 → 3,033 |
| 016 import_export_yaml | 5,560,322 | 5,604,242 | +0.8% | 25 → 25 |
| 017 import_export_toml | 5,514,073 | 5,557,993 | +0.8% | 25 → 25 |
| 018 string_scale | 2,246,038 | 2,401,814 | +6.9% | 14,017 → 14,017 |
| 019 list_scale | 917,213 | 1,055,241 | +15.0% | 15 → 18 |
| 020 lookup_curve | 7,887,643 | 7,893,645 | +0.1% | 47 → 47 |
| 021 io_loop | 718 | 716 | -0.3% | 26 → 26 |
| 022 hof_fold | 39,900,694 | 48,900,719 | +22.6% | 800,006 → 800,006 |

022 folds `range(0, 400000)` with the lazy `foldl`, so its stack stays deep (the
`foldl` accumulator chain) while the `range` counter chain is gone; the +22.6% is
the per-cell `seq` on the counter, larger than the audit's indicative +8%.

## Stack depth per function

Walks of 300,000 elements, `Max Stack` from `eu -S`, before → after:
`range(0, n) last` 600,003 → 16; `max-of` 900,003 → 14; `max-of-by` 1,500,002 → 15;
`all-true?` 600,005 → 12; `reduce` 600,005 → 14; `iterate nth` 600,003 → 9;
`scanl last` 600,008 → 20. Gating checks: `tests/harness/300_strict_prelude_sites.eu`.
