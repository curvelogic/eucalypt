# eu-oaz72.175: blob prelude demand analysis — verification figures

Follows `2026-11-17-blob-list-retention.md` (branch `eu-oaz72.175/blob-trace`).
Release builds, blob mode, base = master 916425b (no PR #1268). Confidence:
ticks and peak blocks are deterministic (measured-single); wall is
median-of-5 interleaved pairs, one machine, not quiet (measured-single; spreads
overlap, so no wall claim is made).

| bench | ticks before → after | peak blocks before → after | wall median [min-max] before / after |
|---|---|---|---|
| 015 | 2,018,163 → 2,048,157 | 1024 → 1024 | 0.218 [.205-.220] / 0.211 [.209-.241] |
| 016 | 5,584,259 → 5,630,143 | 2159 → 2443 | 1.493 [1.488-1.554] / 1.527 [1.480-1.560] |
| 017 | 5,538,010 → 5,583,894 | 2131 → 2106 | 0.980 [.893-1.026] / 0.899 [.863-1.046] |
| 018 | 2,401,814 → 2,401,813 | 1741 → 1626 | 0.266 / 0.248 |
| 019 | 1,050,742 → 1,068,741 | 1024 → 1024 | 0.116 / 0.119 |
| 020 | 7,896,311 → 7,897,347 | 1049 → 1046 | 0.351 / 0.361 |
| 021 | 716 → 716 | 0 → 0 | 0.057 / 0.057 |
| 022 | 49,200,718 → 49,200,717 | 29,609 → 36,415 | 4.826 [4.51-5.02] / 5.204 [4.49-5.68] |

Retention (master base): `range(0,n) foldl-strict(+,0)` peak 1,031 after (was
linear in n); `range(0,200000) count` 14,015 → 13,351 only, because the
closed-lambda pin (PR #1268) still dominates there.

Open: bench 022 peak blocks rose 23% (29.6k → 36.4k) with unchanged ticks, and
016 rose 13%; not investigated. Wall spreads overlap.
