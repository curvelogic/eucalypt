# Dict rendering: cost of the render fallback (eu-oaz72.21 PR 4)

Date: 2026-11-14. Design note §8 asks PR 4 to measure the extra intrinsic
call that every value reaching the render wrapper's native fallback now
pays (`DICT.RENDER_VIEW`, which returns unit for a non-dict).

**Set-up.** Two release builds on one machine: master at `cb486a7`
(baseline) and this branch. Both run with `--source-prelude`, so neither
uses a blob. Workload: render to JSON a list of 400,000 computed numbers
(`range(0, 400000) map(* 3)`) and 100,000 computed strings
(`range(0, 100000) map(_ str.of)`) — 500,000 scalars, each of which
reaches the native fallback. Five interleaved pairs, wall clock in ms.

| build | ticks (`-S`) | wall, five runs (ms) | wall median |
|---|---|---|---|
| master | 74,300,358 | 7069 7517 7652 7663 8763 | 7652 |
| branch | 76,700,358 | 6948 6979 7152 7400 7642 | 7152 |

**Result.** Ticks rise by 2,400,000 (+3.2%), about 4.8 ticks per rendered
scalar, and are deterministic (measured-verified: identical on repeat).
Wall time does not show the change: the spread of either build (about
1.0–1.7 s) is far larger than any difference (measured-single, a shared
cloud machine, not quiet). A value that the wrapper renders through a
constructor arm (booleans, null, boxed scalars, lists, blocks) pays
nothing. Rendering is a small part of most programs, so no change is
proposed; if it ever matters, the native fallback could test the native's
kind before calling the intrinsic.
