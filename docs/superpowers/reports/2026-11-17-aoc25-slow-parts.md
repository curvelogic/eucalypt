# AoC 2025 slow parts: day 04, 08 and 09 part 2

Scope: the three parts that previously did not finish in 600 s
(`examples/aoc25/PERFORMANCE.md`). All answers were checked against an
independent reference implementation on the real inputs and against the
worked examples (`eu test .`).

Method: release build of master plus this branch, 4-core Linux container,
wall time of `eu run/dayNN.eu -t part-N`, 3 runs, ranges shown, label
**measured-single** (one session, not on a quiet machine, no interleaving).
"seq" sets `EU_PP_THRESHOLD=999999999` (no forking); "par" is the default
(`par-*` forks, 4 cores).

| Part | Before | After seq | After par |
|------|--------|-----------|-----------|
| day04 p2 | >600 s | 3.6 s (3.61–3.66) | 3.6 s (3.47–3.76) |
| day08 p2 | >600 s | 5.8 s (5.78–5.99) | 2.9 s (2.82–2.91) |
| day09 p2 | >600 s | 7.8 s (7.54–8.05) | 6.0 s (5.92–6.27) |
| day08 p1 (dist2 destructured) | 7.0 s (6.70–7.18) | 4.9 s (4.50–5.13) | 2.4 s (2.25–2.60) |
| day09 p1 (area destructured) | 3.1 s (3.01–3.27) | 1.5 s (1.50–1.54) | 1.1 s (1.04–1.12) |

## What changed

- **day04 p2**: worklist peel. A dict maps each roll (key `1000*row+col`)
  to its neighbour count, seeded from the part 1 block sums. Removing a
  roll relieves its eight neighbours; a count falling from 4 to 3 pushes the
  roll on a stack, so each roll is handled once. `seq` keeps the loop state
  forced (flat stack).
- **day08 p2**: the old version generated all ~500k pairs (about 46 s for
  generation alone, plus sort and Kruskal). Now only pairs within a distance
  bound `d` are generated (walk the x-sorted points while the x gap is at
  most `d`, then keep encoded keys below the `d^2` limit) and Kruskal runs on
  those. `d` doubles from 20000 until the points are connected and the last
  merge is no longer than `d`; at that point the merge history equals the
  true one, so the bridge is exact, not heuristic. `dist2` uses destructured
  arithmetic.
- **day09 p2**: coordinates are compressed to ranks (about 248 × 248
  cells), inside cells are found by a parity scan across vertical edges
  (`scanl`, a set of crossings), 2D prefix sums go in an `arr`, and a
  rectangle is tested in O(1). A pair is tested only if its area beats the
  suffix best (`foldl-strict` with a lazy `fits` argument). 49,067 pairs have
  an area above the answer, so about that many tests run; an approach that
  sorted all pairs by area and stopped at the first valid one was slower
  (the same tests plus a 123k-element sort).

## Where the remaining time goes

Dict operations dominate day04 p2: about 130k `get-or`/`assoc` calls take
about 2.5 s, i.e. roughly 10–25 µs per call on a 12k-entry dict with integer
keys (a microbenchmark of 60k `assoc` into a 12k-entry dict took about
1.1 s, 60k `get-or` about 0.7 s). That is far above what a persistent tree
should cost, and about a quarter of the run is GC mark time. This is a
profile-and-propose item for the dict intrinsics (not built here). Hypothesis
to test: per-call cost is in the intrinsic (node copying or key
comparison), not in interpreter ticks (about 170 ticks per call).

## Not covered

day10 part 2 and the remaining days belong to later work.
