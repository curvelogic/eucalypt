# AoC cookbook: measurements (eu-oaz72.16, eu-oaz72.22)

- **Date:** 2026-11-14
- **Build:** release build of `master` at 5f236cc plus this branch's docs
  (no code change), one 4-core cloud container, load average under 0.6.
- **Confidence:** every figure is **measured-single**: one machine, one
  build, wall time taken with `date` around the whole `eu` process,
  sequential (`EU_PP_THRESHOLD=999999999`).

## Dijkstra on a 141 × 141 grid (closes the eu-oaz72.22 criterion)

The cookbook's Dijkstra template (queue: a dict keyed by `[priority, seq]`,
state as value; stale entries skipped) on the 141 × 141 weighted grid of
`docs/guide/advent-of-code.md`, answer 1085 (checked against a Python
`heapq` Dijkstra):

| Runs | Wall times (s) | Median | Ticks | Max stack |
|---|---|---|---|---|
| 5 | 4.08, 4.18, 4.19, 4.25, 4.32 | **4.19 s** | 47.4 M | 39,794 |

For comparison, on the same build the dict guide's variant (queue keyed by
`[distance, cell]`, no counter) ran in 3.4–3.7 s (3 runs).

The same template written with `map(bimap(+ d, identity))` and
`map(juxt(weight, identity))` in place of the two named helpers ran in
5.4–5.9 s (3 runs): about 30% slower, from the generic combinators alone.

## Loops: when the state stays evaluated

One million steps, `-S` max stack depth:

| Loop | Wall | Max stack |
|---|---|---|
| `c(n): (n = 0) then(0, c(n - 1))` | 0.83 s | 7 |
| `c(n, x): (n = 0) then(x, c(n - 1, x + 3))` | 1.23 s | 7 |
| `go(k, s)` calling a named top-level `step(s)` | 1.24 s | 7 |
| `times(f, n, x)` with `f` a parameter, called as `times(+ 3, …)` | 5.15 s | 2,500,003 |
| same, with `f` captured from an enclosing block | 3.75 s | 2,000,003 |
| `c(f, n): (n = 0) then(0, c(f, f(n)))` | 3.59 s | 500,007 |
| `c(p?, n): p?(n) then(0, c(p?, n - 1))` | 3.29 s | 500,007 |
| `iterate(+ 3, 0) drop(1000000) head` | 4.88 s | 2,000,003 |

A self-recursive loop is constant-stack when its step is a known function;
when the step or test is an unknown function (a parameter, or captured),
stack grows with the iteration count. The cookbook advises naming the step
in the loop. Whether demand analysis can do better here belongs with
eu-oaz72.27 (strict accumulation).

A loop of 1e6 steps of `(s * 7 + 3) % 1000003` ran in 5.75 s at constant
stack, against 1.23 s for `x + 3`; the cookbook's example uses 1e5 steps.
