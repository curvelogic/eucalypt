# Dict ordered operations: counting and Dijkstra benchmarks

- **Date:** 2026-11-13
- **Bead:** eu-oaz72.21 (dict PR 3; design note
  `docs/superpowers/plans/2026-11-12-dict-design.md` §10 item 3, OPEN 7)
- **Confidence:** measured-single — one session machine (4 vCPU, a cloud
  container, not a quiet dedicated host), one release binary, the five
  configurations interleaved, five rounds; wall time median with min–max.
  Ticks and peak blocks are deterministic per configuration.

## What was measured

`tests/harness/bench/024_dict_counting.eu` — counting by key: eight
synthetic stones, 75 blinks of the public splitting rules, the counts held
in a dict (3,634 keys, total 315,290,635,574,411, checked against Python).
Four formulations of one blink:

- `update-strict` (the committed bench): fold the stones' children into a
  fresh dict with `dict.update-strict(c, + n, 0)`;
- `update`: the same with the lazy `dict.update`;
- `merge-with`: build each stone's child counts, then fold them together
  with `dict.merge-with(+)`;
- `merge-with-strict`: the same with `dict.merge-with-strict(+)`.

`tests/harness/bench/023_dict_dijkstra.eu` — Dijkstra over a synthetic
141 × 141 grid (distance 1085, checked against Python `heapq`), with the
queue a dict keyed by `[distance, [x, y]]` and `dict.pop-min`.

## Results

| configuration | ticks | peak blocks | wall median (min–max) |
|---|---:|---:|---:|
| counting, `update-strict` | 50,626,158 | 2,753 | 3.20 s (3.11–3.48) |
| counting, `update` (lazy) | 50,057,918 | 26,595 | 5.59 s (5.47–5.75) |
| counting, `merge-with` (lazy) | 57,218,702 | 1,408 | 4.47 s (4.38–4.76) |
| counting, `merge-with-strict` | 76,816,002 | 4,275 | 7.62 s (7.28–7.74) |
| Dijkstra 141 × 141 | 39,940,342 | 19,134 | 3.83 s (3.77–3.95) |

Before dicts, the same counting reached 25 blinks in 1.3 s, 40 blinks in
15 s and 8 GB, and ran out of memory at 75 (bead acceptance note,
2026-11-12).

## Reading

- **OPEN 7 (strict variants).** The lazy `update` leaves a chain of
  unevaluated additions per key until the next blink reads it: the peak heap
  is ten times the strict variant's and the run 75% slower. That supports
  the accepted recommendation to add `update-strict` and
  `merge-with-strict`.
- `merge-with-strict` is a prelude fold over the second dict's entries
  (design note §13), so it pays per-entry prelude overhead that the native
  join-based `merge-with` does not; here it is the slowest formulation. The
  lazy `merge-with` stays cheap in this program because each blink's merged
  values are read by the next blink, so no chain builds up. A native strict
  merge is a possible follow-up if programs need it.
- Dijkstra's peak heap grows with the grid, not with the number of steps
  (the 71 × 71 grid needs a quarter of the heap). Most of it is the engine's
  general per-object and fragmentation cost, which a plain cons list of the
  same length shows too, rather than the dict.
