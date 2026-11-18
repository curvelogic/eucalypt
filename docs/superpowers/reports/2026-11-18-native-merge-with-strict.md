# Native `dict.merge-with-strict`: counting benchmark

- **Date:** 2026-11-18
- **Bead:** eu-oaz72.124 (follow-up to eu-oaz72.21; earlier figures in
  `2026-11-13-dict-ordered-ops-benchmarks.md`)
- **Confidence:** measured-single — one session machine (4 vCPU, a cloud
  container, not a quiet dedicated host), one release binary and one blob
  for every configuration, configurations interleaved, five rounds; wall
  time median with min–max. Ticks, max stack and peak blocks are
  deterministic per configuration.

## What was measured

The `merge-with-strict` formulation of the counting benchmark from the
2026-11-13 report: `tests/harness/bench/024_dict_counting.eu` with each
blink building every stone's child counts as a dict
(`children foldl(add(n), dict.empty)`) and folding them together with
`merge(+)`. Total 315,290,635,574,411 and 3,634 distinct stones, as the
committed bench checks, in every configuration.

Three configurations, all run by the same binary (built from this branch):

- **native** — `merge: dict.merge-with-strict`, the new intrinsic;
- **fold** — `merge` defined in the program as the prelude fold that
  `dict.merge-with-strict` was before this change, verbatim;
- **lazy** — `merge: dict.merge-with`.

## Results

| configuration | ticks | max stack | peak blocks | wall median (min–max) |
|---|---:|---:|---:|---:|
| native | 57,613,624 | 7,237 | 1,280 | 2.35 s (2.26–2.41) |
| fold | 73,095,768 | 20,711 | 3,667 | 4.41 s (4.32–4.64) |
| lazy | 55,966,467 | 7,293 | 1,311 | 2.43 s (2.36–2.52) |

Native and fold were interleaved in one set of five rounds, native and lazy
in a second (native's median there was 2.33 s, 2.26–2.67).

## Reading

- The native strict merge takes 21% fewer ticks than the fold and about half
  the wall time, and is level with the lazy `merge-with` (within the
  spread): the per-entry prelude overhead the 2026-11-13 report attributed
  to the fold is gone.
- Stack: on a merge of two 100,000-entry dicts the fold reaches a max stack
  of 300,007; the native merge stays at 14 (19 in the harness target, which
  gates `max-stack < 1000`). The remaining stack in the benchmark above
  comes from the program's other folds, not the merge.
