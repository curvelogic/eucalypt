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

Re-measured at head `c5411e3` (the PR after master was merged in at
`a43e0f4`), with the variants below, one release binary and one freshly
compiled blob, the three configurations interleaved in five rounds:

| configuration | ticks | max stack | peak blocks | wall median (min–max) |
|---|---:|---:|---:|---:|
| native | 54,641,595 | 7,235 | 1,198 | 2.23 s (2.15–2.50) |
| fold | 71,126,414 | 20,707 | 3,338 | 4.49 s (4.47–4.64) |
| lazy | 52,994,438 | 7,291 | 1,746 | 2.48 s (2.26–2.64) |

The first measurement, at head `bd9c86a` before master was merged in
(native and fold interleaved in one set of five rounds, native and lazy in
a second), gave native 57,613,624 ticks and 2.35 s (2.26–2.41), fold
73,095,768 and 4.41 s (4.32–4.64), lazy 55,966,467 and 2.43 s (2.36–2.52).
The tick counts differ because the prelude and engine on master moved in
between; the ratios are the same.

The round-1 reviewer of PR #1321, measuring independently from a clean
release build with its own reconstruction of the variant, found native
54.8M ticks against fold 70.9M and wall medians of 2.89 s, 4.79 s and
3.18 s (native, fold, lazy): the same direction and size.

## The variants

Each is `tests/harness/bench/024_dict_counting.eu` with its `spawn`
replaced by the lines below. The rest of the file is unchanged.

```eu,notest
` "the child counts of the `n` stones `s`, as a dict"
kids(n, s): s children foldl(add(n), dict.empty)

` "merge the child counts of the `n` stones `s` into the counts `acc`"
spawn(acc, [s, n]): merge(+, acc, kids(n, s))
```

followed by one definition of `merge`:

```eu,notest
` "native"
merge: dict.merge-with-strict

` "lazy"
merge: dict.merge-with

` "fold: the prelude definition of `dict.merge-with-strict` before eu-oaz72.124"
merge(f, a, b): {
  ` "merge the entry `[k, v]` of `b` into `acc`"
  step(acc, [k, v]): (acc dict.contains?(k)) then(acc dict.update-strict(k, flip(f, v), null), acc dict.assoc(k, v))
}.(b dict.to-list foldl(step, a))
```

Ticks, max stack and peak blocks come from `eu -S <variant> -t
bench-dict-counting`. Wall time is the elapsed time of the same command
without `-S`.

## Reading

- The native strict merge takes 23% fewer ticks than the fold (21% at the first measurement) and about half
  the wall time, and is level with the lazy `merge-with` (within the
  spread): the per-entry prelude overhead the 2026-11-13 report attributed
  to the fold is gone.
- Stack: on a merge of two 100,000-entry dicts the fold reaches a max stack
  of 300,007; the native merge stays at 14 (19 in the harness target, which
  gates `max-stack < 1000`). The remaining stack in the benchmark above
  comes from the program's other folds, not the merge.
