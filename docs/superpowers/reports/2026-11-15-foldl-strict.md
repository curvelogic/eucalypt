# foldl-strict (eu-oaz72.78, owner decision 31)

`foldl-strict(op, i, l)` evaluates the accumulator to WHNF per step via
`if(a list?, rec, rec)` (the type predicate forces `a` and never errors).
Other shapes (local helper, `rest` thunk) kept a stack frame per element.
Demand analysis derives `op`/`l` strict and leaves `i` lazy on the empty-list
path, which matches the definition.

## Measurements (release builds, base = master, new = branch)

Max stack, range/repeat 1e6, blob and `--source-prelude`: sum, product, count
all 9-10 (was ~1e6 growth). measured-single.

Ticks (deterministic; base = origin/master 607cf0f, clean release build with
a freshly compiled blob; an earlier draft of this report used a stale local
master as base and its figures were wrong): bench 019 blob 989,245 ->
1,049,245 (+6%), source-prelude 875,213 -> 953,213 (+9%); 022 unchanged
(46,900,723 blob, 40,700,694 source) because it uses `foldl`, which this change
leaves alone. Max stack on 019: 12,009 -> 16. So `sum`, `product` and `count`
cost about 10 (blob) / 13 (source) more ticks per element in exchange for
constant stack. measured-single.
Wall, 019, 7 interleaved pairs, ms: base 105-130 / new 99-111. Below the 200 ms
floor, so no ratio is claimed.
