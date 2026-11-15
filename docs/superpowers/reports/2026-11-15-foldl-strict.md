# foldl-strict (eu-oaz72.78, owner decision 31)

`foldl-strict(op, i, l)` evaluates the accumulator to WHNF per step via
`if(a list?, rec, rec)` (the type predicate forces `a` and never errors).
Other shapes (local helper, `rest` thunk) kept a stack frame per element.
Demand analysis derives `op`/`l` strict and leaves `i` lazy on the empty-list
path, which matches the definition.

## Measurements (release builds, base = master, new = branch)

Max stack, range/repeat 1e6, blob and `--source-prelude`: sum, product, count
all 9-10 (was ~1e6 growth). measured-single.

Ticks (deterministic): bench 019 1,349,286 -> 1,049,245; 022 61,700,754 ->
46,900,723 (022 still uses lazy `foldl`; max stack 800k unchanged).
Wall, 5 interleaved pairs, quiet machine, ms: 019 base 102-122 / new 94-95;
022 base 3946-4160 / new 3443-3657. measured-single (one binary pair, one
machine; not a ledger row).
