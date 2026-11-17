# Self-recursive loops with a parameter step: stack growth (eu-oaz72.147)

## Finding

The cookbook measurements (2026-11-14) showed loops whose step or test is a
parameter growing the stack linearly. Two distinct causes were mixed up:

1. **Bug (fixed here).** A self-recursive combinator that applies one of its
   own parameters (`times(f, n, x)`, `c(p?, n)`) qualifies for
   copy-specialisation (`tag.rs::recursive_combinator`). `distribute` then
   substituted the binding's value into *its own body*, unrolling the
   recursion once and leaving an unreduced `(λ(t, f, c). IF(c, t, f))(...)`
   around the recursive call. Demand analysis sees an application of a
   lambda value, so the recursive call and its arguments went lazy. The
   `pre_expand_inlines` comment already states the intent ("recursion must
   still resolve to itself, not unroll"); `distribute` did not honour it.
   Fix: exclude the binding's own name from the substitution set.
2. **Not a bug: genuinely lazy state.** `times(f, n, x): (n = 0) then(x,
   times(f, n - 1, f(x)))` is not strict in `x` (`times(const(0), 1, ⊥)`
   is not ⊥), so `f(f(f(...)))` is a real thunk chain forced at the end.
   A named, inlinable step makes `x` strict only by inlining it; an unknown
   `f` cannot. Making this constant-stack without user help needs
   call-by-value for an argument that is not provably strict (a semantic
   change) or .67 (deferred, decision 19). Not attempted.

## Results (n = 1e6, `-S`, release, prelude blob)

| Loop | Max stack before | after |
|---|---|---|
| `c(f, n): (n = 0) then(0, c(f, f(n)))`, `f` = `_ - 1` | 500,007 | 7 |
| `c(p?, n): p?(n) then(0, c(p?, n - 1))` | 500,007 | 7 |
| `times` with `seq(x, ...)` in the recursive branch | 500,007 | 7 |
| `times(+ 3, n, 0)`, lazy `x` (no `seq`) | 2,500,003 | 2,000,002 |

Ticks (deterministic counts, `-S`): loop `c(f, n)` 36.25M to 30.0M; `p?` loop
35.5M to 30.0M. Benches 015–022 with the blob, base to fix: 015 2,021,908 to
2,018,163; 016 5,604,242 to 5,584,259; 017 5,557,993 to 5,538,010; 018 and 021
unchanged; 019 1,055,241 to 1,050,742; 020 7,893,645 to 7,896,311; 022
48,900,719 to 49,200,718 (+0.6%). Tick counts only; one binary pair, no wall
times. With the prelude compiled from source (no blob) 022 was +2.1% and
015/018/019 gained about 13k fixed ticks; the shipped configuration uses the
blob. Confidence: measured-single (ticks are deterministic, one run).

## User-level workaround for case 2

Force the state each step so it never becomes a chain:

```
times(f, n, x): (n = 0) then(x, seq(x, times(f, n - 1, f(x))))
```

This runs in constant stack after the fix (before it, `seq` did not help:
max stack stayed 500,007). `foldl-strict` is the list-shaped equivalent.

## Verification

`tests/harness/233_constant_stack_loops.eu` (three new loops) gates `max-stack < 1000`
at n = 1e5. Fault injection: with the `reduce.rs` change
reverted the test FAILs; restored, it PASSes. Full `cargo test --release`
passes with and without the blob; clippy and fmt clean.
