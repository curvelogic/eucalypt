# Strict arguments evaluated in place (eu-oaz72.67)

Date: 2026-11-13. Bead: eu-oaz72.67. Master at c6e2036 (after #1173,
derived prelude signatures). Tick figures are **measured-single**: one
machine, one session, release build; ticks are deterministic, so one run
per configuration. Wall figures are as labelled below.

## Change

`src/eval/stg/compiler.rs`. When a call's demand signature proves an
argument strict (strict, non-recursive, saturated call), and the argument
is a saturated strict arithmetic or comparison primop (`ADD SUB MUL DIV
GT GTE LT LTE`, the existing fusible whitelist), the argument is compiled
as `case <arg> of _ -> …` with its value bound in a one-slot case frame,
instead of `let x = thunk <arg> in seq x in f(x)`. No thunk is allocated
and no Update continuation is pushed.

`LetBinder`'s strict binding indices became ordered strict steps (force
a binding, or evaluate in place), realised in argument order, so the
order in which strict arguments are evaluated (and so which error is
raised first) is unchanged. References into the call's own let frame are
bumped past the case frames.

### Why only primops

A case continuation strips metadata from a value returned to it
(`return_meta`); an Update memoises a metadata-annotated value whole. An
arbitrary strict argument evaluated in place would therefore reach its
callee without its metadata (`doc-of(with-doc(5))` in
`tests/harness/165_demand_annotation.eu`). The primops above always
return a bare native, so passing their value is identical. Widening this
needs a continuation that binds a value without stripping metadata — an
engine change, not attempted here.

## Ticks

Loop A (`loop(a, n): if(n = 0, a, loop(a + 1, n - 1))`, n = 1e6):

| | master | branch |
|---|---|---|
| blob mode | 40,000,191 | 35,000,191 |
| source mode | 40,000,194 | 35,000,194 |
| max stack | 7 | 7 |

The bead's target was "no more ticks than before #1158" (37.0 M): met.
Wall, blob mode, median of 7 interleaved master/branch pairs with range
(measured-single, shared cloud machine, so indicative only): master
1.959 s [1.890, 2.159], branch 1.705 s [1.604, 1.871].

Harness gate (`spread`, seven strict accumulators, n = 1e5), ticks per
step, both prelude modes:

| | master | branch |
|---|---|---|
| pre-decoded dispatch | 106 | 89 |
| byte dispatch (`EU_PREDECODE=0`) | 118 | 101 |

The budget is 104 per step, which separates the two in both dispatch
modes (loop A alone does not: byte dispatch on the branch, 43, costs more
than pre-decoded master, 42, for the `then` form).

Prelude loops over `range(0, 1e6)` are unchanged in ticks and stack in
both modes (`sum` 121.0 M blob / 101.75 M source, `foldl(+, 0)`
117.25 M / 101.75 M, `count` 104.0 M / 87.75 M): their accumulator is
passed to an unknown `op`, so it is never a primop argument. A
hand-written strict sum loop, `go(acc + (xs head), xs tail)`, falls from
116.0 M to 114.0 M ticks (blob) and from 100.0 M to 98.0 M (source).

## foldl-strict (decision 12)

This change does not make a generic `foldl-strict` cheaper than `foldl`.
Such a fold passes `op(acc, x)` back as its accumulator: a call to an
unknown function, not a primop, so it is still a thunk, and forcing it
still needs a guard or a `seq` primitive. Only accumulators updated by a
primop directly (a specialised loop) gain.

## Verification

- `cargo test` (all binaries), clippy, fmt clean.
- `tests/harness/errors/`: every file's output and exit code identical
  between master and branch binaries, in blob and source prelude modes
  (232 files × 2).
- Fault injection: with `in_place_primop` returning `None`, only
  `spread-budget` fails, in blob and source mode under both dispatch
  modes; with the frame bump removed, `test-in-place.mixed-args` fails.
- Full harness under `EU_GC_VERIFY=2 EU_GC_STRESS=1`, and under
  `EU_PREDECODE=0`; `scripts/blob-mode-tests.sh`.
