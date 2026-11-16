# iota constant stack (eu-oaz72.85)

Date: 2026-11-16. Machine: shared cloud container, not quiet; two release
binaries (master, and master plus this change) built from clean `target/`
steps, each with its own freshly compiled blob.

## Finding

After eu-oaz72.72/.77/.92/.78 the blob and source prelude already agree:
`iota(panic) nil?` is `false` in both modes and harness 165 gates it.
What remained was stack: with a lazy start, `iota(0) nth(100000)` builds a
chain of 100000 unevaluated `n + 1` thunks (Max Stack 200,003, source mode).

## Change

`iota` keeps its first two cells lazy in the start (`iota(panic) nil?` and
`iota(panic) tail nil?` stay `false`), then runs a local loop that evaluates
its counter as each cell is demanded. `iota(panic) tail tail nil?` now
raises the panic. After: Max Stack 9 (source) and 11 (blob) for the same walk.

`range` was left on its previous lazy counter. Routing it through the new
`iota` cost about 10% wall on bench 022 (median of 7 interleaved: 4145 ms to
4586 ms, `measured-single`). `range(0, 100000) nth(99999)` therefore still
reaches Max Stack 200,001; follow-up bead filed.

## Benches 015-022, ticks (deterministic, `measured-verified` on this machine)

| bench | master | change |
|---|---|---|
| 015 | 2,005,892 | 2,000,884 |
| 016 | 5,659,544 | 5,659,528 |
| 017 | 5,613,295 | 5,613,279 |
| 018 | 2,345,818 | 2,338,814 |
| 019 | 1,049,245 | 1,043,241 |
| 020 | 7,890,549 | 7,890,285 |
| 021 | 716 | 716 |
| 022 | 46,900,723 | 46,500,719 |

Wall, 5 interleaved runs, median: 022 4518 ms vs 4450 ms; 018 277 ms vs 277 ms
(`measured-single`, noisy, under the 200 ms floor for 018). The protocol's
bc/hs pairing no longer applies; this is a master-versus-change comparison.
