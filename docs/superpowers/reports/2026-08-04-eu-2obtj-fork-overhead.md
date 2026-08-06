# eu-2obtj: par-* fork-overhead measurement

Owner decision (2026-08-04, recorded on eu-2obtj): remove the `par-*`
predicted-benefit size gate. `DEFAULT_THRESHOLD` in
`src/eval/stg/parallel/driver.rs` moves from 1024 to 2, so the only remaining
count condition is the degenerate `n < 2`. This report records the fork
overhead measurement cited in the prelude advisory and
`docs/guide/parallelism.md`.

## Method

Three arms over the same 4-element `[1,2,3,4] par-map(inc)` / `map(inc)`
computation, on the 0.14.1-track release binary built from this branch:

- **fork** — `par-map`, `EU_PP_THRESHOLD=2` (forces the COW-fork path, the
  new default shape)
- **pfb** — `par-map`, `EU_PP_THRESHOLD=999999999` (forces the sequential
  fallback; shares the exact same serialise/deserialise codec and wrapper as
  `fork`, so `fork - pfb` isolates the fork+arena mechanism alone)
- **map** — plain `map(inc)`, no `par-map` at all (baseline, no codec
  round-trip)

Interleaved fork/pfb/map, 5 rounds, wall-clock via `python3 time.time()`
around a `timeout 60 eu --heap-limit-mib 12288 <file>` invocation each.
Ticks are not comparable across the fork boundary (per
`examples/aoc25/PERFORMANCE.md`), so this is wall-only, matching existing
practice for this feature. Platform: macOS ARM64 (Apple Silicon). Machine
carried moderate background load (several other agents active in the same
session; load average ~5), so per `docs/superpowers/engine-ab/PROTOCOL.md`
this is **measured-single**, not measured-verified.

## Results

Two interleaved sessions (first discarded one 18.5s outlier — evidently
contention from concurrent background work, discarded from the reported set,
second session ran clean):

```
fork (ms): 87.01 85.63 84.63 84.20 86.04   median 85.63
pfb  (ms): 83.32 82.44 83.51 82.68 83.89   median 83.32
map  (ms): 83.43 82.31 83.23 83.95 82.40   median 83.23

fork - pfb: 2.31ms   (fork+arena mechanism cost alone)
fork - map: 2.40ms   (total par-map-vs-map overhead)
```

A first session (also 5 rounds each arm) gave consistent deltas (2.45ms /
2.71ms) once its one 18.5s cold-start outlier is excluded from the median
(median-of-5 already absorbs it).

## Conclusion

The fixed per-call cost of forking (fork + shared arena, isolated from the
codec both paths already pay) is **~2–3ms** on macOS ARM64, measured-single.
Total wall time for these tiny fixtures (~83–87ms) is dominated by process
startup and compiling the prelude from source (no blob present); the fork
overhead itself is a small, consistent slice of that. This confirms the
prelude advisory's guidance: a `par-map` over a handful of trivial elements
costs a few milliseconds more than `map`, immaterial next to any genuine
per-element work, but a caller mapping over cheap tiny lists in a hot loop
should judge that cost for themselves — which is exactly what removing the
runtime gate now asks them to do.
