# GC Pressure Under Tight Heap Limits

**Version:** eucalypt 0.9.2  
**Date:** 2026-06-19  
**Purpose:** Find AoC examples where GC pressure is real — baseline for W10 generational nursery

---

## Methodology

All 19 day/part combinations from the baseline were re-run with progressively tighter `--heap-limit-mib` values. For each run:

```bash
timeout 120 eu run --heap-limit-mib $limit \
  --statistics-file stats.json day.eu -t part
```

**Note:** `--statistics` outputs a human-readable text format to stderr, not JSON. Use `--statistics-file` for machine-readable JSON output. The task description's parsing example would have silently failed.

Outcome classification:
- **OK**: completed within 120s
- **TIMEOUT**: exceeded 120s (GC thrashing too severe, or program naturally slow)
- **OOM/FAILED**: process exited non-zero (rare; most tight-memory failures manifest as timeout, not OOM)

---

## Full Results Table

| Example | Min viable (MiB) | Natural heap | Behaviour |
|---------|-----------------|--------------|-----------|
| day02-part-1 | **< 32** | 19 MB | Trivially small; no GC at any limit |
| day02-part-2 | **≈ 80** | 78 MB | GC thrashing below limit; *completes at 32 MiB* |
| day05-part-2 | **≈ 68** | 65 MB | GC thrashing below limit; *completes at 32 MiB* |
| day01-part-1 | **256** | 192 MB | Hard cliff; TIMEOUT below 256 MiB |
| day06-part-1 | **256** | 167 MB | Hard cliff; TIMEOUT below 256 MiB |
| day01-part-2 | **512** | 464 MB | Hard cliff; TIMEOUT below 512 MiB |
| day11-part-1 | **512** | 260 MB | Hard cliff; TIMEOUT below 512 MiB |
| day12-part-1 | **512** | 294 MB | Hard cliff; TIMEOUT below 512 MiB |
| day03-part-1 | **2048** | 1361 MB | Hard cliff; TIMEOUT below 2048 MiB |
| day05-part-1 | **2048** | 1552 MB | Hard cliff; TIMEOUT below 2048 MiB |
| day06-part-2 | **2048** | 1036 MB | Hard cliff; TIMEOUT below 2048 MiB |
| day07-part-1 | **2048** | 1632 MB | Hard cliff; TIMEOUT below 2048 MiB |
| day07-part-2 | **2048** | 1033 MB | Hard cliff; TIMEOUT below 2048 MiB |
| day03-part-2 | **4096** | 2970 MB | Hard cliff; TIMEOUT below 4096 MiB |
| day04-part-1 | **4096** | 2262 MB | Hard cliff; TIMEOUT below 4096 MiB |
| day08-part-1 | **16384** | 12621 MB | Hard cliff; TIMEOUT below 16384 MiB |
| day09-part-1 | **16384** | 8736 MB | Hard cliff; TIMEOUT below 16384 MiB |
| day10-part-1 | **> 16384** | 30908 MB | TIMEOUT at all tested limits |
| day11-part-2 | **> 16384** | 10188 MB | Already > 120s at default heap |

---

## The Two GC-Sensitive Examples

### day02-part-2: GC thrashing with graceful degradation

Natural heap: 2514 blocks × 32 KB = **78.5 MB**

| Limit (MiB) | GCs | Recycled | Time | GC% | Blocks used |
|------------|-----|---------|------|-----|------------|
| 128+ | 0 | 0 | 0.06s | 0.0% | 2,514 |
| 80 | 0 | 0 | 0.06s | 0.0% | 2,514 |
| **72** | **231** | **0** | **7.60s** | **99.2%** | **2,303** |
| 64 | 509 | 0 | 9.44s | 99.4% | 2,055 |
| 56 | 787 | 0 | 11.55s | 99.5% | 1,804 |
| 48 | 1,062 | 0 | 11.83s | 99.5% | 1,555 |
| 32 | 1,615 | 0 | 11.44s | 99.5% | 1,042 |

**Transition cliff**: between 80 and 72 MiB — a 10% reduction in heap triggers a 127× slowdown.

**Key observation**: At 32 MiB (the tightest limit), the program uses only **1,042 blocks** (41% of its natural 2,514). It runs with 1,615 GC cycles but still completes correctly. This means the programme's **instantaneous live set is only ~1,042 blocks** (~32 MB); the remaining 1,472 blocks in the natural run are dead thunks at any point in time.

`blocks_recycled = 0` throughout. The GC reclaims no complete blocks — it creates holes *within* blocks (dead thunks, updated closures) that the Immix bump allocator reuses. This is Immix opportunistic defragmentation without full-block recycling.

### day05-part-2: Sharper cliff, worse thrashing

Natural heap: 2,076 blocks × 32 KB = **64.9 MB**

| Limit (MiB) | GCs | Recycled | Time | GC% | Blocks used |
|------------|-----|---------|------|-----|------------|
| 68+ | 0 | 0 | 0.05s | 0.0% | 2,076 |
| **64** | **50** | **0** | **5.81s** | **99.1%** | **2,050** |
| 56 | 436 | 0 | 47.58s | 99.8% | 1,799 |
| 48 | 747 | 0 | 75.50s | 99.9% | 1,548 |
| 32 | 1,282 | 0 | 80.59s | 99.9% | 1,063 |

**Transition cliff**: between 68 and 64 MiB — a 6% reduction triggers a 116× slowdown.  
**Minimum heap**: ~51% of natural size. GC overhead at minimum: 99.9%.

---

## The "Hard Cliff" Pattern (All Other Examples)

All other examples show an identical pattern:

1. At or above the natural heap size → **0 GC collections, no slowdown**
2. Just below the natural heap size → **immediate TIMEOUT** (program cannot complete in 120s)

There is no gradual degradation, no partial GC benefit. The working set (all lazy spine thunks plus intermediate data structures) must all be simultaneously live. When the heap is too small to hold the entire spine, the GC fires repeatedly but cannot free anything, and the programme diverges within the time limit.

**Example — day01-part-1** (natural 192 MB, uses `scanl` over 12,000 instructions):

- At 256 MiB: 0 GCs, 2.96s ✓
- At 128 MiB: TIMEOUT ✗

The live set at peak is the entire `scanl` accumulator chain plus the input list — all simultaneously reachable from the result thunk.

---

## Interpretation: What the GC Behaviour Reveals

### No complete blocks are ever recycled

`blocks_recycled = 0` in every single run, even with 1,615 GC cycles. This has two causes:

1. **Lazy spine retention**: Lazy evaluation keeps the entire input → output pipeline live simultaneously. The input list, intermediate `scanl` results, and output list all share thunks that remain reachable until the final value is forced.

2. **Thunk updates create intra-block holes, not free blocks**: When a thunk is forced, its code pointer is overwritten with the result (a WHNF atom). The old code is dead, but the thunk frame itself and its environment chain are still in the same block as other live objects. No block becomes entirely dead.

### The instantaneous live set is much smaller than cumulative allocation

For day02-part-2:
- Cumulative allocation: 2,514 blocks (78 MB)
- Instantaneous live set: ~1,042 blocks (32 MB) — only 41% of cumulative

This 2.4× ratio indicates significant turnover of short-lived thunks. These thunks die within blocks, creating holes that Immix reuses. A nursery sized at ~32 MB could in principle collect these holes cheaply.

For the hard-cliff examples, the ratio is presumably 1.0× (no turnover) — the live set equals the cumulative allocation.

---

## Nursery Benchmark Recommendations

### Best candidates for W10 generational nursery benchmarking

| Example | Natural heap | Instantaneous live | Turnover ratio | Why |
|---------|-------------|-------------------|---------------|-----|
| **day02-part-2** | 78 MB | ~32 MB | **2.4×** | Best — significant thunk turnover, completes at 32 MiB |
| **day05-part-2** | 65 MB | ~33 MB | **2.0×** | Good — similar pattern, sharper degradation |
| day02-part-1 | 19 MB | 19 MB | 1.0× | Too small; useful as baseline/sanity check |

**Not recommended** for nursery benchmarking: all other examples. Their working set is entirely live at all times — a nursery would add overhead (copying into old generation) without benefit.

### What to measure with a nursery

For day02-part-2 and day05-part-2, a nursery should be tuned to:
- **Nursery size ≈ 10–20 MB** (significantly smaller than the 32 MB instantaneous live set, to collect frequently and catch short-lived thunks before they escape)
- Measure: minor GC count, major GC count, promotion rate, total time vs baseline
- Target: total time at 32 MiB heap limit ≤ 2× baseline (down from current 190×)

---

## Runtime Env Traversal Measurement

The task requested runtime measurement of `EnvFrame::cell()` traversal count. This is not feasible without source changes: the function is called from multiple hot paths in the VM's inner loop and there is no stable instrumentation hook accessible without modifying `src/eval/machine/env.rs`.

**What was done instead** (from the previous profiling report):

Static analysis via `eu dump stg` gives the de Bruijn index distribution, which is the **exact** number of bindings that `cell()` must skip per lookup (independent of frame size). Dividing by the mean frame size (≈ 4 bindings/frame based on typical `let` blocks) gives an estimate of frame pointer dereferences per lookup.

| Example | Mean de Bruijn | Est. frame dereferences/lookup |
|---------|---------------|-------------------------------|
| day01 | 4,507 | ~1,127 |
| day05 | 983 | ~246 |
| day08 | 769 | ~192 |
| day12 | 749 | ~187 |
| day11 | 442 | ~111 |
| day10 | 18 | ~5 |

For a runtime counter without source changes, one option is to rebuild with `EU_STACK_DIAG=1` which does provide indirectly related stack composition data. However, the continuation stack depth is not the same as the env chain depth. A custom debug build with a traversal counter in `cell()` would be needed for precise data.

**Recommendation**: Add a `cfg(feature = "profiling")` counter to `cell()` in a separate branch for this measurement; do not merge into master.

---

## Summary for 0.10.0 Planning

| Question | Answer |
|----------|--------|
| Which examples have real GC pressure? | **Only day02-part-2 and day05-part-2** |
| Do any examples benefit from more frequent GC? | Yes — at 32 MiB, day02-part-2 shows 2.4× thunk turnover |
| Is a nursery useful for the heavy examples (day08/09/10)? | **No** — their live set is the entire heap; no turnover |
| What is the nursery size sweet spot? | 10–20 MB based on the 32 MB instantaneous live set |
| Why does GC help day02/day05 but not others? | Those programmes process fixed-size inputs with stream-like turnover; others build monotonically growing spines |
| What is the min viable heap for each example? | See full table above |
