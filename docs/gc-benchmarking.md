# GC Benchmarking Workflow

This document describes the procedure for measuring GC performance when
making changes to the garbage collector or memory management code.

## Prerequisites

Build the release binary (benchmarks should always run against optimised
builds):

```bash
cargo build --release
```

## Tools

### gc-bench.sh — End-to-end benchmarking

The `scripts/gc-bench.sh` script runs three GC stress benchmark programs
multiple times, collects statistics via `--statistics-file`, computes
medians, and compares against a saved baseline.

**Benchmark programs** (in `harness/test/bench/`):

| File | Target | What it tests |
|------|--------|---------------|
| `007_short_lived.eu` | `bench-short-lived` | High allocation churn — many temporary lists created and immediately discarded |
| `008_long_lived_graph.eu` | `bench-long-lived-graph` | Long-lived persistent structure surviving alongside temporary garbage |
| `009_fragmentation.eu` | `bench-fragmentation` | Interleaved retained/discarded allocations producing heap fragmentation |

**Metrics collected** per benchmark:

- `machine_ticks` — VM instruction count (deterministic)
- `machine_allocs` — heap allocation count (deterministic)
- `collections_count` — number of GC collections triggered
- `total_mark_time_secs` — wall-clock time in mark phase
- `total_sweep_time_secs` — wall-clock time in sweep phase
- `peak_heap_blocks` — maximum heap blocks in use at any point

### Criterion benchmarks

`cargo bench` runs Criterion microbenchmarks defined in `benches/gc.rs`.
These measure low-level GC operations:

- **Allocate then collect** — allocation + full collection cycle
- **Collect with survivors** — collection with varying survival rates (0% to 100%)
- **Allocate into recycled** — allocation into previously-collected blocks

## Step-by-step procedure

### 1. Establish baseline

Before making any GC changes, on a clean working tree:

```bash
scripts/gc-bench.sh baseline --runs 5
```

This saves median statistics to `gc-bench-baseline.json` (git-ignored).

### 2. Make your change

Implement the GC modification, then rebuild:

```bash
cargo build --release
```

### 3. Run tests

Verify correctness is not broken:

```bash
cargo test --lib
cargo test --test harness_test
```

### 4. Compare against baseline

```bash
scripts/gc-bench.sh compare --runs 5
```

This runs the same benchmarks and reports percentage change for each
metric. Regressions above the threshold (default 5%) are flagged.

Example output:

```
--- bench-short-lived ---
  machine_ticks                  baseline=167037166    current=167037166    change=0.0%
  machine_allocs                 baseline=11178642     current=11178642     change=0.0%
  total_mark_time_secs           baseline=0.308        current=0.295       change=-4.2%
  total_sweep_time_secs          baseline=0.073        current=0.068       change=-6.8%
```

### 5. Investigate with Criterion if needed

If gc-bench.sh shows unexpected results, use Criterion for detailed
analysis:

```bash
# Run all GC benchmarks
cargo bench -- gc

# Run a specific benchmark group
cargo bench -- gc_alloc_then_collect

# Compare against saved Criterion baseline
cargo bench -- gc --save-baseline before
# ... make change, rebuild ...
cargo bench -- gc --baseline before
```

### 6. Include results in commit message

When committing GC changes, include a summary of benchmark results in
the commit message body. For example:

```
feat(eu-xxx): implement lazy sweeping

gc-bench.sh results (5 runs, median):
  bench-short-lived:    mark -12%, sweep -45%, ticks 0%
  bench-long-lived:     mark -8%, sweep -38%, ticks 0%
  bench-fragmentation:  mark -5%, sweep -42%, ticks 0%
```

## Options reference

### gc-bench.sh

```
Usage: scripts/gc-bench.sh {baseline|compare} [OPTIONS]

Options:
  --runs N         Number of runs per benchmark (default: 5)
  --heap-limit MIB Heap limit in MiB (default: no limit)
  --threshold PCT  Regression threshold percentage (default: 5)

Environment:
  EU_BIN           Path to eu binary (default: target/release/eu)
```

### Interpreting results

- **Deterministic metrics** (ticks, allocs) should show 0% change unless
  the GC change affects the allocation path or triggers collections at
  different points.
- **Timing metrics** (mark, sweep) naturally vary by a few percent
  between runs. Use `--runs 5` or higher for reliable medians.
- **collections_count** changes indicate the GC is triggering at
  different points, which may be expected for threshold or heuristic
  changes.
- **peak_heap_blocks** changes indicate different peak memory usage.

## Notes

- Always benchmark release builds — debug builds are 10-50x slower.
- The `--heap-limit-mib` flag forces a smaller heap, which can trigger
  more frequent collections. However, there is a known GC assertion
  issue under very small heap limits, so use this flag with caution.
- The baseline file (`gc-bench-baseline.json`) is git-ignored and
  local to each worktree.
