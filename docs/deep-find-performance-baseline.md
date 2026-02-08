# Deep Find/Query Performance Baseline

**Date**: 2026-02-06
**Bead**: eu-d43z
**Platform**: macOS Darwin 24.6.0, release build

## Summary

The prelude-only implementation of deep-find and deep-query is
**usable for small-to-moderate data sizes** (up to ~100 top-level
blocks) with acceptable latency. For large data (1000+ top-level
blocks), VM execution time becomes significant, dominated by GC
pressure from intermediate allocations.

**Recommendation**: Rust intrinsics are NOT urgently needed. The
current prelude implementation is sufficient for typical eucalypt use
cases. If profiling of real workloads shows deep-find/query as a
bottleneck, Rust intrinsics can be pursued as an optimisation.

## Methodology

Two JSON fixtures were generated:

- **List fixture**: 2000 records in a JSON array (~1.15 MB). Tests
  `deep-find` which traverses lists.
- **Flat fixture**: 2500 service blocks as top-level keys (~0.95 MB).
  Tests both `deep-find` and `deep-query` on pure block structures.

All measurements use `eu -S` (statistics flag) on a release build.

## Results

### Compile pipeline overhead

The compile pipeline (parse, translate, cook, eliminate, etc.) is
constant regardless of the query operation. For ~1MB fixtures:

| Phase | Time |
|-------|------|
| Parse | ~120ms |
| Cook | ~190ms |
| Eliminate | ~110ms |
| STG compile | ~90ms |
| **Total pipeline** | **~500-700ms** |

This dominates wall-clock time for small queries on large data.

### VM execution time

| Operation | Data | VM Time | Ticks | Allocs |
|-----------|------|---------|-------|--------|
| Simple lookup (`data.total`) | 1.15MB list | 1ms | 215 | 25 |
| `deep-find-first("host", ...)` | 1.15MB list | 240ms | 8K | 5K |
| `deep-find-first("host", ...)` | 0.95MB flat | 2.0s | 19M | 53K |
| `deep-query-first("host", ...)` | 0.95MB flat | 1.3s | 19M | 53K |
| `deep-query-first("config.host", ...)` | 0.95MB flat | 91.5s | 393M | 3.9M |
| `deep-query-first("config.host", ...)` | 18KB (100 blocks) | 4ms | - | - |
| `deep-query-first("config.host", ...)` | 9KB (50 blocks) | 3ms | - | - |

### Inline benchmark (20 services)

The `bench/010_deep_find_perf.eu` harness test exercises deep-find,
deep-query with bare keys, dotted paths, and wildcards on 20 inline
service blocks:

- **Ticks**: 102,550
- **Allocs**: 12,738
- **Max stack**: 83

### Scaling characteristics

- `deep-find` scales roughly linearly with total block count
- `deep-query` with `**` expands to all descendants before matching —
  this is O(n * d) where n is block count and d is average depth
- The `descendants` expansion creates many intermediate list
  allocations, triggering GC pressure
- GC mark time dominates for large data sets (232ms of 240ms for
  deep-find-first on list fixture)

## Key findings

1. **GC dominates**: For deep-find on large data, GC mark time is
   >95% of VM time. The actual mutator work is minimal.

2. **Descendants expansion is expensive**: The `deep-query` `**`
   handler builds a complete list of all descendant blocks before
   matching the next segment. For 2500 services with 3-4 nesting
   levels, this creates ~15,000 intermediate block references.

3. **Practical size limit**: ~100 top-level blocks for complex
   patterns (`config.host`), ~500 for simple patterns (`host`).
   Beyond this, consider using targeted lookups instead.

4. **List traversal gap**: `deep-query` does not traverse JSON arrays
   (lists). Only `deep-find` correctly recurses into lists. This is a
   known design gap — `deep-query`'s `descendants` function only
   expands block children.

## Recommendations

### No immediate action needed

The prelude implementation is sufficient for eucalypt's typical use
case: processing configuration files and moderate-sized API responses.
These are rarely >100KB.

### If optimisation is pursued (Phase 3)

1. **Rust intrinsic for deep-find**: Would eliminate GC pressure by
   collecting results in Rust Vec before returning to the VM.

2. **Lazy descendants in deep-query**: Rather than expanding all
   descendants eagerly, use a lazy recursive traversal that can
   short-circuit on first match.

3. **Fix list traversal in deep-query**: The `descendants` function
   should recurse into lists, matching `deep-find`'s behaviour.
