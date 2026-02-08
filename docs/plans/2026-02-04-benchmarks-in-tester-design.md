# Benchmarks in Tester — Design

## Problem

Eucalypt users have no way to measure the performance cost of their
code. The tester already captures VM metrics (ticks, allocs,
max-stack) in evidence, but these are buried in YAML and not surfaced
to the user.

## Solution

Extend the tester to recognise `bench-` prefixed targets, execute
them, and report deterministic VM metrics (ticks, allocs, max-stack)
directly to the user.

## Why VM Metrics, Not Wall-Clock Time

Ticks and allocs are deterministic and perfectly reproducible. They're
unaffected by system load, require no multiple iterations or warmup,
and directly measure algorithmic cost. Wall-clock timing is relevant
for runtime/GC engineering (covered by the GC Immix benchmarking
infrastructure) but not for users comparing eucalypt implementations.

## Benchmark Targets

Targets prefixed with `bench-` are recognised as benchmarks, mirroring
the existing `test-` prefix convention:

```eu
` { target: :bench-sort }
sort-benchmark: {
  data: range(1000) reverse
  result: data sort
}
```

A file can mix `test-` and `bench-` targets freely.

## Output

Stats printed alongside normal test results:

```
BENCH bench-sort (yaml)
  ticks:     12,340
  allocs:      891
  max-stack:    42
```

## Correctness Integration

If a bench target also has a `RESULT` key, it's validated as usual.
Stats are always reported regardless. Bench targets with `RESULT`
contribute to the overall pass/fail count. Bench targets without
`RESULT` are report-only and don't affect the summary.

## Implementation

### Test plan analysis (`testplan.rs`)

Recognise `bench-` prefixed targets alongside `test-` prefixed ones.
Include them in the test plan with a flag marking them as benchmarks.

### Execution (`tester.rs`)

No changes — bench targets execute exactly like any other target.
Statistics are already captured.

### Validation (`lib/test.eu`)

For bench targets, report stats. If `RESULT` is present, validate it
as normal. If absent, skip validation — the target is report-only.

### Output

After each bench target executes, print the stats block (ticks,
allocs, max-stack) to stderr. Normal PASS/FAIL output continues to
work for bench targets that have `RESULT`.

### HTML report (`lib/test.eu` report generation)

Include benchmark stats in the existing HTML report, so users get a
persistent record alongside test results.

### Migration of existing bench files

The 5 existing files in `harness/test/bench/` (`001_naive_fib.eu`
through `005_drop_cons.eu`) currently have no `bench-` prefixed
targets. They'd need their targets renamed to `bench-` prefix to opt
into the new reporting. This can be done incrementally.

## Out of Scope

- Wall-clock timing (covered by GC Immix benchmarking infrastructure)
- Baseline comparison / regression detection (separate concern)
- Multiple iterations / statistical analysis (unnecessary with
  deterministic metrics)
