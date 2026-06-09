# W5 Phase 1: Conformance Corpus & GC CI Job

**Bead:** eu-yhk0.3
**Target:** 0.8.0
**Roadmap ref:** W5 Phase 1 (Section 7, ROADMAP.md)

## Problem

The 315+ harness tests are good but they don't pin **rendered output
bytes**. A breaking change to output (like the 0.6.2 `cond` rewrite)
is caught only if a test happens to assert on the affected value. And
GC verification (`EU_GC_VERIFY=2` + `EU_GC_POISON=1`) doesn't run on
the full harness in CI.

## Deliverables

### 1. Document the corpus contract

Write `tests/harness/CONFORMANCE.md` specifying the pass/fail
convention: the `.eu` test files plus their sidecars (`.expect` for
error tests, `.golden` for output tests) are the contract. Each
implementation brings its own runner. Keep the existing files as-is.

### 2. Golden-output `.golden` sidecars

Add a `.golden` file to a curated subset of harness tests covering:
- Every export format (JSON, YAML, TOML, text, HTML, EDN)
- Key prelude behaviours that must not silently change

**Harness plumbing.** Extend `tests/harness_test.rs` to:
- Look for `<test>.golden` alongside each test file
- If present, run the test and compare rendered output byte-for-byte
- Fail on mismatch with a diff
- A `--bless` mode (env var `BLESS=1`) regenerates `.golden` files
  for intentional changes

### 3. GC-verified CI job

Add a CI job on x86-64 that runs the full harness under
`EU_GC_VERIFY=2` + `EU_GC_STRESS=1` + `EU_GC_POISON=1`. This catches
GC correctness issues that only manifest under stress/verification.

## Testing

- Golden sidecars exist for representative tests across all export
  formats
- A deliberate output change is caught by `.golden` mismatch
- `BLESS=1` regenerates the golden files
- The GC-verified CI job runs green on the full harness

## Scope Exclusions

- Property tests (`proptest`) — deferred to 0.9
- Fuzz targets (`cargo-fuzz`) — deferred to 0.9–1.0
- ASAN as a CI gate — deferred
- Cross-backend runner — deferred until an alternative backend exists
