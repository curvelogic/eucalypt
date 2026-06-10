# W5 Phase 1: Golden Output Sidecars & GC CI Job

**Bead:** eu-yhk0.3
**Target:** 0.8.0

## Problem

The 315+ harness tests verify behaviour via eucalypt's own
`//=`/`//!`/`//=>` assertion operators, but they don't pin **rendered
output bytes**. A change that alters output formatting, key ordering,
or prelude function behaviour is only caught if a test happens to
assert on the specific affected value. The 0.6.2 `cond` rewrite —
a silent breaking change — is the motivating example.

Separately, GC verification (`EU_GC_VERIFY=2` + `EU_GC_POISON=1`)
runs on the aarch64 CI job with `EU_GC_STRESS=1`, but there is no
job that runs the full harness under verification + poison + stress
combined on x86-64.

## Deliverables

### 1. Conformance contract document

**File:** `tests/harness/CONFORMANCE.md`

A short document (~1 page) specifying:
- The `.eu` test files plus their sidecars (`.expect` for error
  behaviour, `.golden` for output) are the conformance contract.
- The pass/fail convention: the existing `//=`/`//!` assertion
  operators and the sidecar matching rules.
- Each implementation brings its own runner. The files are the
  contract, not the runner.

### 2. Golden-output `.golden` sidecars

#### Sidecar format

A `.golden` file sits alongside the test file:
`tests/harness/001_basic.eu.golden`. It contains the exact expected
rendered output of running that test, byte-for-byte.

For tests with multiple targets or formats, the golden file contains
the default target's YAML output (the most common case). Tests that
exercise specific formats use a format suffix:
`tests/harness/042_json.eu.golden.json`.

#### Harness changes

**File:** `tests/harness_test.rs`

Currently, normal tests call `tester::test(opt)` (line 54-57) which
runs the full in-process test pipeline (plan → execute → validate →
summary). Golden file checking is a **separate, additional check**
that runs alongside the existing assertion-based validation.

Add a function `check_golden(test_path: &Path, opt: &EucalyptOptions)`:

1. Construct the golden file path: `{test_path}.golden`.
2. If the golden file does not exist, return (no golden check for
   this test — opt-in per test).
3. Run the evaluand with the test's default target and format,
   capturing stdout to a `String`.
4. If `BLESS=1` is set in the environment, write stdout to the
   golden file and return (regeneration mode).
5. Read the golden file contents.
6. Compare byte-for-byte. On mismatch, fail the test with a unified
   diff (use the `similar` or `difference` crate, or a simple
   line-by-line comparison).

Call `check_golden()` from each `run_test()` invocation, after the
existing `tester::test()` call succeeds.

**File:** `src/driver/tester.rs`

The golden check needs to capture rendered output. Currently
`InProcessTester::run()` (lines 618-688) captures stdout/stderr into
temp files referenced by the evidence YAML. The simplest approach is
to **not modify tester.rs** — instead, `check_golden()` in
`harness_test.rs` runs the evaluand directly via a subprocess call
(`eu <file> -x yaml`) and captures stdout, similar to how
`error_test_with_binary()` (lines 51-106) works.

This keeps the golden check independent of the in-process test
runner.

#### Which tests get golden files

Start with a curated subset (~20-30 tests) covering:

- **All export formats**: at least one test each for YAML, JSON,
  TOML, text, HTML, EDN output.
- **Core prelude behaviour**: basic arithmetic, list operations,
  block operations, string functions, merge semantics.
- **Edge cases that have broken before**: the `cond`/`=>` form,
  deep-merge behaviour, operator precedence.

The golden files are generated initially by running `BLESS=1 cargo
test` and reviewing the output.

### 3. GC-verified CI job

**File:** `.github/workflows/build-rust.yaml`

Add a new job `test-gc-verified` that runs on `ubuntu-latest`:

```yaml
test-gc-verified:
  runs-on: ubuntu-latest
  steps:
    - uses: actions/checkout@v4
    - uses: dtolnay/rust-toolchain@stable
    - run: cargo build --release
    - name: Run harness under GC verification
      run: |
        ./target/release/eu test --allow-io tests/harness
      env:
        EU_GC_VERIFY: "2"
        EU_GC_STRESS: "1"
        EU_GC_POISON: "1"
```

This catches GC bugs that only manifest under stress (forced
evacuation every cycle) combined with verification (structural
integrity checks after every collection) and poison (use-after-free
detection).

The existing `test-aarch64-sequential` job already runs with
`EU_GC_STRESS=1`; this new job adds the verification and poison
dimensions on x86-64.

## Acceptance Criteria

1. `tests/harness/CONFORMANCE.md` exists and documents the contract.
2. At least 20 tests have `.golden` sidecars covering all six export
   formats (YAML, JSON, TOML, text, HTML, EDN).
3. `cargo test test_harness_001` compares output against
   `001_basic.eu.golden` and fails on mismatch with a diff.
4. `BLESS=1 cargo test test_harness_001` regenerates the golden file.
5. A deliberate change to prelude output (e.g. temporarily altering
   a prelude function) causes golden mismatches in the affected tests.
6. The `test-gc-verified` CI job runs green on the full harness.
7. The existing harness passes unchanged — golden checks are additive.
