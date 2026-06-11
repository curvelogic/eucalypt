# W5 Phase 1: Golden Output Conformance & GC CI Job

**Bead:** eu-yhk0.3
**Target:** 0.8.0

## Problem

The 315+ harness tests verify behaviour via eucalypt's own assertion
operators (`//=`/`//!`/`//=>`), but they evaluate to blocks of
booleans — they don't pin **rendered output bytes**. A change that
alters output formatting, key ordering, or prelude function behaviour
is only caught if a test happens to assert on the specific affected
value. The 0.6.2 `cond` rewrite — a silent breaking change — is the
motivating example.

Separately, GC verification (`EU_GC_VERIFY=2` + `EU_GC_POISON=1`)
runs on the aarch64 CI job with `EU_GC_STRESS=1`, but there is no
job that runs the full harness under all three combined on x86-64.

## Design

### Conformance files and the `:conform` target

Conformance files are **separate from harness tests**. They live in
`tests/conform/` (not `tests/harness/`) and are not processed by the
existing test runner.

Each conformance file is a normal `.eu` file with a single
`` ` :conform `` target that produces the rendered output to pin:

```eu,notest
# tests/conform/arithmetic.eu

` { target: :conform, format: :yaml }
conform: {
  sum: 1 + 2
  difference: 5 - 3
  product: 4 * 7
  quotient: 10 / 3
}
```

The `:conform` target can specify any output format via `format:`
metadata (`:yaml`, `:json`, `:toml`, `:text`, `:html`, `:edn`).
Default is `:yaml`.

The golden sidecar `tests/conform/arithmetic.eu.golden` contains the
exact expected output:

```yaml
sum: 3
difference: 2
product: 28
quotient: 3
```

### Conformance runner

**File:** `tests/conform_test.rs` (new)

A separate test file, independent of `tests/harness_test.rs`. It
discovers all `.eu` files in `tests/conform/` and generates a test
for each.

For each conformance file:

1. Build the `eu` binary path (same pattern as
   `harness_test.rs:error_test_with_binary()`).
2. Run `eu <file> -t conform` as a subprocess, capturing stdout.
3. Look for `<file>.golden` sidecar.
4. If `BLESS=1` is set, write stdout to the golden file and pass.
5. If the golden file does not exist, fail with a message saying
   the golden file is missing (conformance files must have sidecars).
6. Compare stdout against golden file byte-for-byte.
7. On mismatch, fail with a unified diff.

Test discovery uses `std::fs::read_dir("tests/conform/")` filtered
to `.eu` files, sorted by name. Each becomes a `#[test]` function
via a macro or `test_case`-style generation (same pattern as the
harness test file).

### Diff output on mismatch

On mismatch, print a unified diff. Use the `similar` crate
(`similar::TextDiff`) which is lightweight and already used in Rust
test tooling:

```rust
let diff = similar::TextDiff::from_lines(&golden, &actual);
eprintln!("{}", diff.unified_diff()
    .header("expected (golden)", "actual"));
```

Add `similar` as a dev-dependency in `Cargo.toml`.

### Which conformance files to create

Start with ~20-30 files covering:

**Export formats** (one file per format):
- `yaml_output.eu` — representative YAML rendering
- `json_output.eu` — JSON rendering (key ordering, escaping)
- `toml_output.eu` — TOML rendering
- `text_output.eu` — plain text rendering
- `html_output.eu` — HTML/markup rendering
- `edn_output.eu` — EDN rendering

**Core prelude behaviour**:
- `arithmetic.eu` — basic numeric operations
- `list_ops.eu` — map, filter, fold, zip, etc.
- `block_ops.eu` — merge, deep-merge, lookup, keys
- `string_ops.eu` — interpolation, split, join, replace
- `conditionals.eu` — if, when, cond/=> forms
- `sort_order.eu` — sort-by, sort-nums, key ordering

**Merge semantics** (the most breakage-prone area):
- `shallow_merge.eu` — block merge precedence
- `deep_merge.eu` — deep-merge recursive behaviour
- `merge_with_metadata.eu` — merge + metadata interaction

**Edge cases that have broken before**:
- `cond_clause.eu` — the `cond[…]/=>` form (0.6.2 breakage)
- `operator_precedence.eu` — catenation vs infix

Golden files are generated initially by `BLESS=1 cargo test
--test conform_test` and reviewed before committing.

### Conformance contract document

**File:** `tests/conform/CONFORMANCE.md`

A short document (~1 page) specifying:
- Conformance files in `tests/conform/` plus their `.golden` sidecars
  are the output-stability contract.
- Each file has exactly one `:conform` target.
- The golden file contains the exact expected byte output.
- `BLESS=1` regenerates golden files for intentional changes.
- Each implementation can run the same files with its own runner.

### GC-verified CI job

**File:** `.github/workflows/build-rust.yaml`

Add a new job:

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

The existing `test-aarch64-sequential` job runs with
`EU_GC_STRESS=1`; this new job adds verification and poison on
x86-64.

## Acceptance Criteria

1. `tests/conform/` directory exists with at least 20 conformance
   files and their `.golden` sidecars.
2. `tests/conform/CONFORMANCE.md` documents the contract.
3. `cargo test --test conform_test` discovers and runs all
   conformance files, comparing against golden sidecars.
4. A mismatch produces a clear unified diff showing expected vs
   actual.
5. A missing `.golden` file for a conformance `.eu` file fails the
   test (not silently skipped).
6. `BLESS=1 cargo test --test conform_test` regenerates all golden
   files from current output.
7. A deliberate change to prelude output (e.g. altering a function)
   causes golden mismatches in the affected conformance files.
8. The `test-gc-verified` CI job runs green on the full harness.
9. The existing harness tests are unaffected — conformance tests are
   fully independent.

## Scope Exclusions

- Property tests (`proptest`) — deferred to 0.9.
- Fuzz targets (`cargo-fuzz`) — deferred to 0.9–1.0.
- ASAN as a CI gate — deferred.
- Cross-backend runner — deferred until an alternative backend exists.
- No changes to the existing harness test infrastructure.
