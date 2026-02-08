# Error Message Tests — Design

## Problem

The 24 error test files in `harness/test/errors/` have no validation.
They trigger errors but nothing checks that the right error was
produced. Error messages can silently regress.

## Solution

Sidecar `.expect` files alongside each error test, containing expected
exit code and/or stderr regex. The existing tester infrastructure
already captures exit code and stderr in evidence — this extends
validation to check them.

## Sidecar Format

For `006_div_by_zero.eu`, create `006_div_by_zero.eu.expect`:

```yaml
exit: 1
stderr: "division by zero"
```

- **`exit`** — integer, exact equality check against process exit code
- **`stderr`** — regex pattern, matched against the full stderr output
- Both fields optional; if present, both must pass
- If no sidecar exists, the test is treated as unvalidated (warning/skip)

This approach uses sidecar files for all error tests rather than
in-file metadata. This is necessary because some error tests
deliberately have broken syntax or malformed metadata — the parsing
failure is the thing being tested, so we cannot rely on reading
metadata from the file itself.

## Integration with Existing Tester

The tester pipeline is currently: load plan → run → generate evidence
→ validate → summarise. Error tests slot in with minimal changes.

### Test plan analysis (`testplan.rs`)

When analysing a file from `errors/`, check for a `.expect` sidecar.
If found, mark the test plan as an error test with the parsed
expectations.

### Execution (`tester.rs`)

Run the file exactly as now. The evidence already captures `exit`,
`stdout`, and `stderr`. No changes needed here.

### Validation (`lib/test.eu` or Rust-side)

For error tests, skip the normal `RESULT`-based validation. Instead:

- If `exit` specified: check `evidence.exit == expected`
- If `stderr` specified: regex match against joined stderr lines
- Pass only if all specified checks pass

### Harness runner (`harness_test.rs`)

Error tests get generated test functions just like normal tests, so
they appear in `cargo test` output individually.

### Missing sidecars

Error test files without a `.expect` sidecar are treated as
unvalidated — the test runs but is marked as a warning/skip rather
than a pass or fail. This allows incremental migration of the existing
24 error tests.

## Implementation Plan

### Phase 1: Sidecar parsing and validation logic

Extend `testplan.rs` to detect `.expect` files. Add error test
validation path in the tester that checks exit code equality and
stderr regex matching.

### Phase 2: Harness integration

Generate `cargo test` functions for error tests in `harness_test.rs`,
so error tests appear alongside normal harness tests.

### Phase 3: Write `.expect` sidecars for existing error tests

Migrate the ~20 active error tests (excluding `x`-prefixed disabled
ones). Start with a few known-good smoke tests to validate the
infrastructure:

- One parse error (e.g. `014_unterm_strlit.eu`)
- One runtime error (e.g. `006_div_by_zero.eu`)
- One assertion failure (e.g. `010_assert.eu`)

Then migrate the remainder.

## Interaction with Benchmarks (eu-01v)

Error tests and benchmark targets are orthogonal. `.expect` sidecars
apply only to files in `errors/`. `bench-` prefix targets apply only
to non-error test files. If both features are present on the same file,
`.expect` validation takes precedence (error tests exit early, making
benchmark stats meaningless).

## Out of Scope

- No changes to success test infrastructure
- No changes to the evidence format (it already captures everything
  needed)
- No HTML report changes (error tests show pass/fail like normal
  tests)
- No new metadata keys in `.eu` files
