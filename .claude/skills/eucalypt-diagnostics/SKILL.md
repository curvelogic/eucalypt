---
name: eucalypt-diagnostics
description: Working on eucalypt's error diagnostics and test infrastructure — ExecutionError and its rendering, source maps and Smids, error traces, test-expectation and debug intrinsics, lib/test.eu, the error harness and the diagnostics corpus. Use before changing, reviewing or auditing error messages, source locations, tests/harness/errors/ or tests/diagnostics/.
---

# eucalypt diagnostics and test infrastructure

## Scope

- `src/eval/error.rs` — `ExecutionError`, `to_diagnostic()`
- `src/common/sourcemap.rs` — `Smid`, `SourceMap`, `format_trace`
- `src/eval/stg/` — intrinsics for test expectations and debug functions
- `lib/prelude.eu`, `lib/test.eu` — test and debug library functions
- `tests/harness/errors/` — error test cases and `.expect` sidecars
- `tests/diagnostics/` — the diagnostics corpus and its invariant gate

## Read first

- `CLAUDE.md` — "Writing harness tests that gate", "Panics Are Critical",
  `--debug-trace` and `EU_ERROR_TRACE_DUMP`
- `docs/superpowers/diagnostics/PROTOCOL.md` — the five diagnostics
  invariants, the corpus and its `xfail` ratchet
- `docs/guide/testing.md` — how `lib/test.eu` computes a verdict
- `docs/reference/cli.md` — "Error Traces"

## In scope

- Putting real data into error messages
- Fixing missing or wrong source locations
- Converting panics to proper errors
- Reviewing an existing note or hint on its own merits, one at a time

## Not in scope

- Adding notes, hints or suggestions to error messages — never
- Rewording messages for style
- Bulk removal of notes

## Rules

- An audit is two-phase: report findings first and stop; implement only
  the items approved afterwards, one PR per fix.
- Every fix carries an error harness test, fault-injection verified
  (break the code, see it fail, restore, see it pass); say so in the PR.
- The diagnostics ratchet only tightens: never loosen an invariant or add
  an `xfail` to get green.
- Check diagnostics in both prelude modes (source, and blob via
  `scripts/blob-mode-tests.sh` / `scripts/diagnostics-gate.sh`).
- A PR from an open-ended "find things to improve" dispatch is one the
  owner reviews (`REVIEW.md`); directed work is reviewed normally.
- Work in a worktree branched from `master`, PR to `master`.
- Run the `CLAUDE.md` pre-commit checklist (`cargo fmt --all`, `cargo clippy
  --workspace --all-targets -- -D warnings`, `cargo test`) before every PR.
- The bead (`bd show <id>`) and any spec it references in
  `docs/superpowers/specs/` must be met in full — every phase and success
  criterion — before the work is called complete.
- Review and merge: a fresh `rig-review` against `REVIEW.md` on `master`.
