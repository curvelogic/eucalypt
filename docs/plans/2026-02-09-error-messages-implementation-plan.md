# Error Messages Implementation Plan

**Parent epic**: eu-fti1 (Improve error messages and diagnostics)
**Date**: 2026-02-09

## Overview

This plan implements all recommendations from the error messages research
report. Work is organised into five phases with clear dependencies.
Each task includes testing requirements.

## Phase 1: Quick Wins (Independent — can all be done in parallel)

These tasks are small, self-contained, and can be assigned to separate
agents working concurrently. Each modifies different files with no
overlapping changes.

### 1A: Human-readable data tag names (P1.1)
**Existing bead**: eu-l7e1

Implement `Display` for `DataConstructor` in `src/eval/stg/tags.rs`
mapping tag numbers to user-friendly names:
- 0 → "null", 1 → "true", 2 → "false", 3 → "number",
  4 → "symbol", 5 → "string", 6 → "empty list", 7 → "list",
  8 → "block", 9 → "key-value pair", 10 → "key-value list",
  11 → "datetime"

Change `NoBranchForDataTag(u8)` in `src/eval/error.rs` to use
`DataConstructor` instead of raw `u8`, using `TryFrom<Tag>` conversion.
Update the error message format from "no branch for data tag 5" to
"type mismatch: unexpected string".

**Files**: `src/eval/stg/tags.rs`, `src/eval/error.rs`,
`src/eval/machine/vm.rs`

**Testing**:
- Update `harness/test/errors/002_lists.expect` with new message format
- Update `harness/test/errors/023_arg_types.expect`
- Add new test: `harness/test/errors/030_type_mismatch_num.eu` — pass
  number where string expected
- Add new test: `harness/test/errors/031_type_mismatch_str.eu` — pass
  string where number expected
- Verify all existing error tests still pass

### 1B: Recognise division by zero (P1.2)
**New bead**

In `src/eval/error.rs`, enhance the `Display` implementation for
`NumericRangeError` (or its diagnostic construction). When the error
operands show a zero divisor, produce "division by zero" instead of
"out of range error operating on numbers (x, 0)".

**Files**: `src/eval/error.rs`

**Testing**:
- Add `harness/test/errors/032_division_by_zero.eu` with `.expect`
- Verify existing arithmetic error tests still pass

### 1C: Clean up parse error formatting (P1.3)
**New bead**

Implement proper `Display` for Rowan parser error types so raw Rust
Debug format does not leak into user output:
- `UnclosedDoubleQuote` → "unterminated string literal"
- `EmptyExpression` → "empty expression where a value was expected"
- Review all other parser error variants for similar issues

Fix source location pointers in parse errors to point at the actual
error location rather than the start of the file.

**Files**: `src/syntax/error.rs`, Rowan parser error types

**Testing**:
- Update `harness/test/errors/003_unclosed_string.expect` (or create if
  missing)
- Add `harness/test/errors/033_empty_expression.eu` with `.expect`
- Add `harness/test/errors/034_parse_errors.eu` covering multiple parse
  error variants

### 1D: Remove Smid values from messages (P1.4)
**New bead**

Strip `[3700]`-style Smid references from the `Display` implementation
of `CompileError::FreeVar` in `src/eval/stg/compiler.rs`. The source
location pointer already conveys this information.

**Files**: `src/eval/stg/compiler.rs`

**Testing**:
- Update `harness/test/errors/001_undef.expect` to remove Smid reference
- Verify all compile error tests still pass

### 1E: Improve "not callable" message (P1.5)
**New bead**

Reword `NotCallable` error from "call of not callable" to "tried to
call a value that is not a function". Where possible, include the
actual type encountered (using `DataConstructor::fmt`).

**Files**: `src/eval/error.rs`, `src/eval/machine/vm.rs`

**Testing**:
- Update `harness/test/errors/017_too_many_args.expect`
- Add `harness/test/errors/035_not_callable.eu` exercising different
  non-callable types

### 1F: Drop environment trace from output
**New bead**

Remove the "environment trace:" note from error display in
`src/driver/eval.rs:213-216`. Retain the `env_trace()` machinery
internally (do not delete `annotation_trace()` or the `Traced` variant
fields) — just stop displaying the env trace to users.

**Files**: `src/driver/eval.rs`

**Testing**:
- Update ALL `.expect` files that reference "environment trace:" —
  these should no longer contain that line
- Verify no test regressions

---

## Phase 2: Trace Machinery Fixes (Sequential dependencies)

These tasks fix the trace pipeline so that user-defined functions appear
in stack traces. They must be done in order as each builds on the
previous.

### 2A: Use annotated_lambda for user functions (R1)
**New bead** — FOUNDATION for all trace improvements

Change `ProtoLambda::take_lambda_form` in `src/eval/stg/compiler.rs`
(lines 749-752) to use `dsl::annotated_lambda()` instead of
`dsl::lambda()`, passing `self.annotation`. This stores the user
function's Smid on the lambda form itself, matching the pattern used
by intrinsic wrappers.

This ensures that when a user function closure is entered,
`self.annotation = self.closure.annotation()` picks up the function's
Smid instead of `Smid::default()`.

**Files**: `src/eval/stg/compiler.rs`, `src/eval/stg/syntax.rs`
(verify `annotated_lambda` API)

**Testing**:
- Add `harness/test/errors/036_user_fn_in_trace.eu` — a user function
  that triggers an error; verify the function name appears in the stack
  trace
- Add `harness/test/errors/037_nested_fn_trace.eu` — nested user
  function calls triggering an error; verify multiple user function
  names appear
- Run full test suite to check for regressions (annotation changes
  could affect optimiser behaviour)

### 2B: Add annotation to ApplyTo continuations (R2)
**New bead** — depends on 2A

Extend `Continuation::ApplyTo` in `src/eval/machine/cont.rs` to carry
an `annotation: Smid` field. Set it from `self.annotation` when pushing
the continuation in `src/eval/machine/vm.rs:272`.

Update `stack_trace_iter` in `vm.rs` to include `ApplyTo` annotations
(currently skipped at line 607).

**Files**: `src/eval/machine/cont.rs`, `src/eval/machine/vm.rs`

**Testing**:
- Update the trace test cases from 2A to verify function application
  sites now appear in traces
- Verify GC scanning handles the new field (check if `Continuation`
  implements `Trace` or similar)
- Run benchmarks to confirm no significant performance regression from
  the extra Smid field

### 2C: Propagate Smid to intrinsic errors (P2.1)
**New bead** — independent of 2A/2B but logically in the same phase

Pass the annotation `Smid` from the machine state into intrinsic error
constructors in `src/eval/stg/support.rs` instead of `Smid::default()`.

The `MachineState` already tracks `annotation: Smid`. Thread this
through the `IntrinsicMachine` trait or pass it directly to
`ExecutionError` constructors (`NotEvaluatedNumber`,
`NotEvaluatedString`, `NotEvaluatedZdt`, etc.).

**Files**: `src/eval/stg/support.rs`, `src/eval/machine/intrinsic.rs`,
`src/eval/machine/vm.rs`, `src/eval/error.rs`

**Testing**:
- Verify that runtime errors now include source location pointers in
  diagnostic output
- Update `.expect` files for errors that gain source locations
- Add test: `harness/test/errors/038_runtime_source_loc.eu` — verify
  runtime error points to the source expression

### 2D: Map intrinsic names to user-facing names (P2.2)
**New bead** — independent of 2A/2B

Create a mapping from internal intrinsic names to user-facing names.
Add a `display_name: Option<&'static str>` field to intrinsic
registration in `src/eval/intrinsics.rs`.

Mappings:
- `LETTERS` → `str.letters`
- `ADD` → `+`
- `SUB` → `-`
- `MUL` → `*`
- `DIV` → `/`
- `LOOKUP` → `lookup`
- `SATURATED` → *(filter out — internal)*
- `AND` → *(filter out — internal)*
- `EMITx` → *(filter out — internal)*
- `RENDER` → *(filter out — internal)*
- etc.

Update `format_trace` in `src/common/sourcemap.rs` to use display
names and filter out internal-only entries.

**Files**: `src/eval/intrinsics.rs`, `src/common/sourcemap.rs`

**Testing**:
- Update all `.expect` files referencing intrinsic names in traces
- Add test verifying that user-facing names appear instead of internal
  names
- Add test verifying that internal machinery names are filtered out

### 2E: Source-level stack traces (P2.3)
**New bead** — depends on 2A, 2B, 2C, 2D

Enhance `format_trace` in `src/common/sourcemap.rs` to show source
file:line references where Smid values resolve to file locations.

Target format:
```
stack trace:
  - example.eu:5:3 (in 'outer')
  - example.eu:3:7 (in 'middle')
  - example.eu:1:10 (in 'inner')
```

**Files**: `src/common/sourcemap.rs`, `src/driver/eval.rs`

**Testing**:
- Update all `.expect` files with new trace format
- Add comprehensive trace test covering nested calls, higher-order
  functions, and prelude calls
- Verify traces are useful and not excessively long

---

## Phase 3: Type Error Improvements

### 3A: Expected vs actual type in type mismatches (P2.5)
**Existing bead**: eu-w4s0

For intrinsic type checking functions (`num_arg`, `str_arg`, etc. in
`src/eval/stg/support.rs`), detect the actual type when a value fails
the type check and produce structured error messages:

```
error: type mismatch in str.letters: expected string, found number
```

Requires knowing which intrinsic is being called (available via the
machine state) and the expected type (from the intrinsic's type
signature in the catalogue).

**Files**: `src/eval/stg/support.rs`, `src/eval/error.rs`,
`src/eval/intrinsics.rs`

**Testing**:
- Update existing type mismatch `.expect` files
- Add tests for each type combination (number→string, string→number,
  block→number, list→string, etc.)

### 3B: Convert panics to proper errors (P2.4)
**New bead**

Replace the 9 `panic!()` calls in `DataIterator` and
`StrListIterator` in `src/eval/stg/support.rs` with proper
`ExecutionError` results. This requires changing the iterator
interface to return `Result` items.

Panic messages like "Non-list data after force" and "bad cons cell"
should become structured `ExecutionError` variants with clear
user-facing messages.

**Files**: `src/eval/stg/support.rs`, `src/eval/error.rs`

**Testing**:
- Add tests that trigger each converted panic path (may require
  crafting specific malformed data)
- Verify that the process does not crash — it should produce a
  diagnostic instead

---

## Phase 4: Long-term Improvements

### 4A: Error codes and documentation (P3.1)
**New bead**

Assign stable error codes (`[EU-0001]` etc.) to each `ExecutionError`,
`CompileError`, and `CoreError` variant. Create a documentation page
explaining each error with examples.

Optionally add `eu explain EU-0001` subcommand.

**Files**: all error types, `src/driver/`, documentation

### 4B: "Did you mean?" suggestions (P3.2)
**New bead**

For lookup failures and free variable errors, use edit distance to
suggest similar names:

```
error: key 'upper' not found in block
help: similar keys: 'upper-case', 'to-upper'
```

**Files**: `src/eval/stg/block.rs`, `src/core/error.rs`

### 4C: Multi-label diagnostics (P3.3)
**New bead** — depends on 3A

For type mismatches, show both the definition site and use site with
separate labels on the diagnostic.

**Files**: `src/eval/error.rs`, `src/common/sourcemap.rs`

### 4D: Error recovery and multiple errors (P3.4)
**New bead**

Accumulate and report multiple errors for parse and compile phases
instead of stopping at the first error.

**Files**: `src/driver/prepare.rs`, `src/core/verify/`

### 4E: Structured JSON error output (P3.5)
**New bead**

Add `--error-format=json` flag for tooling integration.

**Files**: `src/driver/eval.rs`, `src/driver/options.rs`

---

## Phase 0: Test Infrastructure (Do first)

### 0A: Expand error test coverage
**New bead** — do this BEFORE other work begins

Audit `harness/test/errors/` and:
- Create `.expect` sidecar files for the 9 test files that lack them
- Add test cases for untested error conditions:
  - `NoBranchForDataTag` for each data type
  - Arithmetic type mismatches
  - Stack overflow / deep recursion
  - Arity mismatches
- Establish the baseline that all other tasks will update

**Files**: `harness/test/errors/`

**Testing**: This IS the testing task. Run `cargo test --test
harness_test` to verify all `.expect` files match.

---

## Dependency Graph

```
Phase 0:  [0A: Test infrastructure]
              |
Phase 1:  [1A] [1B] [1C] [1D] [1E] [1F]  ← all independent
              |
Phase 2:  [2A: annotated_lambda] → [2B: ApplyTo annotation]
              |                              |
          [2C: Smid propagation]    [2D: intrinsic names]
              |         |                    |
              +----+----+--------------------+
                   |
              [2E: source-level traces]
                   |
Phase 3:  [3A: expected vs actual] → [4C: multi-label]
          [3B: panics to errors]
                   |
Phase 4:  [4A: error codes]  [4B: did you mean?]
          [4D: multiple errors]  [4E: JSON output]
```

## Agent Team Structure

For maximum parallelism, this work can be split across agents:

- **Agent 1**: Phase 0 (test infrastructure), then Phase 1A + 1B
- **Agent 2**: Phase 1C + 1D + 1E
- **Agent 3**: Phase 1F, then Phase 2A → 2B
- **Agent 4**: Phase 2C + 2D

After Phase 1-2 converge, Phase 2E and Phase 3+ can proceed
sequentially or with 2 agents.

## Testing Strategy

Every task MUST:
1. Create or update `.expect` sidecar files for affected error messages
2. Run `cargo test --test harness_test` to verify error output matches
3. Run `cargo test` full suite to check for regressions
4. Run `cargo clippy --all-targets -- -D warnings` before completion

Error message changes are visible regressions if wrong — the `.expect`
file mechanism provides strong regression protection.
