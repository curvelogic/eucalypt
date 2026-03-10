# parse-as Design for Eucalypt

**Date:** 2026-03-10
**Status:** Approved
**Related:** eu-yost (IO monad design)

## Overview

Add a pure `parse-as` function — the inverse of `render-as` — that
converts a string containing structured data into eucalypt data
structures at runtime. This enables IO monad workflows where shell
command output (JSON, YAML, etc.) is parsed into blocks and lists for
further processing.

## Design Decisions

| Decision | Choice | Rationale |
|----------|--------|-----------|
| Purity | Pure function, not IO | Parsing has no side effects |
| API | `parse-as(fmt, str)` | Format-first for pipeline style: `s parse-as(:json)` |
| Auto-detect | No | Explicit format only; mirrors `render-as` |
| Error handling | Runtime error | Matches existing eucalypt error behaviour |
| Data only | Yes — no `!eu` evaluation | Security: parsing untrusted shell output must never execute code |
| Formats | All import formats | json, yaml, toml, csv, xml, edn, jsonl |
| Implementation | Reuse import parsers | Avoids duplicating format logic |

## API

```text
parse-as(fmt, str): __parse_string(fmt, str)
```

Supported format symbols: `:json`, `:yaml`, `:toml`, `:csv`, `:xml`,
`:edn`, `:jsonl`.

`:json` and `:yaml` share the same parser (as with imports).

## Data-Only Constraint

**Critical safety requirement.** The YAML import path currently
processes `!eu` tags, evaluating embedded eucalypt expressions. The
`parse-as` function MUST NOT do this — it parses untrusted strings
(e.g. shell command output) and must treat all content as inert data.

Implementation: add a `data_only: bool` parameter to the import
parsers. When `true`:

- YAML `!eu` tags produce plain string values, not eucalypt expressions
- Any other format-specific code execution paths are suppressed
- The resulting `RcExpr` contains only data literals (strings, numbers,
  blocks, lists, booleans, null)

## Intrinsic

New intrinsic `PARSE_STRING`:

| Name | Signature | Purpose |
|------|-----------|---------|
| `PARSE_STRING` | `(fmt_sym, str) → value` | Parse string as structured data |

Implementation steps within the intrinsic:

1. Extract format symbol and string from arguments
2. Call the appropriate import parser with `data_only: true`
3. Receive `RcExpr` (core AST containing only data literals)
4. Compile `RcExpr` to STG
5. Evaluate the compiled STG to produce a heap value
6. Return the heap value

This mirrors `RENDER_TO_STRING` (which does evaluate → render) but in
reverse (parse → compile → evaluate).

## Usage Examples

```text
# Parse JSON from shell output
config: { :io
  r: io.shell("kubectl get configmap foo -o json")
  _: io.check(r)
}.r.stdout parse-as(:json)

# Parse YAML
data: { :io
  r: io.shell("helm template mychart")
}.r.stdout parse-as(:yaml)

# Round-trip
original: {x: 1, y: 2}
same: render-as(original, :json) parse-as(:json)

# Pipeline style
names: { :io
  r: io.shell("curl https://api.example.com/users")
}.r.stdout parse-as(:json) map(_.name)
```

## Implementation Plan

### Task Breakdown

**P1. Data-only flag for import parsers**
- Files: `src/import/yaml.rs` (primary), potentially others
- Add `data_only: bool` parameter to parse functions
- When `true`, YAML `!eu` tags produce string literals
- Existing `read_to_core` call sites pass `false` (no behaviour change)
- Risk: Low. Mechanical addition to existing parsers.
- Agent: Furnace

**P2. PARSE_STRING intrinsic**
- Files: new intrinsic module, `src/eval/intrinsics.rs`
- Implement `StgIntrinsic` for `PARSE_STRING`
- Extract format symbol and string from STG arguments
- Call import parser with `data_only: true`
- Compile resulting `RcExpr` to STG and evaluate
- Register in INTRINSICS vector and `make_standard_runtime()`
- Risk: Medium-High. Requires compiling and evaluating an expression
  mid-evaluation, similar to `RENDER_TO_STRING` but in reverse.
  Must handle errors (invalid format, malformed input) gracefully.
- Depends on: P1
- Agent: Furnace

**P3. Prelude function**
- File: `lib/prelude.eu`
- Add `parse-as(fmt, str): __parse_string(fmt, str)`
- Depends on: P2
- Agent: Quill

**P4. Harness tests**
- Directory: `tests/harness/`
- Test parse-as for each supported format
- Test round-trip: `render-as` then `parse-as`
- Test error cases: invalid format symbol, malformed input
- Test data-only: YAML with `!eu` tags must not execute
- Register tests in `tests/harness_test.rs`
- Depends on: P3
- Agent: Quill

### Dependency Graph

```
P1 (data-only flag) → P2 (intrinsic) → P3 (prelude) → P4 (tests)
```

### Agent Allocation

| Task | Agent | Priority | Complexity |
|------|-------|----------|-----------|
| P1 | Furnace | First | Low |
| P2 | Furnace | Critical path | Medium-High |
| P3 | Quill | After P2 | Low |
| P4 | Quill | After P3 | Medium |
