# LSP Integration Test Harness

**Status**: Spec  
**Bead**: eu-51vu  
**Date**: 2026-05-04

## 1. Overview

An in-process test harness for LSP features using eucalypt scripts
as test definitions.  Tests are self-contained `.eu` files that
describe editing scenarios (open, change, complete, hover, etc.)
with optional expectations attached via the `//!` operator.

Supports a record-then-annotate workflow: capture real editing
sessions as action-only scripts, then add expectations to create
regression tests.

## 2. Test Script Format

Each test is a top-level declaration whose value is a list of
steps.  A step is a block describing an action.  Expectations are
attached to steps using `//!` with a predicate on the step result.

```eu
monad-tag-completion: [
  { open: "{ : }" },
  { complete: [0, 2] } //! _.items has-all([:for, :io, :let]),
  { change: "{ :for x: [1,2,3] }.(x)" },
  { hover: [0, 16] } //! _.content contains("number"),
  { change: "{ :for x: 42 }.(x)" },
  { diagnostics: true } //! _.warnings any?(_.message contains("expected [a]")),
]

no-crash-on-partial-input: [
  { open: "{ :" },
  { complete: [0, 3] },
  { change: "{ :f" },
  { complete: [0, 4] },
  { change: "{ :for " },
  { complete: [0, 7] },
]
```

### 2.1 Action Steps

| Action | Fields | Description |
|--------|--------|-------------|
| `open` | `string` | Open document with this content |
| `change` | `string` | Replace entire document content |
| `edit` | `{pos: [line, char], insert: string}` or `{range: [[l,c],[l,c]], text: string}` | Incremental edit |
| `complete` | `[line, char]` | Request completion at position |
| `hover` | `[line, char]` | Request hover at position |
| `diagnostics` | `true` | Request diagnostics for current document |
| `inlay-hints` | `true` or `{range: [[l,c],[l,c]]}` | Request inlay hints |
| `close` | `true` | Close the document |

### 2.2 Expectations

The `//!` operator attaches a predicate to the preceding step.
The predicate receives the step's result as `_`:

- **complete result**: `{ items: [string] }` — list of completion labels
- **hover result**: `{ content: string }` or `null`
- **diagnostics result**: `{ warnings: [{message: string, range: ...}], errors: [...] }`
- **inlay-hints result**: `{ hints: [{label: string, position: [line, char]}] }`
- **open/change/edit/close result**: `null` (mutation, no query result)

Steps without `//!` have no assertion — they just must not crash.

### 2.3 Multiple Tests Per File

Multiple top-level declarations define separate test cases.
Running with `-t test-name` executes a single test.  Running
without a target executes all.

## 3. Test Runner

### 3.1 Architecture

The test runner is a Rust integration test that:

1. Evaluates the `.eu` test script using the eucalypt evaluator
   (getting the step list as a JSON structure)
2. Creates an in-process LSP state (uses the existing LSP modules
   directly — parser, symbol table, type checker)
3. For each step:
   a. Executes the action (parse document, update state, call
      the appropriate LSP handler)
   b. If `//!` is attached, evaluates the predicate against
      the result
   c. Reports pass/fail

### 3.2 In-Process LSP Simulation

Reuses the existing modules:
- `parse_unit()` for document parsing
- `SymbolTable` for symbol resolution
- `completions()`, `hover()`, `inlay_hints()` for LSP responses
- Type checker for diagnostics

Does NOT use JSON-RPC transport or stdio — tests run against
the handler functions directly.

### 3.3 Prelude Handling

Tests run with the prelude loaded (so monad namespaces, type
annotations, etc. are available).  A `{ no-prelude: true }`
option on a test skips prelude loading for isolated tests.

## 4. Recording Proxy

### 4.1 Design

A simple stdio proxy that sits between editor and `eu --lsp`:

```
Editor <-> proxy <-> eu --lsp
```

The proxy logs client→server messages in eucalypt format:

```eu
recorded-session: [
  { open: "..." },
  { change: "..." },
  { complete: [0, 5] },
  { change: "..." },
  { hover: [2, 10] },
]
```

### 4.2 Implementation

A small standalone script (shell or Rust) that:
- Spawns `eu --lsp` as a subprocess
- Forwards JSON-RPC in both directions
- Extracts semantically meaningful actions from client→server
  messages (didOpen, didChange, completion, hover)
- Writes the eucalypt step list to a file on exit

### 4.3 Workflow

```bash
# Record
eu-lsp-record session.eu   # starts proxy, opens editor
# Edit normally, then quit

# Review
cat session.eu              # see what was captured

# Annotate — add //! expectations manually

# Run
cargo test --test lsp_harness test_session
```

## 5. Acceptance Criteria

1. A test script with `open` + `complete` + expectations runs
   and passes in-process
2. A test script with `change` + `hover` works
3. A test script with `diagnostics` expectations works
4. A test with no expectations passes (no crash = pass)
5. Incremental `edit` steps work
6. Multiple tests in one file run independently
7. The existing parser-crash scenario (`{ : }`) is covered
   by a test script
8. Tests run as part of `cargo test`
9. Recording proxy produces valid eucalypt output (stretch goal
   — can be deferred)

## 6. Files Changed

| File | Change |
|------|--------|
| `tests/lsp_harness.rs` | New integration test runner |
| `tests/lsp/` | Directory for `.eu` test scripts |
| `src/driver/lsp/testing.rs` | In-process LSP simulation helpers |
| `tools/eu-lsp-record` | Recording proxy (stretch goal) |

## 7. Dependencies

None for the core harness.  The recording proxy is independent
and can land later.

## 8. Out of Scope

- Full LSP protocol compliance testing (JSON-RPC framing, etc.)
- Editor-specific integration (Emacs/VS Code plugin tests)
- Performance benchmarking of LSP responses
