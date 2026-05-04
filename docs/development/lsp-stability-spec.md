# LSP Server Stability Testing

**Status**: Spec  
**Bead**: eu-j4j1  
**Date**: 2026-05-04

## 1. Overview

Ensure the LSP server does not crash, panic, or hang on any input
a user might produce during normal editing.  The server is
single-threaded (sequential message processing), so the stability
concerns are not about data races but about robustness to
malformed/partial/pathological input.

Uses the LSP test harness (eu-51vu) as the testing mechanism.

## 2. Stability Threat Model

### 2.1 Partial input (mid-keystroke)

The document is invalid at almost every point during editing.
The parser must recover gracefully and the LSP handlers must
tolerate incomplete parse trees.

Key partial states to test:
- Empty document
- Single character (`{`, `(`, `:`, `` ` ``)
- Unclosed block (`{ x: 1`)
- Unclosed string (`"hello`)
- Incomplete declaration (`x:`)
- Bare colon in block (`{ : }`, `{ :f }`)
- Metadata without declaration (`` ` "doc" ``)
- Unclosed parenthesis (`f(x`)
- Operator without operands (`+`, `x +`)
- Incomplete import (`{ import: `)

### 2.2 Rapid edit sequences

Many didChange notifications in quick succession, simulating
fast typing.  Each state must be parseable without panic:

```eu
rapid-typing-block: [
  { open: "" },
  { change: "{" },
  { change: "{ " },
  { change: "{ x" },
  { change: "{ x:" },
  { change: "{ x: " },
  { change: "{ x: 1" },
  { change: "{ x: 1 " },
  { change: "{ x: 1 }" },
]
```

### 2.3 LSP requests on invalid state

Requesting completion, hover, or diagnostics while the document
is in a partial/invalid state:

```eu
completion-during-typing: [
  { open: "{ :" },
  { complete: [0, 3] },
  { change: "{ :f" },
  { complete: [0, 4] },
  { change: "{ :for " },
  { complete: [0, 7] },
  { change: "{ :for x" },
  { hover: [0, 7] },
]
```

### 2.4 Pathological input

- Very deeply nested blocks (`{a: {b: {c: ...}}}` 100+ levels)
- Very long lines (10KB+ single line)
- Very large number of declarations (1000+ in one block)
- Binary/garbage content (non-UTF8 handled at transport level,
  but random valid UTF8 should not panic)
- Unicode edge cases (emoji, ZWJ sequences, RTL marks in
  identifiers and strings)

### 2.5 Document lifecycle

- Open → immediate close (no edits)
- Open same URI twice
- Edit after close
- Requests for unknown URIs

## 3. Test Scripts

### 3.1 Keystroke-by-keystroke scripts

For common editing patterns, create test scripts that simulate
typing character by character, with LSP requests interspersed:

- Typing a block declaration from scratch
- Typing a monadic block (`{ :for x: [1,2,3] }.(x)`)
- Typing a function with type annotation
- Typing an import statement
- Deleting characters (backspace simulation via `edit`)

### 3.2 Fuzz-like scripts

Generated scripts that produce random valid/invalid intermediate
states.  A Rust helper generates random edit sequences:

```rust
fn generate_fuzz_script(seed: u64, num_steps: usize) -> String
```

These don't have expectations — passing means no panic.

### 3.3 Regression scripts

Each parser panic or LSP crash that's fixed gets a test script
that reproduces the original failure scenario.  The `{ : }` fix
(PR #673) becomes the first regression script.

## 4. Implementation

### 4.1 Test script structure

Each stability test is a `.eu` file in `tests/lsp/stability/`:

```eu
# No expectations needed — passing = no panic
bare-colon: [
  { open: "{ : }" },
  { complete: [0, 3] },
  { hover: [0, 3] },
  { diagnostics: true },
  { inlay-hints: true },
]
```

### 4.2 Panic catching

The test runner wraps each step in `std::panic::catch_unwind`
so that a panic on one step is reported as a test failure with
the step index and action, rather than aborting the entire test
suite.

### 4.3 Timeout

Each step has a timeout (default 5s).  If a step takes longer,
it's reported as a hang.  This catches infinite loops in the
parser or type checker on pathological input.

## 5. Acceptance Criteria

1. All partial-input states from section 2.1 pass without panic
2. Rapid edit sequence (typing a monadic block character by
   character) passes without panic
3. LSP requests (complete, hover, diagnostics, inlay-hints) on
   every intermediate state pass without panic
4. Deeply nested input (100 levels) doesn't stack overflow
5. The `{ : }` regression is covered
6. Fuzz-generated scripts (100 random sequences of 50 steps)
   pass without panic
7. Document lifecycle edge cases (double open, edit after close)
   don't panic

## 6. Files Changed

| File | Change |
|------|--------|
| `tests/lsp/stability/` | Test script `.eu` files |
| `tests/lsp_harness.rs` | Panic-catching and timeout support |

## 7. Dependencies

- eu-51vu (LSP test harness) — provides the test runner

## 8. Out of Scope

- Performance testing (response time SLAs)
- Memory leak detection
- Multi-document scenarios (cross-file references)
- True concurrency testing (server is single-threaded)
