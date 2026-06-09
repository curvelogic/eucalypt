# Fix: `eu dump ast` Shows LALRPOP AST Not Rowan AST

**Bead:** eu-yhk0.5
**Target:** 0.8.0

## Problem

`eu dump ast` shows empty output for files that parse correctly via
the Rowan parser. It appears to use the retired LALRPOP parser path,
which produces nothing for input the Rowan parser accepts. The Rowan
parser is the sole parser (ROADMAP.md Section 2 confirms "the LALRPOP
grammar is retired").

## Design

Route `eu dump ast` through the Rowan parser and display the lossless
syntax tree. Two display modes:

1. **Default:** Pretty-print the Rowan `SyntaxNode` tree showing node
   kinds, token text, and nesting — similar to rust-analyzer's
   `syntax_tree` debug output.

2. **`--debug-format`:** The Rust `Debug` representation of the
   `GreenNode` (full structure including all interned tokens).

## Implementation

**File:** `src/driver/eval.rs` (or wherever `dump ast` is dispatched).

Find the `dump ast` code path and replace the LALRPOP parse call with
the Rowan parser. Use the `SyntaxNode` debug display for output.

If any LALRPOP parser code is only reachable via `dump ast`, it can be
deleted (or noted for deletion).

Small change — likely under 50 lines.

## Testing

- `eu dump ast tests/harness/001_basic.eu` produces non-empty,
  meaningful output showing the syntax tree structure
- Output includes node kinds (`UNIT`, `BLOCK`, `DECLARATION`, etc.)
  and token text
- `--debug-format` produces the Rust Debug representation
- A harness test or manual verification that the output is useful for
  debugging parse issues
