# Fix: `eu dump ast` Shows Re-rendered Source, Not Syntax Tree

**Bead:** eu-yhk0.5
**Target:** 0.8.0

## Problem

`eu dump ast` does not show the AST. It parses the file with the
Rowan parser, then **re-renders** the parsed tree back to eucalypt
source via `express(&embedded)` (`src/driver/prepare.rs:237-247`).
The output is a pretty-printed reconstruction of the source — not the
syntax tree structure. For debugging parse issues, you need to see
the actual node kinds, nesting, and token spans.

The `--debug-format` flag (`prepare.rs:234`) prints the Rust `Debug`
representation of the embedded core expression — also not the Rowan
tree.

## Design

Add a display mode that prints the Rowan `SyntaxNode` tree directly,
showing node kinds, token text, and hierarchical nesting. This is the
primary useful output for `eu dump ast`.

### Output format

Indented tree showing `SyntaxKind` names, with tokens showing their
text. Similar to rust-analyzer's debug tree:

```
UNIT@0..42
  BLOCK@0..42
    DECLARATION@0..6
      DECLARATION_HEAD@0..1
        NAME@0..1 "x"
      COLON@1..2 ":"
      WHITESPACE@2..3 " "
      NUMBER@3..6 "42"
    WHITESPACE@6..7 "\n"
    DECLARATION@7..42
      ...
```

### Implementation

**File:** `src/driver/prepare.rs`

The `dump_ast` function (lines 222-248) currently has three modes:
- `opt.quote_embed()` → embed as eucalypt (the `--embed` flag)
- `opt.quote_debug()` → Rust Debug of embedded expression (`--debug-format`)
- default → pretty-print re-rendered source

Change the **default** mode to print the Rowan syntax tree. The
re-rendered source becomes the `--embed` output (which it semantically
is — an embedded eucalypt representation).

```rust
fn dump_ast(ast: &ParsedAst, opt: &EucalyptOptions) {
    match ast {
        ParsedAst::Unit(unit) => {
            if opt.quote_embed() {
                // Re-render as eucalypt source (existing behaviour)
                let embedded = export::embed_unit(unit);
                println!("{}\n", express(&embedded));
            } else if opt.quote_debug() {
                // Rust Debug of the Rowan SyntaxNode
                println!("{:#?}", unit.syntax());
            } else {
                // Default: indented syntax tree
                print_syntax_tree(unit.syntax(), 0);
            }
        }
        ParsedAst::Expr(soup) => {
            // Same pattern for expressions
            ...
        }
    }
}
```

**New function:** `print_syntax_tree(node: &SyntaxNode, indent: usize)`

A recursive function that walks the Rowan tree:
- For each `NodeOrToken::Node`: print the kind and text range, recurse
  with increased indent.
- For each `NodeOrToken::Token`: print the kind, range, and quoted
  text content.
- Use two-space indentation per level.

This is ~20 lines of code. The `SyntaxNode` API provides
`children_with_tokens()` which yields `NodeOrToken` elements, and
each has `.kind()`, `.text_range()`, and (for tokens) `.text()`.

**File:** `src/syntax/rowan/mod.rs` or a new
`src/syntax/rowan/display.rs`

The `print_syntax_tree` helper could live alongside the existing Rowan
infrastructure. It only depends on `rowan::SyntaxNode` and the
`SyntaxKind` enum.

### Error display

When the parsed tree contains `ERROR_STOWAWAYS` nodes, they appear in
the tree output naturally — no special handling needed. This makes
`eu dump ast` immediately useful for diagnosing parse errors (you can
see exactly where the error node sits in the tree).

## Acceptance Criteria

1. `eu dump ast tests/harness/001_basic.eu` prints an indented tree
   showing `UNIT`, `BLOCK`, `DECLARATION`, `NAME`, `COLON`, `NUMBER`
   etc. with text ranges and token content.
2. `eu dump ast --embed tests/harness/001_basic.eu` produces the
   previous default behaviour (re-rendered eucalypt source).
3. `eu dump ast --debug-format tests/harness/001_basic.eu` produces
   the Rust Debug representation of the Rowan `SyntaxNode`.
4. A file with parse errors shows `ERROR_STOWAWAYS` nodes in the tree
   at the correct position.
5. `eu dump ast -e '1 + 2'` works for expression input.
