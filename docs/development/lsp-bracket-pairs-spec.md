# LSP: First-Class Idiot Bracket Pair Support

**Status**: Spec  
**Bead**: eu-gduc  
**Date**: 2026-05-04

## 1. Overview

Make idiot bracket pairs (user-defined bracket syntax like `⟦ ⟧`,
`‹ ›`, `⌈ ⌉`, `⌊ ⌋`) first-class citizens in the LSP experience.
Currently the LSP recognises bracket pair declarations in the
symbol table and document outline but provides no editing support
for bracket pair *uses*.

## 2. Current State

| Feature | Bracket pairs | Regular blocks |
|---------|--------------|----------------|
| Semantic tokens | None | Properties, functions highlighted |
| Hover | None | Name shows type/doc |
| Completion | None | Field completion, monad tags |
| Inlay hints | None | Operator fixity, monadic binding types |
| Go-to-definition | None | Jumps to declaration |
| Document symbols | Declaration only | All declarations |
| Diagnostics | Unclosed/mismatched | Full |

## 3. Proposed Features

### 3.1 Semantic tokens for bracket delimiters

Highlight bracket open/close characters with a distinct token
type.  The bracket characters (`⟦`, `⟧`, `‹`, `›`, `⌈`, `⌉`,
`⌊`, `⌋`) should use a custom semantic token type (e.g.
`bracket` or reuse `operator`) with a modifier indicating the
pair name.

Also highlight content differently based on mode:
- **Expression mode** (`⟦ expr ⟧`): content highlighted as
  expression
- **Block mode** (`⟦ x: 1, y: 2 ⟧`): content highlighted as
  block declarations (properties, etc.)

### 3.2 Hover on bracket delimiters

Hovering on `⟦` or `⟧` shows:
- The bracket pair name
- The pair definition's doc metadata (if any)
- Whether it's expression-mode or block-mode
- The monad info if it has `monad:` metadata (e.g. "lens
  traversal — each binding is a lens step")

### 3.3 Completion inside bracket blocks

For block-mode bracket pairs, completion inside the bracket
body should work the same as regular blocks:
- Property/function declaration patterns
- Monad tag completion (`:for`, etc.) if the pair is monadic

### 3.4 Inlay hints inside bracket expressions

- For monadic bracket pairs: show binding type hints (same as
  `{ :for x: ... }` blocks)
- For lens brackets `‹ :field 0 :other ›`: show the lens type
  at each step

### 3.5 Go-to-definition on bracket delimiters

Clicking go-to-definition on `⟦` or `⟧` should jump to the
bracket pair definition (`⟦{}⟧: ...`).

### 3.6 Go-to-definition on bracket content

Inside a block-mode bracket pair, go-to-definition on binding
names should resolve to the bracket pair's monad namespace
(e.g. from a binding in `⟦ x: ... ⟧` to the monad's `bind`
function).

### 3.7 Bracket pair matching

Report matching bracket pairs to the editor so it can
highlight matching open/close brackets.  This uses
`textDocument/documentHighlight` or editor-specific bracket
matching.

## 4. Implementation Priority

### Phase 1 (0.6.1)

1. **Semantic tokens** for bracket delimiters — distinct
   highlighting
2. **Hover** on bracket delimiters — pair name, doc, mode
3. **Go-to-definition** on delimiters → pair definition

### Phase 2 (future)

4. Completion inside bracket blocks
5. Inlay hints inside bracket expressions
6. Go-to-definition on bracket content
7. Bracket pair matching

Phase 1 is the minimum for brackets to feel like first-class
citizens.  Phase 2 builds on the monadic block and import
resolution work.

## 5. Implementation Notes

### Identifying bracket pairs in the AST

The Rowan parser produces `BRACKET_OPEN`, `BRACKET_CLOSE`,
`BRACKET_BLOCK`, and `BRACKET_EXPRESSION` nodes.  The LSP
needs to:

1. Find the `BRACKET_OPEN` token and extract the bracket
   character
2. Look up which bracket pair definition it belongs to (via
   the bracket registry or symbol table)
3. Determine the mode (expression vs block) from the AST node
   type

### Symbol table extension

Add bracket pair definitions to the symbol table with their:
- Open/close characters
- Mode (expression/block)
- Monad metadata (if any)
- Documentation
- Definition location (for go-to-definition)

## 6. Acceptance Criteria

### Phase 1

1. `⟦` and `⟧` in source code get semantic token highlighting
2. Hovering on `⟦` shows the pair name and documentation
3. Hovering on `‹` shows "lens bracket" with lens documentation
4. Go-to-definition on `⟦` jumps to the `⟦{}⟧:` definition
5. Block-mode and expression-mode brackets are distinguished in
   hover info
6. Unknown bracket characters (no definition) show appropriate
   error/warning
7. No crash on bracket pairs in partial/incomplete expressions

### Integration

8. All existing harness tests pass
9. LSP stability tests cover bracket pair scenarios

## 7. Files Changed

| File | Change |
|------|--------|
| `src/driver/lsp/semantic.rs` | Semantic tokens for bracket delimiters |
| `src/driver/lsp/hover.rs` | Hover on bracket delimiters |
| `src/driver/lsp/navigation.rs` | Go-to-definition for bracket delimiters |
| `src/driver/lsp/symbol_table.rs` | Track bracket pair definitions with metadata |

## 8. Dependencies

None for Phase 1.  Phase 2 depends on eu-z9zz.10 (monadic
binding hints) and eu-tiyv (import resolution).
