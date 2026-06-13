# W4 Phase 2: Parser Error Recovery

- **Date:** 2026-06-13
- **Status:** Draft
- **Bead:** eu-kgsi.6
- **Roadmap:** W4 — Resilient parser front-end & error recovery (Phase 2)

---

## 1. Problem

Phase 1 (0.8) removed the all-or-nothing parse shim so partial trees reach
the pipeline. But the parser itself has minimal error recovery — a syntax
error in the middle of a block typically corrupts the rest of the file's
parse tree. The LSP produces no diagnostics or hovers for code after the
error point.

Some ad-hoc recovery exists (paren recovery at `parse.rs:552-575` consumes
unexpected tokens up to boundary delimiters), but it is not systematic.

## 2. Goal

Systematic error recovery synchronising on block (`{}`), list (`[]`), and
paren (`()`) boundaries, so a malformed region is isolated in an error node
and the rest of the file parses correctly. A file with one error still
yields diagnostics, hovers, and completions for all valid declarations.

## 3. Design

### 3.1 Synchronisation points

The parser synchronises at **balanced delimiter boundaries**:

- `{` / `}` — block boundaries (the most important: declarations are
  block-scoped)
- `[` / `]` — list boundaries
- `(` / `)` — parenthesised expression boundaries
- Custom bracket pairs (idiot brackets)

When an unexpected token is encountered inside a delimited construct, the
parser:

1. Wraps the unexpected tokens in an `ERROR_STOWAWAYS` node.
2. Consumes forward until a token that could plausibly continue the
   enclosing construct (a comma, closing delimiter, or declaration
   separator).
3. Resumes normal parsing.

When a closing delimiter is missing, the parser infers it at the next
higher-level boundary (e.g. a missing `}` is inferred at the end of the
enclosing block or at EOF).

### 3.2 Declaration-level recovery

Inside a block, declarations are separated by newlines or explicit
delimiters. When a declaration fails to parse:

1. The malformed declaration is wrapped in an `ERROR` node.
2. The parser advances to the next declaration boundary (next newline at
   the same indentation level, or the next `:` at block level).
3. Subsequent declarations parse normally.

This means a typo in one binding does not prevent other bindings in the
same block from being type-checked and surfaced in the LSP.

### 3.3 Error nodes in the Rowan tree

Add `ERROR` as a `SyntaxKind` (alongside the existing `ERROR_STOWAWAYS`
and `ERROR_RESERVED_CHAR`):

- `ERROR_STOWAWAYS` — unexpected tokens consumed during recovery
  (existing).
- `ERROR` — a complete syntactic construct that failed to parse, wrapping
  the tokens that were consumed before recovery.

The desugarer skips `ERROR` nodes, producing `Expr::default()` or omitting
the binding entirely. Downstream passes (cook, type-check) see only the
valid bindings.

### 3.4 Interaction with the LSP

The LSP already handles partial trees (Phase 1). Phase 2 improves the
quality of those trees:

- **More valid bindings** survive a local error → more diagnostics,
  more hovers, more completions.
- **Error nodes have spans** → the LSP can underline exactly the
  malformed region, not the rest of the file.
- **Incremental query (W7)** benefits because a local edit that
  introduces a parse error invalidates only the affected declaration's
  subtree, not the whole file's parse.

### 3.5 Interaction with bracket modes (W2)

Block-mode bracket pairs (idiot brackets) are decided during desugar, not
parse (this was the W2 fix in 0.7.1). Error recovery in the parser must
not assume block-vs-soup mode for custom brackets — it treats them as
opaque delimited regions and recovers at their boundaries.

## 4. Implementation sketch

### Phase 1: Block-level recovery
- In `parse_block_content` (or equivalent): when a declaration fails,
  wrap tokens in `ERROR`, advance to next declaration boundary, continue.
- Test: a block with one malformed and one valid declaration — the valid
  one parses correctly.

### Phase 2: Expression-level recovery
- Generalise the existing paren recovery to all delimiter pairs.
- On unmatched opening delimiter: infer close at the next higher boundary.
- On unexpected token in expression position: wrap in `ERROR_STOWAWAYS`,
  advance to next comma/delimiter/newline.

### Phase 3: Desugarer integration
- Skip `ERROR` nodes during desugaring.
- Emit a diagnostic for each `ERROR` node with its span.

### Phase 4: Validation
- Files with one error still produce diagnostics for valid declarations.
- Error spans are precise (not the rest of the file).
- No panics on any input (honour the panic policy).
- Existing harness tests unchanged.
- New tests with deliberately malformed files.

## 5. Test plan

- **Single-error block**: one malformed declaration in a block of five —
  the other four parse, desugar, and type-check correctly.
- **Missing closing brace**: `{ x: 1` — error localised, rest of file
  parses.
- **Missing closing paren**: `f(1, 2` — error localised at paren level.
- **Nested errors**: error inside a nested block — outer block recovers.
- **LSP integration**: open a file with an error, verify hovers and
  completions work for valid declarations.
- **No panics**: fuzz the parser (W5 Phase 3) with no panics.

## 6. Success criteria

- A file with one error yields diagnostics and hovers for all other
  declarations.
- Error spans are precise — they cover the malformed region, not the
  rest of the file.
- No regression on valid files.
- No panics on any input.
