# W4: Resilient Parser Front-End & Error Recovery

**Bead:** eu-yhk0.2
**Target:** 0.8.0 (Phase 1), 0.9 (Phase 2)
**Roadmap ref:** W4 (Section 7, ROADMAP.md)

## Problem

The Rowan parser already builds lossless syntax trees with error nodes
(`ERROR_STOWAWAYS`) and collects multiple errors — it is already
recovery-capable internally. But an **all-or-nothing shim** discards
the tree on any error (`ParseResult::ok()` in `mod.rs:46` returns
`Err(errors)` if any errors exist), so a single typo turns the whole
file opaque to diagnostics and to every downstream pass. The LSP
syntactic features work by calling the parser directly
(`src/driver/lsp/mod.rs:805ff`); what is blocked is the pipeline.

## Current State (already good)

The parser implementation is already sophisticated:

- **18 error types** with helpful messages and hints (`error.rs`)
- **`ERROR_STOWAWAYS`** wraps erroneous tokens in the tree
- **Recovery patterns**: token consumption up to block/list boundaries
- **Two-phase**: parse builds tree + error list; validate walks tree
  for structural issues
- **LSP-friendly**: incomplete parses produce usable syntax trees

The gap is not the parser itself — it is the **pipeline's refusal to
use a tree that has errors**.

## Design

### Phase 1 (0.8): Surface the partial tree

**Delete the all-or-nothing shim.** Change `ParseResult` so
downstream consumers receive the tree *plus* accumulated errors,
rather than tree-or-errors:

```rust
// Current (mod.rs:46):
pub fn ok(self) -> Result<T, Vec<ParseError>> {
    if self.errors.is_empty() {
        Ok(self.tree())
    } else {
        Err(self.errors)
    }
}

// New: always provide the tree
pub fn into_parts(self) -> (T, Vec<ParseError>) {
    (self.tree(), self.errors)
}
```

**Pipeline changes:** Each downstream phase receives a tree that may
contain `ERROR_STOWAWAYS` nodes. The phases must:

1. **Desugarer**: skip `ERROR_STOWAWAYS` subtrees, emitting a
   diagnostic for each. Continue desugaring the valid portions.
2. **Cook/verify/typecheck**: operate on the partial core expression.
   Missing bindings from errored regions produce normal unbound-name
   diagnostics — this is acceptable and expected.
3. **LSP**: already handles partial trees for syntactic features;
   gains semantic features (hovers, completions) over the valid
   portions.

**~150 lines of change.** The main risk is that downstream phases
panic on unexpected tree shapes — the panic policy applies (every
panic becomes a diagnostic with a regression test).

### Phase 2 (0.9): Improved error recovery in the parser

Enhance the parser's own recovery to isolate malformed regions more
tightly:

1. **Block/list boundary synchronisation**: when an error occurs
   inside a block or list, consume tokens until the matching closing
   delimiter, wrapping the bad region in `ERROR_STOWAWAYS`. Resume
   normal parsing after the boundary. (Partially implemented already
   for apply-tuples at `parse.rs:552-575`; generalise.)

2. **Declaration-level recovery**: a malformed declaration head or
   missing colon should consume until the next declaration boundary
   (next unindented name, or closing brace), not abort the whole
   block.

3. **Typed error nodes**: replace the generic `ERROR_STOWAWAYS` with
   more specific error node kinds where possible (e.g.
   `ERROR_DECLARATION`, `ERROR_EXPRESSION`) so downstream phases can
   produce better diagnostics.

4. **Bracket recovery**: couples with W2's deferred block/soup
   decision — a mismatched bracket pair should be recoverable rather
   than cascading.

## Testing

### Phase 1
- A file with one syntax error still yields diagnostics and hovers
  for the rest of the file
- The partial tree always reaches the pipeline
- Downstream phases do not panic on partial trees (fuzz with W5)
- Error count and quality is at least as good as current

### Phase 2
- A malformed declaration in the middle of a file does not prevent
  diagnostics for surrounding declarations
- Block/list boundary recovery isolates errors to the containing
  construct
- The error node type is specific enough for useful diagnostics

## Scope Exclusions

- No changes to error message prose (that is Clarion's domain)
- No incremental re-parsing (that is W7)
- No new syntax — purely internal pipeline plumbing
