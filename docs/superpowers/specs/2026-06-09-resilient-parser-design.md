# W4 Phase 1: Surface the Partial Parse Tree

**Bead:** eu-yhk0.2
**Target:** 0.8.0

## Problem

The Rowan parser already builds lossless syntax trees with error nodes
(`ERROR_STOWAWAYS`) and collects multiple errors — it is
recovery-capable internally. But a shim layer in
`src/syntax/parser.rs` (lines 11-46) discards the tree on any error:

```rust
pub fn parse_unit(text: &str) -> Result<ast::Unit, ParserError> {
    let parse_result = rowan::parse_unit(text);
    if !parse_result.errors().is_empty() {
        return Err(ParserError::ParseErrors(parse_result.errors().clone()));
    }
    Ok(parse_result.tree())
}
```

A single parse error turns the whole file opaque to every downstream
phase. The LSP works around this by calling the Rowan parser directly
(`src/driver/lsp/mod.rs:766-769`), but the main pipeline
(desugaring → cooking → type-checking) gets nothing.

## Design

Replace the all-or-nothing shim with a pass-through that provides
the tree **and** the errors to all consumers.

### Step 1: Change the parser wrapper return type

**File:** `src/syntax/parser.rs`

Change `parse_unit()` and `parse_expression()` to return
`(T, Vec<ParseError>)` instead of `Result<T, ParserError>`:

```rust
pub fn parse_unit(text: &str) -> (ast::Unit, Vec<ParseError>) {
    let mut parse_result = rowan::parse_unit(text);
    parse_result.validate();
    (parse_result.tree(), parse_result.errors().clone())
}

pub fn parse_expression(text: &str) -> (ast::Soup, Vec<ParseError>) {
    let mut parse_result = rowan::parse_expr(text);
    parse_result.validate();
    (parse_result.tree(), parse_result.errors().clone())
}
```

### Step 2: Update call sites in the source loader

**File:** `src/driver/source.rs`

The `SourceLoader` calls `parser::parse_unit()` and
`parser::parse_expression()` to produce `Content` objects for the
desugarer. Currently it propagates the `Result` — on error, the whole
file fails.

Change to:
1. Accept the `(tree, errors)` pair.
2. Collect parse errors into a diagnostic accumulator (a new
   `Vec<ParseError>` field on `SourceLoader`, or passed through to
   the caller).
3. Wrap the tree in `Content` as before — even when errors exist.

Parse errors are reported as diagnostics but do **not** prevent
downstream phases from running on the valid portions of the tree.

### Step 3: Update the desugarer to handle ERROR_STOWAWAYS

**File:** `src/core/desugar/rowan_ast.rs`

The desugarer walks the Rowan AST via the typed `ast::` node wrappers.
When it encounters an `ERROR_STOWAWAYS` node (which has no typed
wrapper), it must:

1. **Skip the error subtree** — do not attempt to desugar it.
2. **Emit a placeholder** — insert an `Expr::ErrUnresolved` or
   similar sentinel so downstream phases see a gap rather than a
   panic.
3. **Record a diagnostic** — note the error region's span for
   reporting.

The key implementation point is `translate_block_entries()` (which
iterates declarations in a block) and `translate_soup()` (which
iterates expressions). Both must check for `ERROR_STOWAWAYS` in
their children and skip gracefully.

The typed AST node iterators (e.g. `unit.declarations()`,
`block.entries()`) already filter by `SyntaxKind`, so
`ERROR_STOWAWAYS` children are naturally excluded. The risk is in
**incomplete nodes** — a `DECLARATION` that is missing its body
because the body was consumed into an `ERROR_STOWAWAYS`. The
desugarer must handle `None` returns from child accessors (e.g.
`decl.body()` returning `None`) without panicking.

### Step 4: Update the WASM pipeline

**File:** `src/wasm_pipeline.rs`

Lines 120-164 call the parser and check `.errors().is_empty()`,
returning early on error. Change to the same pattern: collect errors
as diagnostics, continue with the tree.

### Step 5: Update the check pipeline

**File:** `src/driver/check.rs`

Lines 71-105 build the prelude interface. Parse errors here should
remain fatal (a broken prelude is unrecoverable). But user file
parsing (lines 275-310 in `type_check_path_with_seed()`) should
follow the new pattern.

### What does NOT change

- The Rowan parser itself (`src/syntax/rowan/parse.rs`) — no changes.
- The LSP path — it already uses `Parse<T>` directly and handles
  errors.
- Error message quality — no changes to `ParseError` display.
- The `Parse<T>` struct in `src/syntax/rowan/mod.rs` — the `ok()`
  method can stay for test convenience; production code just stops
  using it.

### Risk: downstream panics

The main risk is that downstream phases (cook, verify, typecheck)
encounter unexpected `None` values or missing subtrees and panic.
Mitigation:

1. Audit each phase for `.unwrap()` / `.expect()` calls on AST
   accessors.
2. Convert panics to graceful skips with diagnostics.
3. Every panic discovered becomes a regression test (per the panic
   policy).
4. Run the full harness with deliberately broken files to flush out
   panics.

## Acceptance Criteria

1. A file with one syntax error (e.g. a missing colon in a
   declaration) still produces type-check diagnostics and LSP hovers
   for the valid declarations in the same file.
2. The partial tree reaches the desugarer — verified by `eu dump
   desugared` producing output for the valid portions.
3. Parse errors are reported as diagnostics alongside any downstream
   diagnostics (type warnings, unresolved names from the error
   region).
4. No downstream phase panics on any file in the existing harness
   when a single token is deleted from the beginning, middle, or end.
5. Error count and locations are at least as good as the current
   all-or-nothing reporting.
6. The existing harness passes unchanged (no regression for valid
   files).
7. `eu check` on a file with parse errors reports the parse errors
   and any type warnings from the valid portions, with appropriate
   exit codes.
