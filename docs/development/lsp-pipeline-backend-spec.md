# LSP: SourceLoader Pipeline as Primary Backend

**Status**: Spec  
**Bead**: eu-kqut  
**Date**: 2026-05-10

## 1. Overview

Refactor the LSP server to use the SourceLoader pipeline as its
primary semantic backend instead of duplicating language semantics
at the AST level.

Currently the LSP maintains two parallel representations: an
AST-level symbol table for hover/completion/go-to-definition, and
a core-level pipeline (via SourceLoader) only for type checking
diagnostics.  The AST-level approach duplicates import resolution,
metadata normalisation, and other semantic processing that the
desugarer already handles correctly.

After this change, the SourceLoader pipeline runs in the background
on every meaningful document change, and its artefacts (source map,
type environment, resolved imports) are the authoritative source
for semantic LSP features.

## 2. ServerState Redesign

```rust
pub struct ServerState {
    store: DocumentStore,
    prelude_table: SymbolTable,

    /// Cached pipeline result from the last successful run.
    cached: Option<CachedPipeline>,

    /// Green node from the last parse, for change detection.
    last_green: Option<(Url, GreenNode)>,

    /// Channel for receiving pipeline results from the
    /// background thread.
    pipeline_rx: mpsc::Receiver<PipelineResult>,

    /// Sender for cancelling in-flight pipeline runs.
    cancel: Option<Arc<AtomicBool>>,
}

struct CachedPipeline {
    uri: Url,
    loader: SourceLoader,
    type_env: HashMap<String, Type>,
    warnings: Vec<TypeWarning>,
}
```

## 3. Document Change Flow

### 3.1 On didOpen / didChange

1. Store the new text in `DocumentStore`
2. Re-parse to get a new `GreenNode`
3. Compare with `last_green` — if identical, stop (no semantic
   change occurred)
4. Update `last_green`
5. Publish parse-error diagnostics immediately (from parse errors)
6. Cancel any in-flight background pipeline run
7. Spawn a background thread:
   - Sleep 300ms (debounce)
   - Check cancel flag — if set, exit
   - Run the full SourceLoader pipeline
   - Send `PipelineResult` via mpsc channel

### 3.2 Main loop integration

The main loop currently blocks on `connection.receiver`.  After
this change, it also checks `pipeline_rx` with `try_recv` after
processing each LSP message:

```rust
for msg in &connection.receiver {
    handle_message(connection, &mut state, msg)?;
    // Check for pipeline results
    while let Ok(result) = state.pipeline_rx.try_recv() {
        state.apply_pipeline_result(connection, result)?;
    }
}
```

When a pipeline result arrives:
- Swap `cached` to the new `CachedPipeline`
- Publish type diagnostics from the new warnings
- At most two SourceLoaders exist at any time: the current
  cached one and one being built in the background

### 3.3 Change detection

Compare `GreenNode` equality after re-parse.  Rowan's `GreenNode`
implements `Eq` via structural comparison of the interned tree.
If the green nodes are equal, the document's semantic structure
hasn't changed and the pipeline need not re-run.

## 4. New Locator Variant

```rust
pub enum Locator {
    // ... existing variants ...
    /// In-memory buffer with a filesystem path for source map
    /// registration.  Used by the LSP to feed document content
    /// from the editor buffer without requiring a save to disk.
    Buffer { path: PathBuf, text: String },
}
```

The SourceLoader reads the text from the `Buffer` locator
directly instead of from disk, but registers the path in the
source map so error locations and go-to-definition point to the
correct file.

This also fixes the current oversight where type checking is
silently skipped for unsaved buffers.

## 5. Pipeline Execution

The background thread runs:

1. Create a fresh `SourceLoader`
2. Load the prelude: `loader.load(&prelude_input)` then
   `loader.translate(&prelude_input)`
3. Load the primary document using `Locator::Buffer { path, text }`
   where `text` comes from the DocumentStore snapshot and `path`
   from the document URI
4. `loader.translate(&doc_input)` — desugars, resolves imports
   (imported files are read from disk via their normal
   `Locator::Fs` paths)
5. `loader.merge_units(&inputs)`
6. `loader.cook()`
7. `loader.eliminate()`
8. Run the type checker on `loader.core().expr`
9. Send `PipelineResult { uri, loader, type_env, warnings }`
   via mpsc channel

If any step fails, send an error result so diagnostics can
report it.  The previous `CachedPipeline` remains valid until
a successful result replaces it.

## 6. Request Handler Changes

### Always AST-based (must work on broken input)

| Feature | Source |
|---------|--------|
| Parse diagnostics | Latest parse |
| Semantic tokens | AST |
| Folding ranges | AST |
| Formatting | AST |
| Selection ranges | AST |
| Document symbols | AST |

### Use cached pipeline when available

| Feature | Pipeline data | Fallback |
|---------|--------------|----------|
| Hover | `type_env` for types | AST symbol table for name/doc |
| Completion | `type_env` for record fields | AST for name completion |
| Inlay hints | `type_env` | None (absent until pipeline runs) |
| Type diagnostics | `warnings` | None |
| Go-to-definition (local) | AST symbol table | — |
| Go-to-definition (imports) | `loader.source_map()` | Not available |

Absent cache means absent data, not wrong data.

### Removed

- AST-level import scraping (`import_inputs` in context.rs)
- AST-level import symbol loading in `build_symbol_table` and
  `LspTestSession`

The `lsp-context` mechanism is also removed — it was never
connected to the server before PR #679, and import resolution
via the pipeline supersedes it.

## 7. Test Harness Changes

`LspTestSession` mirrors the real server architecture:

- `open()` / `change()` triggers a parse and starts a background
  pipeline run (same thread model as production)
- `wait_for_pipeline()` blocks until the background run completes
  (deterministic for assertion tests)
- Stability tests fire requests while the pipeline is in flight,
  exercising concurrency
- `run_pipeline()` convenience method: `open()` + `wait_for_pipeline()`

```rust
let mut s = LspTestSession::new();
s.open("{ import: \"lib.eu\" }\nmain: double(21)");
// AST features work immediately
let _ = s.complete(0, 5);
// Pipeline running in background — requests must not crash
let _ = s.hover(1, 6);
// Wait for pipeline
s.wait_for_pipeline();
// Now semantic features are available
let text = s.hover_text(1, 6);
```

Rapid-edit stability tests:

```rust
for edit in edits {
    s.change(edit);  // cancels previous pipeline
    let _ = s.complete(0, 5);
}
s.wait_for_pipeline();
```

## 8. Acceptance Criteria

### Core functionality

1. Type checking works on unsaved buffers (no longer requires
   file on disk)
2. Hover shows type information from the pipeline's type env
3. Go-to-definition jumps into imported files using the
   pipeline's source map
4. Completion includes names from imports resolved by the
   pipeline
5. Changing a document only triggers a pipeline run when the
   green node actually changes

### Debounce and concurrency

6. Rapid typing does not cause multiple simultaneous pipeline
   runs — each new change cancels the previous
7. LSP requests during a pipeline run do not crash
8. At most two SourceLoaders exist at any time

### Change detection

9. Whitespace-only changes that alter semantics (e.g.
   `x(y)` → `x (y)`) trigger a pipeline run
10. Changes within string content that don't alter the parse
    tree structure do not trigger a pipeline run

### Regression

11. All existing LSP tests pass
12. All existing harness tests pass
13. `eu check lib/prelude.eu` — zero warnings

## 9. Files Changed

| File | Change |
|------|--------|
| `src/syntax/input.rs` | Add `Locator::Buffer` variant |
| `src/driver/source.rs` | Handle `Locator::Buffer` in load |
| `src/driver/lsp/mod.rs` | Redesign ServerState, debounce, pipeline integration |
| `src/driver/lsp/context.rs` | Remove `import_inputs`, simplify |
| `src/driver/lsp/testing.rs` | Background pipeline, `wait_for_pipeline()` |
| `tests/lsp_test.rs` | Update tests for new architecture |

## 10. Out of Scope

- Incremental desugaring (only re-desugar changed declarations)
- Incremental type checking
- Multi-document pipeline (workspace-level analysis)
- Configurable debounce interval
