# LSP: Resolve Imports and Prelude for Go-To-Definition

**Status**: Spec  
**Bead**: eu-tiyv  
**Date**: 2026-05-04

## 1. Overview

The LSP currently loads the prelude into the symbol table but does
not resolve `{ import: "file.eu" }` declarations in the user's
document.  This means go-to-definition, hover, and completion only
work for prelude names, not for names from imported files.

Fix: when a document is opened or changed, parse its import
declarations and load the imported files into the symbol table.

## 2. Current State

- Prelude symbols are loaded via `prelude_symbols()` on server
  startup
- User document is parsed on `didOpen`/`didChange`
- Symbol table contains prelude + current document symbols
- Go-to-definition resolves names via symbol table lookup
- Imported file symbols are absent → go-to-definition fails for
  them

## 3. Proposed Behaviour

### 3.1 Import extraction

When a document is opened or changed, scan its AST for import
metadata blocks:

```eu
{ import: "lib/utils.eu" }
{ import: ["a.eu", "b.eu"] }
```

Extract the file paths from these declarations.

### 3.2 Import resolution

Resolve paths relative to:
1. The document's directory
2. The workspace root (if known)
3. The lib path (if configured)

This matches the resolution order used by the evaluator.

### 3.3 Symbol loading

For each resolved import:
1. Read and parse the file
2. Add its declarations to the symbol table with
   `SymbolSource::Import` (new variant)
3. Track the import's URI so go-to-definition can jump to it

### 3.4 Transitive imports

First pass: resolve direct imports only (one level).
Transitive imports (imports of imports) are deferred — they
add complexity (cycle detection, ordering) and the common case
is one level deep.

### 3.5 Cache and invalidation

Cache parsed imports keyed by file path + mtime.  Invalidate
when:
- The imported file changes (if the editor has it open, we get
  didChange; if not, check mtime on next resolution)
- The importing document's import list changes

## 4. SymbolSource Extension

```rust
pub enum SymbolSource {
    Local,      // current document
    Prelude,    // standard prelude
    Import,     // imported file (NEW)
}
```

Go-to-definition uses the source to determine where to jump:
- `Local` → location within current document
- `Prelude` → location within prelude resource (may not be
  navigable)
- `Import` → location within imported file (navigable)

## 5. Acceptance Criteria

1. `{ import: "lib.eu" }` — names from `lib.eu` resolve in
   go-to-definition
2. Go-to-definition jumps to the correct file and line
3. Hover on imported names shows their type/doc annotations
4. Completion offers imported names
5. Relative path resolution works (relative to document dir)
6. Missing import file → no crash, name stays unresolved
7. Circular imports → no infinite loop
8. Multiple imports in one metadata block work
9. Prelude names still resolve (no regression)
10. Import resolution updates on didChange (e.g. adding a new
    import line)

## 6. Files Changed

| File | Change |
|------|--------|
| `src/driver/lsp/mod.rs` | Import resolution on didOpen/didChange |
| `src/driver/lsp/symbol_table.rs` | Add `SymbolSource::Import`, import loading |
| `src/driver/lsp/navigation.rs` | Handle `Import` source in go-to-definition |

## 7. Dependencies

None.  Independent of type system work.

## 8. Out of Scope

- Transitive import resolution (future work)
- File watcher for external changes to imported files
- Workspace-wide symbol search
- Resource imports (`resource:lens` etc.) — these are embedded
  and not navigable to source
