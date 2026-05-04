# LSP Code Actions for Structural Editing

**Status**: Spec  
**Bead**: eu-88m3  
**Date**: 2026-05-04

## 1. Overview

AST-aware structural editing commands exposed as LSP code actions.
Works in any editor with LSP support (Emacs via eglot, VS Code,
etc.) without editor-specific implementations.

## 2. Code Actions

### 2.1 Wrap as Namespace

**Trigger**: cursor on a property declaration name (e.g. `my-ns: value`)

**Action**: wrap value into a namespace block:

```eu
# Before
my-ns: value

# After
my-ns: {
  ▮
}
```

If the declaration already has a value, move it as the first
declaration inside the block.

### 2.2 Promote Metadata

**Trigger**: cursor on a declaration with shortcut metadata

**Action**: expand shortcut to block form:

```eu
# Before
` "docstring"
name: value

# After
` { doc: "docstring" }
name: value
```

Also works for `:suppress` → `{ export: :suppress }` and
`:main` → `{ target: :main }`.

### 2.3 Add Metadata Field

**Trigger**: cursor on a declaration

**Action** (parameterised by field name): add a metadata field.
If the declaration has no metadata, create it.  If it has
shortcut metadata, promote first.

```eu
# Before (no metadata)
name: value

# After (add type:)
` { type: "▮" }
name: value

# Before (shortcut metadata)
` "docstring"
name: value

# After (add type: — auto-promotes)
` { doc: "docstring", type: "▮" }
name: value
```

Common fields to offer: `type:`, `doc:`, `target:`, `export:`,
`monad:`.

### 2.4 Demote Metadata

**Trigger**: cursor on a declaration with single-field block
metadata

**Action**: collapse to shortcut form:

```eu
# Before
` { doc: "docstring" }
name: value

# After
` "docstring"
name: value
```

Only offered when the block has exactly one field and that field
has a known shortcut form (`doc:` → string, `export: :suppress`
→ `:suppress`, `target: :main` → `:main`).

### 2.5 Lens Form Toggle

**Trigger**: cursor on a lens bracket expression or a composed
lens expression

**Action**: toggle between bracket and composition forms:

```eu
# Bracket → Composition
‹ :items 0 :meta ›
# becomes
at(:items) ∘ nth(0) ∘ at(:meta)

# Composition → Bracket
at(:items) ∘ nth(0) ∘ at(:meta)
# becomes
‹ :items 0 :meta ›
```

The bracket → composition direction always works (each step
maps to a lens constructor).  Composition → bracket only works
when every composed element is a simple lens constructor
(`at(sym)`, `nth(n)`, `each`, `filtered(p)`).

### 2.6 Let-Block Toggle

**Trigger**: cursor on a block expression

**Action**: toggle between plain block and `:let` monadic block:

```eu
# Block → Let-block
{ x: 1, y: x + 1 }
# becomes
{ :let x: 1, y: x + 1 }

# Let-block → Block
{ :let x: 1, y: x + 1 }
# becomes
{ x: 1, y: x + 1 }
```

Note: these are not semantically equivalent — in a `:let` block,
later bindings can reference earlier ones sequentially.  The code
action is a refactoring aid, not a guaranteed equivalence.  The
action title should indicate this (e.g. "Convert to sequential
let-block").

### 2.7 Map / For-Comprehension Toggle

**Trigger**: cursor on a `map(f)` catenation or a single-binding
`:for` block

**Action**: toggle between map and for-comprehension:

```eu
# Map → For
xs map(f)
# becomes
{ :for x: xs }.(f(x))

# For → Map (only when single binding, simple body)
{ :for x: xs }.(f(x))
# becomes
xs map(f)
```

The for → map direction is only offered when:
- The block has exactly one binding
- The body is a single expression (no guards, no nested binds)
- The body can be expressed as a function of the bound variable

The code action extracts the body as a lambda or named function
reference.

## 3. LSP Protocol

Code actions use `textDocument/codeAction`:

- The server inspects the cursor position and surrounding AST
- Returns a list of applicable `CodeAction` items
- Each item has a title, kind (`refactor.rewrite`), and a
  `TextEdit` (or `WorkspaceEdit`) describing the transformation

The editor displays these in a menu (lightbulb in VS Code,
`eglot-code-actions` in Emacs).

## 4. Implementation

### 4.1 AST analysis

Each code action needs a function that:
1. Finds the AST node at the cursor position
2. Determines if the action is applicable
3. Computes the text edit

These use the Rowan parse tree directly — no desugaring needed.

### 4.2 Code action dispatch

In `handle_request`, add a handler for `textDocument/codeAction`
that collects applicable actions for the cursor position:

```rust
fn code_actions(
    root: &SyntaxNode,
    source: &str,
    range: &Range,
    table: &SymbolTable,
) -> Vec<CodeAction>
```

### 4.3 Edit computation

Each action computes a `TextEdit` — a range to replace and the
replacement text.  For multi-site edits (e.g. promote metadata
that also modifies the backtick), use `WorkspaceEdit` with
multiple `TextEdit` entries.

## 5. Implementation Priority

### Phase 1 (0.6.1)

1. Wrap as namespace
2. Promote metadata
3. Add metadata field
4. Demote metadata
5. Let-block toggle

### Phase 2 (future)

6. Lens form toggle
7. Map / for-comprehension toggle

Phase 1 covers the daily-use structural editing needs.  Phase 2
involves more complex AST analysis (lens composition parsing,
lambda extraction).

## 6. Acceptance Criteria

### Phase 1

1. "Wrap as namespace" appears in code actions for property
   declarations
2. "Promote metadata" appears for declarations with shortcut
   metadata
3. "Add type annotation" adds `type:` field, auto-promoting
   if needed
4. "Demote metadata" appears for single-field block metadata
5. "Convert to let-block" / "Convert to plain block" toggles
   correctly
6. Actions produce valid eucalypt syntax
7. Actions preserve surrounding whitespace/formatting
8. Actions work in Emacs (eglot) and VS Code

### Integration

9. No crash when requesting code actions on any valid/invalid
   document state
10. LSP stability tests cover code action requests

## 7. Files Changed

| File | Change |
|------|--------|
| `src/driver/lsp/mod.rs` | Register code action handler |
| `src/driver/lsp/code_actions.rs` | New module: action detection and edit computation |
| `src/driver/lsp/symbol_table.rs` | Possibly extend for metadata analysis |

## 8. Dependencies

- eu-51vu (LSP test harness) — for testing code actions in
  test scripts
- eu-j4j1 (LSP stability) — code action requests must not crash

## 9. Out of Scope

- Formatting after transformation (rely on existing formatter)
- Undo/redo (handled by editor)
- Multi-cursor code actions
