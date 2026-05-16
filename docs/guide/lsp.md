# Language Server Protocol (LSP) Support

Eucalypt includes a built-in LSP server for editor integration.
Start it with `eu lsp`.

## Editor Setup

### Emacs (eglot)

Add to your eucalypt-mode configuration:

```elisp
(add-to-list 'eglot-server-programs '(eucalypt-mode "eu" "lsp"))
```

### VS Code

Install the eucalypt VS Code extension, which configures the
language server automatically.

## Features

### Diagnostics

- Parse errors appear as you type
- Type warnings from `eu check` appear after a short debounce
  (300ms after the last keystroke)
- Pipeline errors (e.g. invalid monad spec) surface as warnings
  with source locations

### Hover

- Declaration names show documentation and type annotations
- Prelude functions show their type signatures
- Monad tags (`:for`, `:io`) show monad description and binding
  type
- Bracket pair delimiters show pair name, mode, and documentation

### Completion

- Names from the current file, prelude, and imported files
- Monad tag completion after `{ :`
- Dotted access completion (e.g. `str.` offers string functions)

### Go-to-definition

- Jump to declaration in the current file
- Jump into imported files
- Jump to prelude source (via temp file)
- Jump to bracket pair definitions from bracket delimiters

### Inlay Hints

- Monadic block binding types (e.g. `x: number` for
  `{ :for x: [1,2,3] }`)
- Operator fixity on operator declarations
- Parameter names at multi-argument call sites

### Code Actions

- **Wrap as namespace** — wrap a single declaration's value into
  a block, or wrap multiple selected declarations into a new
  namespace with reference prefixing
- **Promote metadata** — expand shortcut form to block
  (e.g. `` ` "doc" `` → `` ` { doc: "doc" } ``)
- **Demote metadata** — collapse single-field block to shortcut
- **Add metadata field** — add type, doc, target, export, monad,
  format, or type-def metadata
- **Let-block toggle** — convert between `{ x: 1 }` and
  `{ :let x: 1 }`
- **Qualify name** — suggest `str.split` for unresolved `split`

### Other

- Document symbols (outline)
- Folding ranges
- Semantic tokens (syntax highlighting)
- Document highlight (matching bracket pairs)
- Find references
- Rename symbol
- Selection range
- Formatting

## Architecture

The LSP server uses a two-tier architecture:

1. **AST-level** (immediate): parse errors, completion, hover,
   semantic tokens — works on every keystroke, even on broken
   input
2. **Pipeline-level** (debounced): type checking, import
   resolution, monadic binding types — runs the full
   desugarer/type checker pipeline after 300ms of no changes,
   with green node comparison to skip unchanged documents
