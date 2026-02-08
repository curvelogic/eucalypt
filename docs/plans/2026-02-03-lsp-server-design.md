# LSP Server Design

Date: 2026-02-03

## Overview

A Language Server Protocol implementation for eucalypt, bundled as the
`eu lsp` subcommand. Communicates via stdio. Built on the `lsp-server`
crate (same as Gleam, Nickel, rust-analyzer).

Three phases, each building on the previous:

1. **Syntax-level features** — diagnostics, document symbols, folding,
   selection ranges
2. **Semantic features** — go-to-definition, hover, completion, find
   references
3. **Polish** — formatting, semantic tokens, rename, code actions,
   inlay hints

### Foundations

Eucalypt already has excellent LSP foundations:

- **Rowan-based lossless parser** with error recovery — preserves all
  source information (whitespace, comments, tokens)
- **Comprehensive AST** with 30+ node types covering the entire
  language, each with `TextRange` for source mapping
- **12+ parse error types** with positional ranges — diagnostics are
  nearly free
- **Tree-sitter grammar** and **Emacs major mode** in `editors/` —
  syntax highlighting already works
- **Import graph** using petgraph — transitive import resolution with
  cycle detection
- **Formatter** (`eu fmt`) using Rowan — two-mode (conservative and
  full reformat), with internal fragment formatting support

### Design Principles

- **Reuse the compiler** — the LSP shares the parser, import resolver,
  and formatter with the main `eu` binary. No separate analysis engine.
- **File-declared context** — the file being edited declares its own
  LSP context via metadata. No guessing.
- **Start minimal, iterate** — diagnostics first, then navigation,
  then polish.

---

## Context Model

When the LSP analyses a file, it needs to know what other files
provide names in scope. Three sources, in order:

### 1. Prelude (always loaded)

The standard prelude (`lib/prelude.eu`) is always loaded as context.
Its declarations (`map`, `filter`, `foldl`, `str.split`, `io.args`,
etc.) are available for completion, hover, and go-to-definition.
Cached once on server start — it never changes during a session.
When the `eu` binary updates (with new prelude functions), restarting
the language server picks up the changes automatically.

### 2. Imports (followed automatically)

Import metadata blocks are already parsed by the existing
`ImportGraph` system:

```eu
` { import: "helpers.eu" }
` { import: ["utils.eu", "data=config.yaml"] }
```

The LSP follows these transitively, using the same `analyse_rowan_ast`
and `Input::from_str` logic as the compiler. Imported files are parsed
and their declarations added to the symbol table.

### 3. lsp-context (explicit co-inputs)

Files that would normally be provided as CLI co-inputs at runtime can
be declared in the file's top-level metadata:

```eu
` { lsp-context: ["base.yaml", "overrides.json", "helpers.eu"] }
```

This is equivalent to the command line:

```
eu base.yaml overrides.json helpers.eu thisfile.eu
```

Ordering matters — earlier entries provide bindings that later ones
can reference, following the same `merge_in` / `rebody` semantics as
CLI co-inputs. Unnamed entries merge their top-level keys flat into
scope. Named entries (e.g. `"data=data.json"`) wrap keys under the
given name.

The `lsp-context` value is a list of strings, each parsed with
`Input::from_str()` — the same format as CLI arguments. Relative paths
are resolved relative to the directory containing the `.eu` file being
edited. For a single entry, a bare string is also accepted:

```eu
` { lsp-context: "data.json" }
```

For data files (JSON, YAML, TOML, CSV), the LSP parses them to
extract top-level keys for completion and go-to-definition. No deep
analysis of values.

---

## Phase 1: Syntax-Level Features

### Architecture

- **Crate**: `lsp-server` (synchronous, crossbeam-channel based)
- **Binary**: `eu lsp` subcommand, added to the existing clap command
  structure in `src/driver/options.rs`
- **Communication**: stdio (stdin/stdout for LSP messages, stderr for
  logging)
- **Threading**: Single-threaded event loop. Re-parse on each
  `didChange` notification. For typical eucalypt files (small), full
  re-parse is sub-millisecond with Rowan.

### Features

#### 1a. Diagnostics

On each `textDocument/didOpen` and `textDocument/didChange`:

1. Re-parse the file with the Rowan parser
2. Collect `Parse::errors()` — each `ParseError` has a `TextRange`
3. Convert to LSP `Diagnostic` with severity, range, and message
4. Push via `textDocument/publishDiagnostics`

The 12+ error types in `src/syntax/rowan/error.rs` map directly to
diagnostic messages: `UnexpectedToken`, `UnclosedSingleQuote`,
`UnterminatedBlock`, `EmptyDeclarationBody`, etc.

Also parse imported files and report their errors, so the user sees
downstream breakage.

#### 1b. Document Symbols

Walk the Rowan `Unit` node, extract `Declaration` nodes:

- Property declarations → `SymbolKind::Property`
- Function declarations (with parameters) → `SymbolKind::Function`
- Operator declarations → `SymbolKind::Operator`

Return as `DocumentSymbol[]` with name, kind, range, and selection
range. Provides the outline/breadcrumb view in both VS Code and Emacs.

Nested blocks produce nested symbol hierarchies.

#### 1c. Folding Ranges

Derive from syntax tree nodes:

- `Block` (`{ ... }`) → folding range
- `List` (`[ ... ]`) → folding range
- Multi-line string patterns → folding range
- Multi-line metadata annotations → folding range

Only emit folding ranges for nodes spanning more than one line.

#### 1d. Selection Range

Smart expand/contract selection using Rowan's parent chain. Each
`SyntaxNode` maps to a selection range, with its parent as the next
expansion level. This is a core Rowan strength — rust-analyzer does
exactly this.

Example expansion sequence for cursor on `x` in `f(x + y)`:
`x` → `x + y` → `(x + y)` → `f(x + y)` → full declaration

### Phase 1 Context

Parse the open file only. Follow imports for error reporting. Load
prelude as a resource but do not build a symbol table from it yet.
`lsp-context` parsing is deferred to phase 2.

### Editor Deliverables (Phase 1)

#### Emacs

Eglot registration for the existing `eucalypt-ts-mode`:

```elisp
(add-to-list 'eglot-server-programs
             '(eucalypt-ts-mode "eu" "lsp"))
```

The tree-sitter mode already provides syntax highlighting and
indentation. Eglot adds diagnostics (via Flymake), document symbols,
and folding.

#### VS Code

Minimal extension:

- `package.json` declaring the `eucalypt` language, `.eu` extension,
  and language server command (`eu lsp`)
- TextMate grammar (`syntaxes/eucalypt.tmLanguage.json`) derived from
  the existing tree-sitter `queries/highlights.scm`
- Language configuration (comment toggling with `#`, bracket pairs,
  auto-closing, 2-space indentation)
- Extension entry point using `vscode-languageclient`

---

## Phase 2: Semantic Features

Phase 2 requires building a **symbol table** from parsed files.

### Symbol Table Construction

On file open/change, after parsing:

1. Parse the open file with Rowan
2. Load the prelude (cached — it never changes)
3. Follow `import` metadata to parse imported files (existing
   `ImportGraph` logic)
4. Parse `lsp-context` metadata — same `Input::from_str()` parsing,
   treated as CLI co-inputs with `merge_in` / `rebody` semantics
5. Walk all parsed trees to build a symbol table: name → (file, range,
   kind, documentation, parameters)

Declaration kinds:

- **Property**: `name: expression`
- **Function**: `f(x, y): expression` — with parameter names and
  arity
- **Operator**: `(x + y): expression` — with fixity and precedence
  from metadata

Documentation comes from `` ` { doc: "..." } `` metadata blocks that
already exist in the language.

For data files referenced via `lsp-context`, extract top-level keys
for the symbol table. JSON objects produce property symbols, YAML
mappings produce property symbols, etc.

### Features

#### 2a. Go to Definition

Resolve the identifier under the cursor against the symbol table.
Search order:

1. Local scope (same block / let binding)
2. File-level declarations
3. Imported file declarations
4. `lsp-context` file declarations
5. Prelude declarations

Return the declaration's file URI and range.

For dotted access like `data.user`, resolve `data` first, then look up
`user` within its declarations.

#### 2b. Hover

Show information about the symbol under the cursor:

- Declaration kind (property / function / operator)
- Parameter names for functions
- Documentation from `` ` { doc: "..." } `` metadata
- Source file (for imported / prelude symbols)
- For prelude built-ins: the intrinsic name

No type inference — purely syntactic information from declarations and
metadata.

#### 2c. Completion

At the cursor position, determine what is in scope and offer
completions. Sources:

1. Local declarations in the same block
2. File-level declarations
3. Imported symbols
4. `lsp-context` symbols
5. Prelude symbols (highest value — `map`, `filter`, `foldl`,
   `str.split`, `io.args`, etc.)

Each completion item includes:

- Label (the name)
- Kind (property / function / operator)
- Detail (parameter list for functions)
- Documentation (from metadata)

For dotted access (`str.`), offer completions from the `str` block's
declarations.

#### 2d. Find References

Given a declaration, search all open/imported files for uses of that
name. Walk the Rowan trees looking for `Name` nodes matching the
declaration. Scope-aware — a local `x` in an inner block does not
match a top-level `x`.

---

## Phase 3: Formatting, Semantic Tokens, and Polish

### 3a. Formatting

Wire up the existing `eu fmt` formatter via LSP:

- **`textDocument/formatting`**: Call `format_source()` with reformat
  mode on the full document. Return the formatted text as a single
  `TextEdit` replacing the entire document.

- **`textDocument/rangeFormatting`**: Find the smallest complete
  syntactic unit (declaration or block) containing the selection.
  Format that unit using the internal `format_soup()` /
  `format_unit()` API. Return `TextEdit`s for the affected range only.

The formatter already works on the Rowan tree, so this is plumbing
rather than new formatting logic. Fragment formatting may need the
internal API to be extended slightly to accept a `Declaration` or
`Element` node rather than only a full `Unit`.

### 3b. Semantic Tokens

Augment tree-sitter / TextMate highlighting with context-aware token
classification. Using the symbol table from phase 2:

- Prelude / built-in functions → `function` + `defaultLibrary`
  modifier
- User-defined functions → `function`
- Parameters → `parameter`
- Top-level properties → `property`
- Operator identifiers → `operator`
- Symbols (`:foo`) → `enumMember` (or custom `symbol` type)
- Metadata annotations → `decorator`
- String interpolation expressions → distinguished from string content
- Comments → `comment`

Implement `textDocument/semanticTokens/full` first. Add
`textDocument/semanticTokens/range` later for performance with large
files.

### 3c. Rename

Given the symbol table and find-references from phase 2, rename a
symbol across the file and its imports:

1. Find all references to the symbol
2. Generate `TextEdit`s for each occurrence
3. Return as `WorkspaceEdit`

Scope-aware — renaming a local binding does not affect same-named
bindings in other scopes.

### 3d. Code Actions

Quick fixes for common errors:

- **Unresolved name matching a prelude symbol**: Suggest the correct
  qualified name (e.g. `split` → `str.split`)
- **`{name: name}` self-reference**: If the eu-dlr static check is
  implemented, offer a fix suggestion
- **Missing import**: If a name resolves to a file in the workspace
  but is not imported, suggest adding an import

### 3e. Inlay Hints

- Show operator precedence and associativity inline for operator
  declarations
- Show parameter names at call sites for functions with multiple
  arguments
- Potentially show inferred structure for complex soup expressions
  (how operators bind)

---

## Crate and Dependency Summary

New dependencies for the LSP:

- `lsp-server` — LSP protocol handling, message dispatch
- `lsp-types` — Rust type definitions for LSP protocol messages
- `serde_json` — already a dependency, used for LSP message
  serialisation

No async runtime needed — `lsp-server` is synchronous.

## File Structure

```
src/
  bin/eu.rs                    # Add LspMode dispatch
  driver/
    options.rs                 # Add LspArgs / lsp subcommand
    lsp.rs (new)               # LSP server main loop
    lsp/
      diagnostics.rs (new)     # Parse errors → LSP diagnostics
      symbols.rs (new)         # Document symbols from Rowan tree
      folding.rs (new)         # Folding ranges
      selection.rs (new)       # Selection ranges
      context.rs (new)         # lsp-context metadata parsing
      symbol_table.rs (new)    # Phase 2: name resolution
      completion.rs (new)      # Phase 2: completion provider
      hover.rs (new)           # Phase 2: hover provider
      navigation.rs (new)      # Phase 2: go-to-def, references
      formatting.rs (new)      # Phase 3: formatter integration
      semantic.rs (new)        # Phase 3: semantic tokens
      actions.rs (new)         # Phase 3: code actions
editors/
  vscode/ (new)                # VS Code extension
    package.json
    language-configuration.json
    syntaxes/eucalypt.tmLanguage.json
    src/extension.ts
  emacs/
    eucalypt-ts-mode.el        # Existing — add eglot registration
```

---

## Beads

Existing:

- **eu-307** — LSP server. Update description with design reference.

New beads to create per phase:

**Phase 1:**
- LSP server scaffold (eu lsp subcommand, lsp-server integration,
  stdio loop)
- LSP diagnostics from Rowan parser
- LSP document symbols
- LSP folding and selection ranges
- VS Code extension (TextMate grammar + language client)

**Phase 2:**
- lsp-context metadata parsing
- Symbol table construction (prelude + imports + lsp-context)
- Go-to-definition
- Hover
- Completion
- Find references

**Phase 3:**
- LSP formatting integration (full document + range)
- Semantic tokens
- Rename
- Code actions
- Inlay hints
