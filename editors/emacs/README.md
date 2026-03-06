# eucalypt-mode

An Emacs major mode for editing [Eucalypt](https://github.com/curvelogic/eucalypt)
files, powered by tree-sitter.

## Features

### Syntax highlighting

Full tree-sitter-based font-lock with four levels of detail:

- **Level 1** — comments, strings (plain, c-strings, raw strings)
- **Level 2** — numbers, symbols, keywords (`if`, `then`, `cond`, `true`, `false`, `null`), builtins (`__ADD` etc.)
- **Level 3** — declarations (properties vs functions), operator declarations, parameters (including destructuring patterns), function calls
- **Level 4** — brackets, operators, string interpolation, anaphora, metadata annotations, prelude functions

### Indentation

Automatic indentation for blocks, lists, parenthesised expressions,
declaration bodies, and idiot bracket expressions.

### Navigation and Imenu

- `C-M-a` / `C-M-e` — move to beginning/end of declaration
- Imenu integration lists all declarations in the buffer

### Command integration

- `C-c C-k` — render the current buffer through `eu` and display output
- `C-u C-c C-k` — same, but edit the command before running
- `eucalypt-render-region` — render just the active region
- Output is displayed in a buffer with the appropriate mode (yaml-mode,
  json-mode, or text-mode) based on the output format

### Unicode operator input

- `C-c C-u` — transient menu for inserting Unicode operators:

  | Key | Operator | Meaning |
  |-----|----------|---------|
  | `a` | `∧` | logical and |
  | `o` | `∨` | logical or |
  | `n` | `¬` | logical not |
  | `<` | `≤` | less-or-equal |
  | `>` | `≥` | greater-or-equal |
  | `!` | `≠` | not-equal |
  | `-` | `∸` | unary minus |
  | `d` | `÷` | exact division |
  | `b` | `•` | bullet anaphor |
  | `c` | `∘` | compose |
  | `e` | `∅` | empty |

- **Quail input method** (`eucalypt`) — type ASCII sequences that
  auto-translate to Unicode:

  | Sequence | Result | | Sequence | Result |
  |----------|--------|-|----------|--------|
  | `&&` | `∧` | | `\|\|` | `∨` |
  | `<=` | `≤` | | `>=` | `≥` |
  | `!=` | `≠` | | `/-` | `∸` |
  | `..` | `∘` | | `**` | `•` |
  | `[[` | `⟦` | | `]]` | `⟧` |
  | `((` | `⟨` | | `))` | `⟩` |
  | `<<` | `«` | | `>>` | `»` |

- `eucalypt-unicodify` — batch-replace ASCII operator sequences with
  Unicode equivalents in the buffer or region (skips strings and comments)

### LSP integration

Automatic [eglot](https://github.com/joaotavora/eglot) configuration —
connects to `eu lsp` when eglot is activated in a eucalypt buffer.

### Project detection

Recognises Eucalypt projects by the presence of an `Eufile` in a parent
directory.  Integrates with Emacs's built-in `project.el`.

### Optional integrations

- **rainbow-delimiters** — automatically enabled if installed
- **yaml-mode** / **json-mode** — used for output buffers when available

## Requirements

- Emacs 29.1 or later (built with tree-sitter support)
- The `tree-sitter-eucalypt` grammar (bundled in this repository)
- The [transient](https://github.com/magit/transient) package (bundled
  with Emacs 29+)

## Installation

### With use-package :vc (Emacs 30+)

```elisp
(use-package eucalypt-mode
  :vc (:url "https://github.com/curvelogic/eucalypt"
       :lisp-dir "editors/emacs"))
```

Then install the tree-sitter grammar:

```elisp
(add-to-list 'treesit-language-source-alist
             '(eucalypt "https://github.com/curvelogic/eucalypt"
                        nil "editors/tree-sitter-eucalypt/src"))
(treesit-install-language-grammar 'eucalypt)
```

### With package-vc (Emacs 29)

Emacs 29's `package-vc` does not support `:lisp-dir`, so clone
manually:

```sh
git clone https://github.com/curvelogic/eucalypt.git ~/path/to/eucalypt
```

```elisp
(add-to-list 'load-path "~/path/to/eucalypt/editors/emacs")

(add-to-list 'treesit-language-source-alist
             '(eucalypt "~/path/to/eucalypt"
                        nil "editors/tree-sitter-eucalypt/src"))
(treesit-install-language-grammar 'eucalypt)

(require 'eucalypt-mode)
```

### With straight.el

```elisp
(straight-use-package
 '(eucalypt-mode :type git
                 :host github
                 :repo "curvelogic/eucalypt"
                 :files ("editors/emacs/*.el")))
```

Then install the tree-sitter grammar as above.

## Customisation

All options are in the `eucalypt` customisation group (`M-x customize-group RET eucalypt`):

| Variable | Default | Description |
|----------|---------|-------------|
| `eucalypt-indent-offset` | `2` | Spaces per indentation level |
| `eucalypt-eu-command` | `"eu"` | Path to the `eu` executable |
| `eucalypt-eu-global-opts` | `""` | Default options passed to `eu` |
| `eucalypt-eu-file-opts` | `nil` | Per-file options (set via file-local variables) |

### Per-file options example

At the end of a `.eu` file:

```
# Local Variables:
# eucalypt-eu-file-opts: ("data.yaml" "-x" "json")
# End:
```

## Key bindings

| Binding | Command | Description |
|---------|---------|-------------|
| `C-c C-k` | `eucalypt-render-buffer` | Render buffer through `eu` |
| `C-c C-u` | `eucalypt-unicode-menu` | Unicode operator insertion menu |
| `C-M-a` | `treesit-beginning-of-defun` | Go to start of declaration |
| `C-M-e` | `treesit-end-of-defun` | Go to end of declaration |

## Licence

Copyright 2024, 2026 Greg Hawkins.  Distributed under the same terms as
the [eucalypt](https://github.com/curvelogic/eucalypt) project.
