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

These commands run `eu` on the visited file, from the file's directory, so
relative paths (extra inputs in `eucalypt-eu-file-opts`, files a program
reads, and error locations) resolve from the file's location wherever Emacs
was started. They offer to save the buffer first. With `C-u`, each lets you
edit the command before it runs.

- `C-c C-c` — `eucalypt-run`: `eu FILE`
- `C-c C-r` — `eucalypt-run-target`: `eu FILE -t TARGET`, completing the
  target from `eu list-targets FILE` (with each target's doc)
- `C-c C-e` — `eucalypt-evaluate`: `eu FILE -e EXPR`
- `C-c C-t` — `eucalypt-test`: `eu test FILE`
- `C-c C-v` — `eucalypt-check`: `eu check FILE`

`eucalypt-run`, `-run-target` and `-evaluate` show standard output in
`*Eucalypt Output*` (in yaml-mode, json-mode, js-mode or text-mode, from
`-j`, `--json`, `-x FMT` or `--export FMT` in the arguments) and any
errors in `*Eucalypt Errors*`. `eucalypt-test` and `eucalypt-check` run in
`*eucalypt test*` and `*eucalypt check*`; `g` there runs them again. All of
these buffers use `eucalypt-compilation-mode`, so each `┌─ FILE:LINE:COL`
location is clickable and `next-error` (`M-g n`) visits it. `eu` runs with
`NO_COLOR=1`, so no ANSI escape codes appear.

To render text that is not saved to a file:

- `C-c C-k` — `eucalypt-render-buffer`: pipe the buffer to `eu` (as
  `eu@-`, or `yaml@-` and so on from the file extension), from the file's
  directory if it has one
- `eucalypt-render-region` — the same for the active region

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
| `C-c C-c` | `eucalypt-run` | Run `eu` on the file |
| `C-c C-r` | `eucalypt-run-target` | Run `eu` on the file with a target |
| `C-c C-e` | `eucalypt-evaluate` | Evaluate an expression against the file |
| `C-c C-t` | `eucalypt-test` | Run `eu test` on the file |
| `C-c C-v` | `eucalypt-check` | Run `eu check` on the file |
| `C-c C-k` | `eucalypt-render-buffer` | Render buffer through `eu` |
| `C-c C-u` | `eucalypt-unicode-menu` | Unicode operator insertion menu |
| `C-M-a` | `treesit-beginning-of-defun` | Go to start of declaration |
| `C-M-e` | `treesit-end-of-defun` | Go to end of declaration |

## Checking the mode

`scripts/check-emacs-mode.sh` first runs the ERT tests of the commands
above (`editors/emacs/test/eucalypt-mode-commands-test.el`). They drive a
stub `eu` and need no network, grammar or built binary; set `EU` to a real
`eu` to run the tests tagged `real-eu` against it as well:

```sh
EU=target/release/eu scripts/check-emacs-mode.sh
```

### Against the grammar

The mode's font-lock queries name tree-sitter node types, so a grammar
change can break them: a query that no longer matches the grammar raises
`treesit-query-error` and switches off every feature at that
`treesit-font-lock-level`. After changing the grammar or the queries, run

```sh
scripts/check-emacs-mode.sh            # examples/aoc25/*.eu by default
scripts/check-emacs-mode.sh FILE.eu... # or a corpus of your own
```

It compiles the grammar from `editors/tree-sitter-eucalypt/src` into a
temporary directory, compiles every query the mode uses, and fontifies each
file at levels 1 to 4 in `emacs --batch`, failing on any query error or if
level 3 and 4 faces are not applied. It needs Emacs 29+ with tree-sitter
and a C compiler; without them it prints `SKIP` and exits 0, or exits 2
under `--require` (for CI, where they must be present).

## Licence

Copyright 2024, 2026 Greg Hawkins.  Distributed under the same terms as
the [eucalypt](https://github.com/curvelogic/eucalypt) project.
