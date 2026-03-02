# Unified Emacs Mode Design

**Bead:** eu-e3x3
**Status:** Design approved, implementation plan pending

## Overview

Consolidate the two existing Emacs modes (`eucalypt-mode.el` — legacy
regex, and `eucalypt-ts-mode.el` — tree-sitter) into a single,
properly packaged `eucalypt-mode` backed by tree-sitter, with LSP
integration, command integration, Unicode input assistance, bracket
colouring, and bug fixes.

The legacy `eucalypt-mode.el` is deleted. The unified mode requires
Emacs 29.1+ (tree-sitter support).

---

## Section 1: Package Structure

**Approach:** Rename `eucalypt-ts-mode.el` to `eucalypt-mode.el`,
keeping tree-sitter as the baseline. Single file, no multi-file
package complexity. The `define-derived-mode` symbol becomes
`eucalypt-mode` (not `eucalypt-ts-mode`).

**Package header:**

```elisp
;;; eucalypt-mode.el --- Major mode for Eucalypt -*- lexical-binding: t; -*-
;; Package-Requires: ((emacs "29.1"))
;; Keywords: languages, eucalypt, tree-sitter
```

**Auto-mode-alist entries:**

```elisp
(add-to-list 'auto-mode-alist '("\\.eu\\'" . eucalypt-mode))
(add-to-list 'auto-mode-alist '("\\.eucalypt\\'" . eucalypt-mode))
(add-to-list 'auto-mode-alist '("Eufile\\'" . eucalypt-mode))
```

**Legacy cleanup:** Delete `editors/emacs/eucalypt-mode.el` (the old
regex mode). All useful functionality (command integration, output
format inference) is migrated into the new unified mode.

---

## Section 2: Eglot LSP Integration

Register the `eu lsp` command as the language server for
`eucalypt-mode` via eglot:

```elisp
(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               '(eucalypt-mode "eu" "lsp")))
```

This is declarative — users who don't use eglot pay no cost. Users who
call `M-x eglot` in a `.eu` buffer get LSP features automatically.

---

## Section 3: Command Integration

### 3.1 Render / Test Commands

Migrate the command infrastructure from the legacy mode:

- `eucalypt-render-buffer` (C-c C-k) — process current buffer through
  `eu` and display output in a side buffer
- `eucalypt-render-region` — process selected region
- Output format inference from command flags (`-j` for JSON, `-x` for
  explicit format, default YAML)
- Output buffer mode selection (yaml-mode, json-mode, etc.)

**Customisation variables:**

```elisp
(defcustom eucalypt-eu-command "eu" ...)
(defcustom eucalypt-eu-global-opts "" ...)
(defcustom eucalypt-eu-file-opts nil
  "Per-file options to pass to `eu'. Set via file-local variables."
  :type '(repeat string)
  :group 'eucalypt
  :safe #'listp)
```

Per-file options allow a `.eu` file to declare its own command-line
context via file-local variables:

```elisp
;; Local Variables:
;; eucalypt-eu-file-opts: ("other-input.yaml" "-x" "json")
;; End:
```

### 3.2 Project Detection

Use `Eufile` as the project root marker, integrating with both
`project.el` and the `eu` command:

```elisp
(defun eucalypt-project-root ()
  "Find the Eucalypt project root by locating Eufile."
  (when-let ((root (locate-dominating-file default-directory "Eufile")))
    root))

;; Register with project.el
(defun eucalypt-project-find (dir)
  "Project.el integration for Eucalypt projects."
  (when-let ((root (locate-dominating-file dir "Eufile")))
    (cons 'eucalypt root)))

(cl-defmethod project-root ((project (head eucalypt)))
  (cdr project))

(add-hook 'project-find-functions #'eucalypt-project-find)
```

When an `Eufile` is found, commands run with `default-directory` set
to the project root so that relative imports resolve correctly.

---

## Section 4: Unicode Input

### 4.1 Quail Input Method

A custom `eucalypt` input method for entering Unicode operators:

```elisp
(quail-define-package
 "eucalypt" "UTF-8" "EU" t
 "Eucalypt Unicode operator input method"
 nil t nil nil nil nil nil nil nil nil t)

(quail-define-rules
 ;; Logical operators
 ("&&"  ?∧)    ; logical and
 ("||"  ?∨)    ; logical or
 ("!!"  ?¬)    ; negation (input method only, !! is index op)
 ;; Comparison
 ("<="  ?≤)    ; less-than-or-equal
 (">="  ?≥)    ; greater-than-or-equal
 ("!="  ?≠)    ; not-equal
 ;; Arithmetic
 ("/-"  ?∸)    ; unary minus
 ("//"  ?÷)    ; exact division (input method only, // is assertion)
 ;; Composition
 (".."  ?∘)    ; compose
 ;; Anaphora
 ("**"  ?•)    ; block anaphor (bullet)
 ;; Sets
 ("{}"  ?∅)    ; empty set
 ;; Brackets (common pairs)
 ("(("  ?⟨)   ("<|"  ?⟨)
 ("))"  ?⟩)   ("|>"  ?⟩)
 ("[["  ?⟦)
 ("]]"  ?⟧)
 ("<<"  ?«)
 (">>"  ?»))
```

Activated with `M-x set-input-method RET eucalypt`.

### 4.2 Transient Menu

A transient menu (Emacs 28+ built-in) for browsing and inserting
Unicode operators by category:

```elisp
(transient-define-prefix eucalypt-unicode-menu ()
  "Insert Eucalypt Unicode operators."
  ["Logical"
   ("a" "∧ and"     (lambda () (interactive) (insert "∧")))
   ("o" "∨ or"      (lambda () (interactive) (insert "∨")))
   ("n" "¬ not"     (lambda () (interactive) (insert "¬")))]
  ["Comparison"
   ("<" "≤ lte"     (lambda () (interactive) (insert "≤")))
   (">" "≥ gte"     (lambda () (interactive) (insert "≥")))
   ("!" "≠ neq"     (lambda () (interactive) (insert "≠")))]
  ["Arithmetic"
   ("-" "∸ negate"  (lambda () (interactive) (insert "∸")))
   ("d" "÷ divide"  (lambda () (interactive) (insert "÷")))]
  ["Anaphora"
   ("b" "• bullet"  (lambda () (interactive) (insert "•")))]
  ["Other"
   ("c" "∘ compose" (lambda () (interactive) (insert "∘")))
   ("e" "∅ empty"   (lambda () (interactive) (insert "∅")))])
```

Bound to `C-c C-u` in the mode keymap.

### 4.3 Auto-Unicodify

A command `eucalypt-unicodify` that replaces ASCII operator sequences
with their Unicode equivalents throughout the buffer or region:

| ASCII | Unicode | Prelude operator |
|-------|---------|------------------|
| `&&`  | `∧`     | logical and      |
| `||`  | `∨`     | logical or       |
| `<=`  | `≤`     | less-or-equal    |
| `>=`  | `≥`     | greater-or-equal |
| `!=`  | `≠`     | not-equal        |

This requires the prelude to define `≤`, `≥`, and `≠` as aliases —
tracked in bead eu-jrrt (dependency of eu-e3x3).

**Implementation:** Simple `replace-regexp-in-region` calls, operating
only outside strings and comments (check `syntax-ppss`).

---

## Section 5: Bracket Colouring

### 5.1 Standard Brackets — Rainbow Delimiters

Use `rainbow-delimiters` for depth-based colouring of `()`, `[]`,
`{}`. Configured automatically when the package is available:

```elisp
(with-eval-after-load 'rainbow-delimiters
  (add-hook 'eucalypt-mode-hook #'rainbow-delimiters-mode))
```

### 5.2 Unicode Brackets — Fixed Distinct Face

Unicode brackets (`RESERVED_OPEN` / `RESERVED_CLOSE` tokens, e.g.
`⟨⟩`, `⟦⟧`, `«»`) get a single fixed face distinct from standard
brackets. This ensures they remain visually distinguishable at small
font sizes where `⟨` might be confused with `<` or `(`.

```elisp
(defface eucalypt-unicode-bracket-face
  '((t :foreground "#e5c07b" :weight bold))
  "Face for Unicode bracket characters in Eucalypt mode."
  :group 'eucalypt)
```

Tree-sitter font-lock rule:

```elisp
:language 'eucalypt
:feature 'unicode-bracket
'((bracket_expr
   (reserved_open) @eucalypt-unicode-bracket-face
   (reserved_close) @eucalypt-unicode-bracket-face))
```

---

## Section 6: Docstring Markdown Rendering

Eucalypt docstrings (`` ` "..." `` and `` ` { doc: "..." } ``) are
assumed to contain Markdown content. Two complementary rendering
approaches:

### 6.1 Inline Font-Lock Sub-Highlighting

A `jit-lock` function post-processes tree-sitter `metadata` regions
containing string nodes, applying Markdown-like faces to their
content:

- `` `code` `` → `font-lock-constant-face` (or a custom
  `eucalypt-doc-code-face`)
- `*bold*` / `**bold**` → bold weight
- `_italic_` → italic slant

The function identifies doc string regions by checking whether a
`metadata` node's child is a `string` node (for `` ` "..." ``) or a
`block` node containing a `doc` key (for `` ` { doc: "..." } ``).
Only the string content is sub-highlighted, not the backtick or block
structure.

```elisp
(defun eucalypt--fontify-doc-markdown (start end)
  "Apply lightweight Markdown highlighting inside docstrings."
  (goto-char start)
  (let ((case-fold-search nil))
    ;; Inline code
    (while (re-search-forward "`\\([^`]+\\)`" end t)
      (when (eucalypt--in-doc-string-p (match-beginning 0))
        (put-text-property (match-beginning 1) (match-end 1)
                           'face 'eucalypt-doc-code-face)))
    ;; Bold
    (goto-char start)
    (while (re-search-forward "\\*\\*?\\([^*]+\\)\\*\\*?" end t)
      (when (eucalypt--in-doc-string-p (match-beginning 0))
        (put-text-property (match-beginning 1) (match-end 1)
                           'face '(:weight bold))))
    ;; Italic
    (goto-char start)
    (while (re-search-forward "\\_<_\\([^_]+\\)_\\_>" end t)
      (when (eucalypt--in-doc-string-p (match-beginning 0))
        (put-text-property (match-beginning 1) (match-end 1)
                           'face '(:slant italic))))))
```

The `eucalypt--in-doc-string-p` helper uses `treesit-node-at` to
check whether the point is inside a metadata string node.

### 6.2 LSP Hover Documentation

When eglot is active, LSP `textDocument/hover` can return docstrings
as Markdown. Eglot renders Markdown in the eldoc popup natively —
no Emacs mode work needed beyond ensuring the LSP server extracts and
returns doc metadata. This gives full Markdown rendering (headings,
lists, code blocks) on hover without any buffer modification.

The LSP server enhancement is out of scope for the Emacs mode — it's
a `eu lsp` feature tracked separately.

---

## Section 7: Bug Fixes

### Bug A: `#` in Strings Treated as Comment

**Problem:** The syntax table marks `#` as comment-start (`<`), so
Emacs font-lock incorrectly highlights `#` inside strings as a
comment. Tree-sitter handles strings correctly, but the syntax table
is still consulted for `syntax-ppss`, `forward-comment`, etc.

**Fix:** Add a `syntax-propertize-function` that scans for `#` inside
tree-sitter string nodes and overrides its syntax class to punctuation:

```elisp
(defun eucalypt-ts-mode--syntax-propertize (start end)
  "Override syntax for # inside strings."
  (goto-char start)
  (while (re-search-forward "#" end t)
    (let ((node (treesit-node-at (1- (point)))))
      (when (member (treesit-node-type node)
                    '("string_content" "c_string_content" "r_string_content"))
        (put-text-property (1- (point)) (point)
                           'syntax-table '(1 . nil))))))
```

### Bug B: Extra Indentation After Metadata

**Problem:** Metadata backtick lines (`` ` "doc" ``) cause the next
declaration to be indented one extra level because tree-sitter sees it
as a child of the metadata node.

**Fix:** Add an indent rule that anchors declarations following
metadata to the same level as the metadata:

```elisp
;; Declaration after metadata aligns with metadata
((match nil "declaration" nil 1) parent-bol 0)
```

This may need refinement based on the exact tree-sitter node
structure; test with real files.

---

## Section 8: Editor Updates for 0.4.0 Features

Cross-review of all 0.4.0 implementation plans identified the
following editor work needed:

### 8.1 Tree-Sitter Grammar Updates

**Idiot brackets (eu-wenf):** Tasks 12-13 added to plan.
- `bracket_expr` rule with `reserved_open` / `reserved_close` tokens
- Bracket pair declarations in `declaration_head`

**Monadic blocks (eu-1x9a):** Tasks 12-13 added to plan.
- `bracket_block` rule for block-mode brackets
- `{}` parameter pattern for block-mode bracket definitions
- `:monad` metadata tag highlighting

**Destructuring (eu-spqy):** Tasks 11-12 added to plan.
- Block and list nodes in `parameter_list`
- `‖` (U+2016) in operator character class
- Juxtaposed call syntax (`f{...}`, `f[...]`)

**Tensor arrays (eu-atzn):** No grammar changes needed. Array
functions are standard prelude identifiers. Prelude function names for
highlighting can be updated in the Emacs mode's keyword list.

### 8.2 VS Code Extension Updates

Each plan above includes a VS Code extension task for TextMate grammar
and language configuration updates.

### 8.3 Tree-Sitter Unicode Operator Coverage

Bead eu-fbyk tracks replacing the hardcoded `OPER_CHARS` character
class in `grammar.js` with an external scanner that uses Unicode
general categories, matching the Rust lexer's dynamic
`is_operator_char()` logic.

---

## Dependencies

- **eu-jrrt** (add `≤`, `≥`, `≠` to prelude) — required for the
  auto-unicodify replacements to produce valid code
- **eu-fbyk** (tree-sitter external scanner for Unicode operators) —
  optional improvement, not blocking the Emacs mode work
- Tree-sitter grammar updates from idiot brackets / monadic blocks /
  destructuring plans — these are tracked in their respective plans

## Out of Scope

- VS Code extension work (tracked in individual feature plans)
- Tree-sitter grammar updates for new language features (tracked in
  feature plans, referenced in Section 7)
- Flycheck / flymake integration (potential future enhancement)
- REPL integration (potential future enhancement)
