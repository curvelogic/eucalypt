# Unified Emacs Mode Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Consolidate two Emacs modes into one tree-sitter-backed
`eucalypt-mode` with LSP, command integration, Unicode input, bracket
colouring, and bug fixes.

**Architecture:** Start from the existing `eucalypt-ts-mode.el`,
rename all symbols from `eucalypt-ts-*` to `eucalypt-*`, migrate
command infrastructure from the legacy `eucalypt-mode.el`, then add
new features (LSP, Unicode, brackets, project detection) incrementally.
Delete the legacy mode file last.

**Tech Stack:** Emacs Lisp, tree-sitter (Emacs 29.1+), eglot, quail,
transient.

---

## Background

There are currently two Emacs modes:

- `editors/emacs/eucalypt-mode.el` — legacy regex-based mode with
  command integration (render buffer, format inference, output buffers)
- `editors/emacs/eucalypt-ts-mode.el` — tree-sitter mode with proper
  font-lock, indentation, imenu, and navigation

The design doc is at `docs/plans/2026-03-01-emacs-mode-design.md`.

---

## Task 1: Rename eucalypt-ts-mode to eucalypt-mode

Create the unified mode file by renaming all `eucalypt-ts-*` symbols
to `eucalypt-*`. This is a mechanical find-and-replace.

**Files:**
- Create: `editors/emacs/eucalypt-mode.el` (new unified file)
- The old `editors/emacs/eucalypt-mode.el` (legacy) will be replaced

**Step 1: Copy the tree-sitter mode as the starting point**

The legacy file will be overwritten. Its command integration is
migrated in Task 3. For now, copy `eucalypt-ts-mode.el` to
`eucalypt-mode.el`, replacing the legacy file entirely.

**Step 2: Rename all symbols**

In the new `eucalypt-mode.el`, make these replacements (all
occurrences):

| Old | New |
|-----|-----|
| `eucalypt-ts-mode` | `eucalypt-mode` |
| `eucalypt-ts-indent-offset` | `eucalypt-indent-offset` |
| `eucalypt-ts-mode--syntax-table` | `eucalypt-mode--syntax-table` |
| `eucalypt-ts-mode--font-lock-settings` | `eucalypt-mode--font-lock-settings` |
| `eucalypt-ts-mode--indent-rules` | `eucalypt-mode--indent-rules` |
| `eucalypt-ts-mode--defun-name` | `eucalypt-mode--defun-name` |
| `eucalypt-ts` (defgroup name) | `eucalypt` |
| `eucalypt-ts-` (prefix in defgroup) | `eucalypt-` |

**Step 3: Update package header**

Replace the header with:

```elisp
;;; eucalypt-mode.el --- Major mode for Eucalypt -*- lexical-binding: t; -*-

;; Copyright © 2024, 2026 Greg Hawkins

;; Author: Greg Hawkins
;; Version: 1.0.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: languages, eucalypt, tree-sitter
;; URL: https://github.com/curvelogic/eucalypt

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Major mode for editing Eucalypt files (.eu, .eucalypt), powered by
;; tree-sitter.  Provides syntax highlighting, indentation, LSP
;; integration via eglot, command integration for rendering buffers,
;; and Unicode operator input assistance.
;;
;; Requirements:
;; 1. Emacs 29.1 or later (with tree-sitter support)
;; 2. The tree-sitter-eucalypt grammar installed
;;
;; Installation of the grammar:
;;   (add-to-list 'treesit-language-source-alist
;;                '(eucalypt "https://github.com/curvelogic/eucalypt"
;;                           nil nil "editors/tree-sitter-eucalypt/src"))
;;   (treesit-install-language-grammar 'eucalypt)
;;
;; Then add to your init file:
;;   (require 'eucalypt-mode)

;;; Code:
```

**Step 4: Update the provide and footer**

Change the final lines to:

```elisp
(provide 'eucalypt-mode)

;;; eucalypt-mode.el ends here
```

**Step 5: Update the mode docstring**

In `define-derived-mode`, update the docstring:

```elisp
(define-derived-mode eucalypt-mode prog-mode "Eucalypt"
  "Major mode for editing Eucalypt files, powered by tree-sitter.

Key bindings:
\\{eucalypt-mode-map}"
```

**Step 6: Verify the file is self-consistent**

Search for any remaining `eucalypt-ts` references. There should be
none.

**Step 7: Commit**

```
git commit -m "feat(emacs): rename eucalypt-ts-mode to eucalypt-mode"
```

---

## Task 2: Add Keymap

The tree-sitter mode had no keymap. Add one for the commands that will
be added in Task 3 and the Unicode menu in Task 6.

**Files:**
- Modify: `editors/emacs/eucalypt-mode.el`

**Step 1: Add the keymap variable**

After the syntax table definition (after the `eucalypt-mode--syntax-table`
variable), add:

```elisp
(defvar eucalypt-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-c\C-k" #'eucalypt-render-buffer)
    (define-key map "\C-c\C-u" #'eucalypt-unicode-menu)
    map)
  "Keymap for `eucalypt-mode'.")
```

The functions referenced here don't exist yet — they'll be added in
Tasks 3 and 6. Emacs is fine with forward-referencing command symbols
in keymaps.

**Step 2: Commit**

```
git commit -m "feat(emacs): add mode keymap with render and unicode bindings"
```

---

## Task 3: Migrate Command Integration

Port the `eu` command infrastructure from the legacy mode: render
buffer/region, output format inference, output buffer handling.

**Files:**
- Modify: `editors/emacs/eucalypt-mode.el`

The legacy mode (old `eucalypt-mode.el`, readable at git commit
`a5e5b30` or earlier) has these functions to migrate:

- `eucalypt-eu-command` (defcustom)
- `eucalypt-eu-global-opts` (defcustom)
- `eucalypt--command-output-buffer` (defconst)
- `eucalypt--command-error-buffer` (defconst)
- `eucalypt--form-command`
- `eucalypt--process-region`
- `eucalypt-render-region`
- `eucalypt-render-buffer`
- `eucalypt--infer-output-format`
- `eucalypt--select-output-mode`
- `eucalypt-buffer-input-format`
- `eucalypt--extension-to-format`

**Step 1: Add customisation variables**

After the `eucalypt-indent-offset` defcustom, add:

```elisp
(defcustom eucalypt-eu-command
  "eu"
  "The command used to execute the eucalypt command line program."
  :type 'string
  :group 'eucalypt)

(defcustom eucalypt-eu-global-opts
  ""
  "Default options to pass to `eu'."
  :type 'string
  :group 'eucalypt)

(defcustom eucalypt-eu-file-opts nil
  "Per-file options to pass to `eu'.
Set via file-local variables, e.g.:

  ;; Local Variables:
  ;; eucalypt-eu-file-opts: (\"data.yaml\" \"-x\" \"json\")
  ;; End:"
  :type '(repeat string)
  :group 'eucalypt
  :safe #'listp)
```

**Step 2: Add command infrastructure**

After the customisation variables, add:

```elisp
(defconst eucalypt--command-output-buffer
  "*Eucalypt Output*"
  "Name of buffer for `eu' command output.")

(defconst eucalypt--command-error-buffer
  "*Eucalypt Errors*"
  "Name of buffer for `eu' command error output.")

(defun eucalypt--form-command (opts)
  "Formulate a command line call to `eu' with OPTS."
  (let ((exe (executable-find eucalypt-eu-command)))
    (unless exe
      (error "Cannot find `%s' executable" eucalypt-eu-command))
    (string-join
     (append (list exe)
             (split-string eucalypt-eu-global-opts " " t)
             eucalypt-eu-file-opts
             (list opts))
     " ")))

(defun eucalypt--infer-output-format (command)
  "Infer the output format from COMMAND flags."
  (cond
   ((string-match "-j" command) 'json)
   ((string-match "-x[[:space:]]+\\(\\w+\\)" command)
    (intern (match-string 1 command)))
   (t 'yaml)))

(defun eucalypt--select-output-mode (format)
  "Select an appropriate major mode for FORMAT."
  (cond
   ((and (eq format 'yaml) (commandp 'yaml-mode)) #'yaml-mode)
   ((and (eq format 'json) (commandp 'json-mode)) #'json-mode)
   ((and (eq format 'json) (commandp 'js-mode))   #'js-mode)
   (t #'text-mode)))

(defun eucalypt--process-region (min max command)
  "Process region from MIN to MAX with COMMAND and display output."
  (let* ((output-format (eucalypt--infer-output-format command))
         (output-mode (eucalypt--select-output-mode output-format)))
    (shell-command-on-region min max command
                             eucalypt--command-output-buffer
                             nil
                             eucalypt--command-error-buffer
                             t)
    (when-let ((buf (get-buffer eucalypt--command-output-buffer)))
      (with-current-buffer buf
        (funcall output-mode)))))

(defun eucalypt--extension-to-format (extension)
  "Infer an input format from file EXTENSION."
  (pcase extension
    ("yaml" "yaml")
    ("csv"  "csv")
    ("txt"  "txt")
    (_      "eu")))

(defun eucalypt-buffer-input-format ()
  "Determine the input format for the current buffer."
  (eucalypt--extension-to-format
   (file-name-extension (or buffer-file-name "file.eu"))))

(defun eucalypt-render-buffer (prefix)
  "Process the entire buffer through `eu' and display output.
With PREFIX argument, prompt to edit the command first."
  (interactive "P")
  (let* ((fmt (eucalypt-buffer-input-format))
         (cmd (eucalypt--form-command (concat fmt "@-")))
         (command (if prefix (read-shell-command "Command: " cmd) cmd)))
    (eucalypt--process-region (point-min) (point-max) command)))

(defun eucalypt-render-region (prefix)
  "Process the region through `eu' and display output.
With PREFIX argument, prompt to edit the command first."
  (interactive "P")
  (let* ((fmt (eucalypt-buffer-input-format))
         (cmd (eucalypt--form-command (concat fmt "@-")))
         (command (if prefix (read-shell-command "Command: " cmd) cmd)))
    (eucalypt--process-region (region-beginning) (region-end) command)))
```

**Step 3: Commit**

```
git commit -m "feat(emacs): migrate command integration from legacy mode"
```

---

## Task 4: Add Project Detection

Detect Eucalypt project roots via `Eufile` and integrate with
`project.el`.

**Files:**
- Modify: `editors/emacs/eucalypt-mode.el`

**Step 1: Add project detection functions**

After the command integration code (before the mode definition), add:

```elisp
(defun eucalypt-project-root ()
  "Find the Eucalypt project root by locating Eufile."
  (when-let ((root (locate-dominating-file default-directory "Eufile")))
    root))

(defun eucalypt-project-find (dir)
  "Project.el integration for Eucalypt projects.
Locate Eufile starting from DIR."
  (when-let ((root (locate-dominating-file dir "Eufile")))
    (cons 'eucalypt root)))

(cl-defmethod project-root ((project (head eucalypt)))
  "Return the root directory of an Eucalypt PROJECT."
  (cdr project))

(add-hook 'project-find-functions #'eucalypt-project-find)
```

**Step 2: Add cl-lib require**

At the top of the file, after the existing requires, add:

```elisp
(require 'cl-lib)
```

This is needed for `cl-defmethod`.

**Step 3: Use project root in commands**

Update `eucalypt--process-region` to run from the project root when
available:

```elisp
(defun eucalypt--process-region (min max command)
  "Process region from MIN to MAX with COMMAND and display output."
  (let* ((default-directory (or (eucalypt-project-root) default-directory))
         (output-format (eucalypt--infer-output-format command))
         (output-mode (eucalypt--select-output-mode output-format)))
    (shell-command-on-region min max command
                             eucalypt--command-output-buffer
                             nil
                             eucalypt--command-error-buffer
                             t)
    (when-let ((buf (get-buffer eucalypt--command-output-buffer)))
      (with-current-buffer buf
        (funcall output-mode)))))
```

**Step 4: Commit**

```
git commit -m "feat(emacs): add Eufile project detection and project.el integration"
```

---

## Task 5: Add Eglot LSP Integration

Register `eu lsp` as the language server for eucalypt-mode.

**Files:**
- Modify: `editors/emacs/eucalypt-mode.el`

**Step 1: Add eglot registration**

After the project detection code, add:

```elisp
(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               '(eucalypt-mode "eu" "lsp")))
```

This is a single `with-eval-after-load` form — no cost if the user
doesn't use eglot, and auto-configures when they do.

**Step 2: Commit**

```
git commit -m "feat(emacs): register eu lsp with eglot"
```

---

## Task 6: Add Unicode Input Method

Add the quail input method for entering Unicode operators via ASCII
sequences.

**Files:**
- Modify: `editors/emacs/eucalypt-mode.el`

**Step 1: Add quail require and input method definition**

After the eglot registration, add:

```elisp
(require 'quail)

(quail-define-package
 "eucalypt" "UTF-8" "EU" t
 "Eucalypt Unicode operator input method.
Translates ASCII operator sequences to Unicode equivalents.

Key sequences:
  && → ∧  (logical and)
  || → ∨  (logical or)
  <= → ≤  (less-or-equal)
  >= → ≥  (greater-or-equal)
  != → ≠  (not-equal)
  /- → ∸  (unary minus)
  .. → ∘  (compose)
  << → «  >> → »  (angle brackets)
  (( → ⟨  )) → ⟩  (mathematical angle brackets)
  [[ → ⟦  ]] → ⟧  (double square brackets)"
 nil t nil nil nil nil nil nil nil nil t)

(quail-define-rules
 ;; Logical operators
 ("&&"  ?∧)
 ("||"  ?∨)
 ;; Comparison
 ("<="  ?≤)
 (">="  ?≥)
 ("!="  ?≠)
 ;; Arithmetic
 ("/-"  ?∸)
 ;; Composition
 (".."  ?∘)
 ;; Brackets
 ("(("  ?⟨)
 ("))"  ?⟩)
 ("[["  ?⟦)
 ("]]"  ?⟧)
 ("<<"  ?«)
 (">>"  ?»))
```

**Step 2: Commit**

```
git commit -m "feat(emacs): add quail input method for Unicode operators"
```

---

## Task 7: Add Transient Unicode Menu

Add a transient menu for browsing and inserting Unicode operators by
category.

**Files:**
- Modify: `editors/emacs/eucalypt-mode.el`

**Step 1: Add transient menu definition**

After the quail rules, add:

```elisp
(require 'transient)

(defun eucalypt--insert-char (char)
  "Return a command that inserts CHAR at point."
  (lambda () (interactive) (insert char)))

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
  ["Other"
   ("c" "∘ compose" (lambda () (interactive) (insert "∘")))
   ("e" "∅ empty"   (lambda () (interactive) (insert "∅")))])
```

The `eucalypt--insert-char` helper is defined but not used by the
transient (transient requires inline lambdas). It can be removed if
not needed elsewhere.

**Step 2: Commit**

```
git commit -m "feat(emacs): add transient menu for Unicode operator insertion"
```

---

## Task 8: Add Auto-Unicodify Command

Add `eucalypt-unicodify` to replace ASCII operator sequences with
Unicode equivalents, respecting strings and comments.

**Files:**
- Modify: `editors/emacs/eucalypt-mode.el`

**Step 1: Add the unicodify command**

After the transient menu, add:

```elisp
(defvar eucalypt--unicodify-replacements
  '(("&&" . "∧")
    ("||" . "∨")
    ("<=" . "≤")
    (">=" . "≥")
    ("!=" . "≠"))
  "ASCII to Unicode operator replacements for `eucalypt-unicodify'.")

(defun eucalypt-unicodify (&optional beg end)
  "Replace ASCII operator sequences with Unicode equivalents.
Operates on the region from BEG to END, or the entire buffer if no
region is active.  Skips replacements inside strings and comments."
  (interactive
   (if (use-region-p)
       (list (region-beginning) (region-end))
     (list (point-min) (point-max))))
  (save-excursion
    (dolist (pair eucalypt--unicodify-replacements)
      (goto-char beg)
      (while (search-forward (car pair) end t)
        (let ((ppss (syntax-ppss (match-beginning 0))))
          (unless (or (nth 3 ppss)    ; inside string
                      (nth 4 ppss))   ; inside comment
            (replace-match (cdr pair) t t)))))))
```

**Step 2: Commit**

```
git commit -m "feat(emacs): add eucalypt-unicodify command"
```

---

## Task 9: Add Bracket Colouring

Configure rainbow-delimiters for standard brackets and a custom face
for Unicode brackets.

**Files:**
- Modify: `editors/emacs/eucalypt-mode.el`

**Step 1: Add the Unicode bracket face**

After the auto-unicodify code, add:

```elisp
(defface eucalypt-unicode-bracket-face
  '((((background dark))
     :foreground "#e5c07b" :weight bold)
    (((background light))
     :foreground "#986801" :weight bold))
  "Face for Unicode bracket characters in Eucalypt mode."
  :group 'eucalypt)
```

**Step 2: Add rainbow-delimiters hook**

```elisp
(with-eval-after-load 'rainbow-delimiters
  (add-hook 'eucalypt-mode-hook #'rainbow-delimiters-mode))
```

**Step 3: Note on Unicode bracket font-lock**

The tree-sitter font-lock rule for Unicode brackets requires the
`bracket_expr` rule in the tree-sitter grammar (added by the idiot
brackets implementation plan, eu-wenf). Until that grammar update
lands, the face is defined but no font-lock rule references it.

When the grammar is updated, add this to the font-lock settings:

```elisp
:language 'eucalypt
:feature 'unicode-bracket
'((bracket_expr
   (reserved_open) @eucalypt-unicode-bracket-face
   (reserved_close) @eucalypt-unicode-bracket-face))
```

And add `unicode-bracket` to the fourth tier of
`treesit-font-lock-feature-list`.

For now, just define the face and the rainbow-delimiters hook.

**Step 4: Commit**

```
git commit -m "feat(emacs): add bracket colouring support"
```

---

## Task 10: Fix Bug — `#` in Strings Treated as Comment

Add a `syntax-propertize-function` to prevent `#` inside strings from
being treated as a comment character.

**Files:**
- Modify: `editors/emacs/eucalypt-mode.el`

**Step 1: Add the syntax-propertize function**

Before the mode definition, add:

```elisp
(defun eucalypt-mode--syntax-propertize (start end)
  "Override syntax for `#' inside tree-sitter string nodes.
Between START and END, any `#' that falls inside a string node
has its syntax class changed from comment-start to punctuation."
  (goto-char start)
  (while (re-search-forward "#" end t)
    (let ((node (treesit-node-at (1- (point)))))
      (when (and node
                 (member (treesit-node-type node)
                         '("string_content" "c_string_content"
                           "r_string_content" "comment")))
        ;; Only override if inside a string (not an actual comment)
        (when (member (treesit-node-type
                       (treesit-node-parent node))
                      '("string" "c_string" "r_string"))
          (put-text-property (1- (point)) (point)
                             'syntax-table '(1 . nil)))))))
```

**Step 2: Register in mode definition**

In the `define-derived-mode` body, after the comment setup, add:

```elisp
  ;; Syntax propertize (fix # inside strings)
  (setq-local syntax-propertize-function
              #'eucalypt-mode--syntax-propertize)
```

**Step 3: Commit**

```
git commit -m "fix(emacs): prevent # in strings being treated as comment"
```

---

## Task 11: Fix Bug — Extra Indentation After Metadata

Fix the indentation rule so declarations following metadata backtick
lines don't get an extra indent level.

**Files:**
- Modify: `editors/emacs/eucalypt-mode.el`

**Step 1: Update the indent rules**

In `eucalypt-mode--indent-rules`, add a rule before the
`(parent-is "declaration")` catch-all. The metadata node is a child of
the declaration, so when the cursor is on a declaration that follows a
metadata declaration, it should align with its siblings, not indent
further.

Add this rule in the indent rules list, before the `declaration`
parent rule:

```elisp
     ;; Metadata is part of declaration — don't indent the body extra
     ((parent-is "metadata") parent-bol 0)
```

The full updated indent rules should be:

```elisp
(defvar eucalypt-mode--indent-rules
  `((eucalypt
     ;; Top-level declarations at column 0
     ((parent-is "source_file") column-0 0)
     ;; Inside blocks, indent declarations
     ((parent-is "block") parent-bol eucalypt-indent-offset)
     ;; Inside lists, indent elements
     ((parent-is "list") parent-bol eucalypt-indent-offset)
     ;; Inside parentheses, indent contents
     ((parent-is "paren_expr") parent-bol eucalypt-indent-offset)
     ((parent-is "argument_list") parent-bol eucalypt-indent-offset)
     ((parent-is "parameter_list") parent-bol eucalypt-indent-offset)
     ;; Soup continuation
     ((parent-is "soup") parent-bol eucalypt-indent-offset)
     ;; Metadata is part of declaration — don't indent the body extra
     ((parent-is "metadata") parent-bol 0)
     ;; Declaration body continuation
     ((parent-is "declaration") parent-bol eucalypt-indent-offset)
     ;; Closing brackets align with opening
     ((node-is "}") parent-bol 0)
     ((node-is "]") parent-bol 0)
     ((node-is ")") parent-bol 0)
     ;; Default: no change
     (no-node parent-bol 0)))
  "Indentation rules for `eucalypt-mode'.")
```

**Step 2: Commit**

```
git commit -m "fix(emacs): correct indentation after metadata declarations"
```

---

## Task 12: Delete Legacy Mode

Remove the old regex-based `eucalypt-mode.el` file. By this point all
its useful functionality has been migrated to the unified mode.

**Files:**
- Delete: `editors/emacs/eucalypt-ts-mode.el`

**Step 1: Delete the old tree-sitter mode file**

The legacy `eucalypt-mode.el` was already overwritten in Task 1. The
remaining file to clean up is `eucalypt-ts-mode.el`, which is now
superseded by the unified `eucalypt-mode.el`.

```
git rm editors/emacs/eucalypt-ts-mode.el
```

**Step 2: Commit**

```
git commit -m "chore(emacs): remove superseded eucalypt-ts-mode.el"
```

---

## Task 13: Update Tree-Sitter Highlights Query

The `editors/tree-sitter-eucalypt/queries/highlights.scm` file is used
by other editors (neovim, helix) and should be kept in sync with the
Emacs font-lock features. No functional change to Emacs, but ensures
consistency.

**Files:**
- Modify: `editors/tree-sitter-eucalypt/queries/highlights.scm`

**Step 1: Review and update the prelude keyword list**

The current `highlights.scm` (line 94-106) has a subset of prelude
names. Update the `#any-of?` list to include commonly used names that
are missing:

```scheme
((identifier) @keyword
  (#any-of? @keyword
    "if" "then" "when" "cond"
    "true" "false" "null" "nil"
    "cons" "head" "tail" "first" "second"
    "head-or" "tail-or" "second-or"
    "map" "filter" "foldl" "foldr" "scanl" "scanr"
    "and" "or" "not"
    "merge" "concat" "append" "prepend"
    "identity" "const" "compose" "apply" "flip"
    "take" "drop" "take-while" "drop-while"
    "all" "any" "all-true?" "any-true?"
    "keys" "values" "lookup" "has"
    "range" "repeat" "iterate" "cycle"
    "zip" "zip-with" "reverse" "remove"
    "mapcat" "group-by" "qsort" "partition"
    "negate" "inc" "dec" "floor" "ceiling"
    "max" "min" "abs" "num"
    "panic" "assert"))
```

**Step 2: Commit**

```
git commit -m "chore(tree-sitter): update prelude names in highlights query"
```

---

## Task 14: Final Verification and Cleanup

Verify the complete mode file is self-consistent and all features
work together.

**Files:**
- Modify: `editors/emacs/eucalypt-mode.el` (if any fixes needed)

**Step 1: Check for stale references**

Search the new `eucalypt-mode.el` for:
- Any remaining `eucalypt-ts` references (should be none)
- Any references to the deleted legacy mode
- Duplicate `defcustom` or `defvar` declarations

**Step 2: Check require ordering**

Verify that all `require` statements are at the top of the file:

```elisp
(require 'treesit)
(require 'prog-mode)
(require 'cl-lib)
(require 'quail)
(require 'transient)
```

**Step 3: Remove unused helper**

If `eucalypt--insert-char` from Task 7 is not used, remove it.

**Step 4: Run byte-compile check (if Emacs available)**

```
emacs -Q --batch -f batch-byte-compile editors/emacs/eucalypt-mode.el
```

This catches undefined function warnings, missing requires, and other
issues. If Emacs is not available on the build machine, skip this step.

**Step 5: Commit any fixes**

```
git commit -m "chore(emacs): final cleanup of unified eucalypt-mode"
```

---

## Task 15: Close Bead

**Step 1: Close the bead**

```
bd close eu-e3x3
```

**Step 2: Sync and push**

```
bd sync
git push
```

---

## Summary of Deliverables

| Task | Feature | Key Change |
|------|---------|------------|
| 1 | Rename to eucalypt-mode | Symbol rename, package header |
| 2 | Keymap | C-c C-k, C-c C-u bindings |
| 3 | Command integration | Render buffer/region, format inference |
| 4 | Project detection | Eufile root, project.el |
| 5 | Eglot LSP | `eu lsp` registration |
| 6 | Quail input method | ASCII → Unicode sequences |
| 7 | Transient menu | Browse/insert Unicode operators |
| 8 | Auto-unicodify | Batch replace ASCII → Unicode |
| 9 | Bracket colouring | Rainbow-delimiters, Unicode face |
| 10 | Bug fix: # in strings | syntax-propertize-function |
| 11 | Bug fix: metadata indent | Indent rule for metadata parent |
| 12 | Delete legacy | Remove eucalypt-ts-mode.el |
| 13 | Highlights query | Update prelude names |
| 14 | Final cleanup | Verify, byte-compile |
| 15 | Close bead | bd close eu-e3x3 |

## Task Dependencies

```
Task 1 (rename)
  └─→ Task 2 (keymap)
        └─→ Task 3 (commands)
              └─→ Task 4 (project detection)
                    └─→ Task 5 (eglot)
                          └─→ Task 6 (quail)
                                └─→ Task 7 (transient)
                                      └─→ Task 8 (unicodify)
                                            └─→ Task 9 (brackets)
                                                  └─→ Task 10 (bug: #)
                                                        └─→ Task 11 (bug: indent)
                                                              └─→ Task 12 (delete legacy)
                                                                    └─→ Task 13 (highlights)
                                                                          └─→ Task 14 (cleanup)
                                                                                └─→ Task 15 (close)
```

All tasks are sequential since they modify the same file.
