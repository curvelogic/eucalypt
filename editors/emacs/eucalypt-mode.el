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
;;                           nil "editors/tree-sitter-eucalypt/src"))
;;   (treesit-install-language-grammar 'eucalypt)
;;
;; Then add to your init file:
;;   (require 'eucalypt-mode)

;;; Code:

(require 'treesit)
(require 'prog-mode)
(require 'cl-lib)
(require 'quail)
(require 'transient)

;; Optional dependencies — declare to suppress byte-compile warnings.
;; These are loaded conditionally at runtime via `with-eval-after-load'
;; or `commandp' guards.
(defvar eglot-server-programs)
(declare-function rainbow-delimiters-mode "rainbow-delimiters" ())
(declare-function yaml-mode "yaml-mode" ())
(declare-function json-mode "json-mode" ())
(declare-function sp-local-pair "smartparens" (modes open close &rest keys))

;;; Group

(defgroup eucalypt nil
  "Major mode for editing Eucalypt files using tree-sitter."
  :prefix "eucalypt-"
  :group 'languages
  :link '(url-link :tag "GitHub" "https://github.com/curvelogic/eucalypt"))

;;; Customisation

(defcustom eucalypt-indent-offset 2
  "Number of spaces for each indentation level."
  :type 'integer
  :group 'eucalypt)

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

;;; Syntax table

;; We deliberately do NOT mark \" as a string delimiter because tree-sitter
;; handles all string detection, and the syntax table would otherwise interfere
;; with c\"...\" and r\"...\" prefix strings by incorrectly pairing quotes.
(defvar eucalypt-mode--syntax-table
  (let ((table (make-syntax-table)))
    ;; No comment syntax entries at all — tree-sitter handles all
    ;; highlighting.  The syntax table must not mark # as comment-start
    ;; because Emacs applies syntactic faces (comment-face) based on
    ;; the syntax table even in tree-sitter modes, and # inside string
    ;; literals would be misidentified.
    ;;
    ;; Quotes are just punctuation in tree-sitter mode
    (modify-syntax-entry ?\" "." table)
    (modify-syntax-entry ?\' "." table)
    ;; Brackets
    (modify-syntax-entry ?\( "()" table)
    (modify-syntax-entry ?\) ")(" table)
    (modify-syntax-entry ?\[ "(]" table)
    (modify-syntax-entry ?\] ")[" table)
    (modify-syntax-entry ?\{ "(}" table)
    (modify-syntax-entry ?\} "){" table)
    ;; Backtick is metadata, not a paired quote — treat as punctuation so
    ;; electric-pair-mode does not auto-insert a closing backtick.
    (modify-syntax-entry ?\` "." table)
    ;; Word constituents for identifiers
    (modify-syntax-entry ?_ "w" table)
    (modify-syntax-entry ?- "w" table)
    (modify-syntax-entry ?! "w" table)
    (modify-syntax-entry ?? "w" table)
    table)
  "Syntax table for `eucalypt-mode'.")

;;; Keymap

(defvar eucalypt-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-c\C-k" #'eucalypt-render-buffer)
    (define-key map "\C-c\C-u" #'eucalypt-unicode-menu)
    map)
  "Keymap for `eucalypt-mode'.")

;;; Font-lock

(defvar eucalypt-mode--font-lock-settings
  (treesit-font-lock-rules
   :language 'eucalypt
   :feature 'comment
   '((comment) @font-lock-comment-face)

   :language 'eucalypt
   :feature 'string
   '(;; Plain strings
     (string) @font-lock-string-face
     (string_content) @font-lock-string-face
     ;; C-strings (with C-style escapes)
     (c_string) @font-lock-string-face
     (c_string_content) @font-lock-string-face
     (c_escape_sequence) @font-lock-escape-face
     ;; Raw strings
     (r_string) @font-lock-string-face
     (r_string_content) @font-lock-string-face
     ;; Brace escapes ({{ and }} in plain/raw strings)
     (brace_escape) @font-lock-escape-face)

   :language 'eucalypt
   :feature 'number
   '((number) @font-lock-number-face)

   :language 'eucalypt
   :feature 'symbol
   '((symbol (identifier) @font-lock-constant-face)
     (symbol (quoted_identifier) @font-lock-constant-face))

   :language 'eucalypt
   :feature 'operator
   '((operator) @font-lock-operator-face)

   :language 'eucalypt
   :feature 'bracket
   '(["(" ")" "[" "]" "{" "}"] @font-lock-bracket-face
     [":" ","] @font-lock-delimiter-face
     ["`"] @font-lock-preprocessor-face
     ;; Idiot bracket expressions: ⟦ expr ⟧, «expr», etc.
     (bracket_expr) @eucalypt-unicode-bracket-face)

   :language 'eucalypt
   :feature 'interpolation
   '((interpolation
      "{" @font-lock-bracket-face
      "}" @font-lock-bracket-face)
     (format_spec) @font-lock-type-face)

   :language 'eucalypt
   :feature 'declaration
   '((declaration
      (declaration_head (identifier) @font-lock-variable-name-face))
     (declaration
      (declaration_head
       (identifier) @font-lock-function-name-face
       (parameter_list)))
     ;; Juxtaposed block definition: f{x y}: ...
     (declaration
      (declaration_head
       (identifier) @font-lock-function-name-face
       (block_param)))
     ;; Juxtaposed list definition: f[x, y]: ...
     (declaration
      (declaration_head
       (identifier) @font-lock-function-name-face
       (list_param))))

   :language 'eucalypt
   :feature 'operator-declaration
   '((operator_declaration
      (operator) @font-lock-function-name-face))

   :language 'eucalypt
   :feature 'parameter
   '(;; Simple parameters
     (parameter_list (identifier) @font-lock-variable-name-face)
     (operator_declaration (identifier) @font-lock-variable-name-face)
     ;; Block destructuring pattern: {x y} or {x: a  y: b}
     (block_pattern
      (declaration (declaration_head (identifier) @font-lock-variable-name-face)))
     ;; Fixed-length list destructuring pattern: [a, b, c]
     (list_pattern (identifier) @font-lock-variable-name-face)
     ;; Cons pattern: [h : t]
     (cons_pattern (identifier) @font-lock-variable-name-face)
     ;; Juxtaposed block param: f{x y}
     (block_param (identifier) @font-lock-variable-name-face)
     ;; Juxtaposed list param: f[x, y] or f[h : t]
     (list_param (identifier) @font-lock-variable-name-face))

   :language 'eucalypt
   :feature 'function-call
   '((application
      (name (identifier) @font-lock-function-call-face)))

   :language 'eucalypt
   :feature 'builtin
   :override t
   '(((identifier) @font-lock-builtin-face
      (:match "^__[A-Z]+" @font-lock-builtin-face)))

   :language 'eucalypt
   :feature 'keyword
   :override t
   '(((identifier) @font-lock-keyword-face
      (:match "^\\(if\\|then\\|when\\|cond\\|true\\|false\\|null\\|nil\\)$"
              @font-lock-keyword-face)))

   :language 'eucalypt
   :feature 'prelude
   '(((identifier) @font-lock-builtin-face
      (:match "^\\(cons\\|head\\|tail\\|first\\|second\\|map\\|filter\\|foldl\\|foldr\\|and\\|or\\|not\\|merge\\|concat\\|identity\\|const\\|compose\\|take\\|drop\\|take-while\\|drop-while\\|all\\|any\\|all-true\\?\\|any-true\\?\\|keys\\|values\\|lookup\\|has\\|range\\|repeat\\|iterate\\|cycle\\|zip\\|zip-with\\|reverse\\|remove\\|mapcat\\|group-by\\|qsort\\|partition\\|negate\\|inc\\|dec\\|floor\\|ceiling\\|max\\|min\\|abs\\|num\\|panic\\|assert\\|append\\|prepend\\|head-or\\|tail-or\\|second-or\\|flip\\|apply\\|scanl\\|scanr\\)$"
              @font-lock-builtin-face)))

   :language 'eucalypt
   :feature 'anaphor
   '((anaphor) @font-lock-type-face)

   :language 'eucalypt
   :feature 'metadata
   '((metadata) @font-lock-preprocessor-face
     ;; Block-valued unit metadata (non-docstring)
     (unit_metadata (block) @font-lock-doc-face))

   :language 'eucalypt
   :feature 'docstring
   '(;; String-valued metadata aliased to docstring by the grammar
     (docstring) @font-lock-doc-face))
  "Font-lock settings for `eucalypt-mode'.")

;;; Indentation

(defvar eucalypt-mode--indent-rules
  `((eucalypt
     ;; Top-level declarations at column 0
     ((parent-is "source_file") column-0 0)
     ;; Closing brackets must be checked BEFORE any parent-based rules,
     ;; otherwise `}' inside a block matches `(parent-is "block")' first
     ;; and gets indented by eucalypt-indent-offset instead of column 0.
     ((node-is "}") parent-bol 0)
     ((node-is "]") parent-bol 0)
     ((node-is ")") parent-bol 0)
     ;; Backtick metadata begins a declaration — align with declaration, not inside it
     ((node-is "metadata") parent-bol 0)
     ;; Content within a metadata annotation (e.g. block after backtick) — no indent
     ((parent-is "metadata") parent-bol 0)
     ;; The declaration_head that follows a metadata node must not be indented
     ;; further: it is a sibling of `metadata' within `declaration', so the
     ;; `(parent-is "declaration")' rule below would otherwise add an offset.
     ((node-is "declaration_head") parent-bol 0)
     ;; Inside blocks, indent declarations
     ((parent-is "block") parent-bol eucalypt-indent-offset)
     ;; Inside block destructuring patterns
     ((parent-is "block_pattern") parent-bol eucalypt-indent-offset)
     ;; Inside lists, indent elements
     ((parent-is "list") parent-bol eucalypt-indent-offset)
     ;; Inside idiot bracket expressions
     ((parent-is "bracket_expr") parent-bol eucalypt-indent-offset)
     ;; Inside parentheses, indent contents
     ((parent-is "paren_expr") parent-bol eucalypt-indent-offset)
     ((parent-is "argument_list") parent-bol eucalypt-indent-offset)
     ((parent-is "parameter_list") parent-bol eucalypt-indent-offset)
     ;; Soup continuation
     ((parent-is "soup") parent-bol eucalypt-indent-offset)
     ;; Declaration body continuation
     ((parent-is "declaration") parent-bol eucalypt-indent-offset)
     ;; Default: no change
     (no-node parent-bol 0)))
  "Indentation rules for `eucalypt-mode'.")

;;; Defun detection

(defun eucalypt-mode--defun-name (node)
  "Return the name of the declaration at NODE."
  (when (string= (treesit-node-type node) "declaration")
    (let ((head (treesit-node-child-by-field-name node "declaration_head")))
      (when head
        (let ((name (treesit-node-child head 0)))
          (when (string= (treesit-node-type name) "identifier")
            (treesit-node-text name t)))))))

;;; Syntax propertize — mark string boundaries for syntax-ppss
;;
;; The syntax table deliberately does not mark " as a string delimiter
;; (it would break c"..." and r"..." prefix strings).  But packages
;; like rainbow-delimiters use `syntax-ppss' to detect strings.  This
;; function walks tree-sitter string nodes and applies string-fence
;; syntax (class 15) to their opening and closing quotes so that
;; `syntax-ppss' correctly reports "inside string" for those regions.

(defun eucalypt-mode--syntax-propertize (start end)
  "Mark string delimiters with string-fence syntax between START and END.
Uses tree-sitter to find string nodes (string, c_string, r_string)
and applies syntax class 15 (string fence) to the opening and closing
quote characters.  This allows `syntax-ppss' to detect string context
without relying on the buffer syntax table."
  (let ((root (treesit-buffer-root-node)))
    (dolist (type '("string" "c_string" "r_string"))
      (dolist (node (treesit-query-capture
                     root
                     (format "((%s) @s)" type)
                     start end))
        (let* ((n (cdr node))
               (ns (treesit-node-start n))
               (ne (treesit-node-end n)))
          ;; Find the opening quote (last " before content)
          (when (>= ne start)
            ;; Opening quote: for c" and r", the quote is at ns+1; for plain ", at ns
            (let ((q-start (save-excursion
                             (goto-char ns)
                             (if (memq (char-after) '(?c ?r))
                                 (1+ ns)
                               ns))))
              (put-text-property q-start (1+ q-start)
                                 'syntax-table '(15 . nil))
              ;; Closing quote
              (put-text-property (1- ne) ne
                                 'syntax-table '(15 . nil)))))))))


;;; Command integration

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

;;; Project detection

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

;;; Render commands

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

;;; Eglot LSP integration

(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               '(eucalypt-mode "eu" "lsp")))

;;; Unicode input method (quail)

(quail-define-package
 "eucalypt" "UTF-8" "EU" t
 "Eucalypt Unicode operator input method.
Translates ASCII operator sequences to Unicode equivalents.

Key sequences:
  && → ∧  (logical and)
  || → ∨  (logical or)
  ~~ → ¬  (logical not)
  <= → ≤  (less-or-equal)
  >= → ≥  (greater-or-equal)
  != → ≠  (not-equal)
  /- → ∸  (unary minus)
  /% → ÷  (exact division)
  .. → ∘  (compose)
  ** → •  (bullet / anaphor)
  || → ‖  (cons operator — use |||| for ‖ after ∨)
  |> → ↑  (head prefix)
  !! → ✓  (non-nil check)
  ^^ → ⊕  (bitwise XOR)
  ~< → ≪  (left shift)
  ~> → ≫  (right shift)
  0N → ℕ  (natural numbers)
  {} → ∅  (empty set)
  << → «  >> → »  (angle brackets)
  (( → ⟨  )) → ⟩  (mathematical angle brackets)
  [[ → ⟦  ]] → ⟧  (double square brackets)"
 nil t nil nil nil nil nil nil nil nil t)

(quail-define-rules
 ;; Logical operators
 ("&&"  ?∧)
 ("||"  ?∨)
 ("~~"  ?¬)
 ;; Comparison
 ("<="  ?≤)
 (">="  ?≥)
 ("!="  ?≠)
 ;; Arithmetic
 ("/-"  ?∸)
 ("/%"  ?÷)
 ;; Bitwise
 ("^^"  ?⊕)
 ("~<"  ?≪)
 ("~>"  ?≫)
 ;; Composition
 (".."  ?∘)
 ;; Anaphora
 ("**"  ?•)
 ;; List operators
 ("|||" ?‖)
 ("|>"  ?↑)
 ("!!"  ?✓)
 ;; Sets
 ("{}"  ?∅)
 ;; Natural numbers
 ("0N"  ?ℕ)
 ;; Brackets
 ("(("  ?⟨)
 ("))"  ?⟩)
 ("[["  ?⟦)
 ("]]"  ?⟧)
 ("<<"  ?«)
 (">>"  ?»))

;;; Transient Unicode menu

(transient-define-prefix eucalypt-unicode-menu ()
  "Insert Eucalypt Unicode operators."
  ["Logical"
   ("a" "∧ and"        (lambda () (interactive) (insert "∧")))
   ("o" "∨ or"         (lambda () (interactive) (insert "∨")))
   ("n" "¬ not"        (lambda () (interactive) (insert "¬")))]
  ["Comparison"
   ("<" "≤ lte"        (lambda () (interactive) (insert "≤")))
   (">" "≥ gte"        (lambda () (interactive) (insert "≥")))
   ("!" "≠ neq"        (lambda () (interactive) (insert "≠")))]
  ["Arithmetic"
   ("-" "∸ negate"     (lambda () (interactive) (insert "∸")))
   ("d" "÷ divide"     (lambda () (interactive) (insert "÷")))]
  ["Bitwise"
   ("x" "⊕ xor"        (lambda () (interactive) (insert "⊕")))
   ("L" "≪ left-shift" (lambda () (interactive) (insert "≪")))
   ("R" "≫ right-shift" (lambda () (interactive) (insert "≫")))]
  ["List / Anaphor"
   ("b" "• bullet"     (lambda () (interactive) (insert "•")))
   ("C" "‖ cons"       (lambda () (interactive) (insert "‖")))
   ("h" "↑ head"       (lambda () (interactive) (insert "↑")))
   ("?" "✓ non-nil?"   (lambda () (interactive) (insert "✓")))]
  ["Other"
   ("c" "∘ compose"    (lambda () (interactive) (insert "∘")))
   ("e" "∅ empty-set"  (lambda () (interactive) (insert "∅")))
   ("N" "ℕ naturals"   (lambda () (interactive) (insert "ℕ")))])

;;; Auto-unicodify

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

;;; Bracket colouring

(defface eucalypt-unicode-bracket-face
  '((((background dark))
     :foreground "#e5c07b" :weight bold)
    (((background light))
     :foreground "#986801" :weight bold))
  "Face for Unicode bracket characters in Eucalypt mode."
  :group 'eucalypt)

(with-eval-after-load 'rainbow-delimiters
  (add-hook 'eucalypt-mode-hook #'rainbow-delimiters-mode))

;; Backtick is a metadata marker in eucalypt, not a paired quote.
;; Disable smartparens auto-pairing for ` in eucalypt buffers.
(with-eval-after-load 'smartparens
  (sp-local-pair 'eucalypt-mode "`" nil :actions nil))

;;; Markdown highlighting within docstrings

(defun eucalypt--apply-markdown-faces (start end)
  "Apply markdown faces within the region from START to END."
  ;; Inline code: `code`
  (save-excursion
    (goto-char start)
    (while (re-search-forward "`\\([^`]+\\)`" end t)
      (put-text-property (match-beginning 0) (match-end 0)
                         'face 'font-lock-constant-face)))
  ;; Bold: **text**
  (save-excursion
    (goto-char start)
    (while (re-search-forward "\\*\\*\\([^*]+\\)\\*\\*" end t)
      (put-text-property (match-beginning 1) (match-end 1)
                         'face 'bold)))
  ;; Italic: *text* (not preceded or followed by *)
  (save-excursion
    (goto-char start)
    (while (re-search-forward "\\(?:^\\|[^*]\\)\\(\\*\\([^*]+\\)\\*\\)\\(?:[^*]\\|$\\)" end t)
      (put-text-property (match-beginning 2) (match-end 2)
                         'face 'italic))))

(defun eucalypt--fontify-markdown-in-docstrings (beg end)
  "Apply markdown highlighting within docstring regions between BEG and END."
  (when (and (treesit-ready-p 'eucalypt t)
             (treesit-buffer-root-node 'eucalypt))
    (let ((root (treesit-buffer-root-node 'eucalypt)))
      ;; Direct docstring nodes (string-valued metadata, aliased by grammar)
      (dolist (cap (treesit-query-capture root '((docstring) @ds) beg end))
        (when (eq (car cap) 'ds)
          (eucalypt--apply-markdown-faces
           (treesit-node-start (cdr cap))
           (treesit-node-end (cdr cap))))))))

;;; Mode definition

;;;###autoload
(define-derived-mode eucalypt-mode prog-mode "Eucalypt"
  "Major mode for editing Eucalypt files, powered by tree-sitter.

Key bindings:
\\{eucalypt-mode-map}"
  :group 'eucalypt
  :syntax-table eucalypt-mode--syntax-table

  (unless (treesit-ready-p 'eucalypt)
    (error "Tree-sitter grammar for Eucalypt is not available"))

  ;; Tree-sitter setup
  (treesit-parser-create 'eucalypt)

  ;; Comments
  (setq-local comment-start "# ")
  (setq-local comment-end "")
  (setq-local comment-start-skip "#+ *")

  ;; Indentation
  (setq-local indent-tabs-mode nil)
  (setq-local tab-width eucalypt-indent-offset)
  (setq-local treesit-simple-indent-rules eucalypt-mode--indent-rules)

  ;; Font-lock
  (setq-local treesit-font-lock-settings eucalypt-mode--font-lock-settings)
  (setq-local treesit-font-lock-feature-list
              '((comment string)
                (number symbol keyword builtin)
                (declaration operator-declaration parameter function-call)
                (bracket operator interpolation anaphor metadata prelude docstring)))

  ;; Navigation
  (setq-local treesit-defun-type-regexp "declaration")
  (setq-local treesit-defun-name-function #'eucalypt-mode--defun-name)

  ;; Imenu
  (setq-local treesit-simple-imenu-settings
              '(("Declaration" "\\`declaration\\'" nil eucalypt-mode--defun-name)))

  ;; Syntax propertize — mark string boundaries for syntax-ppss
  ;; so rainbow-delimiters and other packages can detect strings
  (setq-local syntax-propertize-function
              #'eucalypt-mode--syntax-propertize)

  ;; Enable tree-sitter features
  (treesit-major-mode-setup)

  ;; Markdown highlighting within docstrings (layered on top of font-lock)
  (jit-lock-register #'eucalypt--fontify-markdown-in-docstrings))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.eu\\'" . eucalypt-mode))
;;;###autoload
(add-to-list 'auto-mode-alist '("\\.eucalypt\\'" . eucalypt-mode))
;;;###autoload
(add-to-list 'auto-mode-alist '("Eufile\\'" . eucalypt-mode))

(provide 'eucalypt-mode)

;;; eucalypt-mode.el ends here
