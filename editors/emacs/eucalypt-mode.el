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
(require 'compile)

;; Optional dependencies — declare to suppress byte-compile warnings.
;; These are loaded conditionally at runtime via `with-eval-after-load'
;; or `commandp' guards.
(defvar eglot-server-programs)
(declare-function rainbow-delimiters-mode "rainbow-delimiters" ())
(declare-function yaml-mode "yaml-mode" ())
(declare-function json-mode "json-mode" ())
(declare-function sp-local-pair "smartparens" (modes open close &rest keys))
(defvar treesit-fold-range-alist)
(declare-function treesit-fold-range-seq "treesit-fold" (node offset))

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
    (define-key map "\C-c\C-c" #'eucalypt-run)
    (define-key map "\C-c\C-r" #'eucalypt-run-target)
    (define-key map "\C-c\C-e" #'eucalypt-evaluate)
    (define-key map "\C-c\C-t" #'eucalypt-test)
    (define-key map "\C-c\C-v" #'eucalypt-check)
    (define-key map "\C-c\C-u" #'eucalypt-unicode-menu)
    map)
  "Keymap for `eucalypt-mode'.")

;;; Font-lock
;;
;; The highlight rules are not written here: they are derived from the
;; grammar's own `queries/highlights.scm' (editors/tree-sitter-eucalypt),
;; the single source of truth shared with other tree-sitter editors.  At
;; load time that file is split into patterns, each capture name
;; (`@keyword') is renamed to an Emacs face through
;; `eucalypt-mode--capture-faces', the standard predicates are rewritten
;; for Emacs, and the patterns are grouped by feature.

(defcustom eucalypt-highlights-file nil
  "Path of the tree-sitter `highlights.scm' the mode derives font-lock from.
When nil the mode looks next to `eucalypt-mode.el' (for a packaged copy) and
then in the grammar's `queries' directory of the eucalypt repository."
  :type '(choice (const :tag "Search" nil) file)
  :group 'eucalypt)

(defconst eucalypt-mode--capture-faces
  '(("comment"               font-lock-comment-face       comment)
    ("string"                font-lock-string-face        string)
    ("string.escape"         font-lock-escape-face        string)
    ("string.special"        font-lock-constant-face      string)
    ("number"                font-lock-number-face        number)
    ("keyword"               font-lock-keyword-face       keyword)
    ("function.builtin"      font-lock-builtin-face       builtin)
    ("constant"              font-lock-constant-face      symbol)
    ("operator"              font-lock-operator-face      operator)
    ("punctuation.delimiter" font-lock-delimiter-face     bracket)
    ("punctuation.bracket"   font-lock-bracket-face       bracket)
    ("punctuation.special"   font-lock-preprocessor-face  interpolation)
    ("variable"              font-lock-variable-use-face  interpolation)
    ("property"              font-lock-variable-name-face declaration)
    ("function"              font-lock-function-name-face declaration)
    ("parameter"             font-lock-variable-name-face parameter)
    ("function.call"         font-lock-function-call-face function-call)
    ("variable.special"      font-lock-type-face          anaphor)
    ("attribute"             font-lock-preprocessor-face  metadata)
    ("comment.documentation" font-lock-doc-face           docstring))
  "Map from `highlights.scm' capture names to (FACE FEATURE).
A pattern's FEATURE is that of its first capture; every capture is renamed
to its own FACE.  The order is the order rules are applied in: the prelude
names (`@keyword') come first so that the more specific rules for user
bindings, keys and symbols, which override, repaint them.")

(defconst eucalypt-mode--override-features
  '(keyword builtin symbol interpolation declaration parameter anaphor
    metadata docstring)
  "Features whose rules replace faces set by earlier rules.
`function-call' does not, so a call of a prelude name keeps its keyword
face.")

(defconst eucalypt-mode--replaced-nodes '("bracket_expr")
  "Node types whose whole-node patterns in `highlights.scm' are not used.
The Emacs-specific rule in `eucalypt-mode--extra-queries' replaces them: a
whole-node capture must not repaint the faces of the node's contents.")

(defconst eucalypt-mode--font-lock-feature-list
  '((comment string)
    (number symbol keyword builtin)
    (declaration parameter function-call)
    (bracket operator interpolation anaphor metadata docstring))
  "The `treesit-font-lock-feature-list' of `eucalypt-mode'.")

(defconst eucalypt-mode--extra-queries
  '(:language eucalypt :feature bracket
    ((bracket_expr) @eucalypt-unicode-bracket-face))
  "Emacs-specific addition to `highlights.scm': Unicode idiot brackets.")

(defun eucalypt-mode--highlights-file ()
  "Locate `highlights.scm', or signal an error."
  (let* ((here (file-name-directory
                (or load-file-name buffer-file-name
                    (locate-library "eucalypt-mode") "")))
         (file (or eucalypt-highlights-file
                   (seq-find #'file-readable-p
                             (list (expand-file-name "highlights.scm" here)
                                   (expand-file-name
                                    "../tree-sitter-eucalypt/queries/highlights.scm"
                                    here))))))
    (unless (and file (file-readable-p file))
      (signal 'file-missing
              (list "Cannot find eucalypt highlights.scm; set \
`eucalypt-highlights-file' (no syntax highlighting without it)")))
    file))

(defun eucalypt-mode--split-patterns (text)
  "Split the query source TEXT into its top-level patterns (strings).
Comments are dropped.  A pattern is one balanced form followed by any
captures or quantifiers attached to it."
  (let ((patterns nil) (pos 0) (len (length text)))
    (cl-flet ((read-form (start)
                ;; Return the end of the balanced form starting at START.
                (let ((i start) (depth 0) (done nil))
                  (while (and (not done) (< i len))
                    (let ((c (aref text i)))
                      (cond
                       ((eq c ?\") (setq i (1+ i))
                        (while (not (eq (aref text i) ?\"))
                          (when (eq (aref text i) ?\\) (setq i (1+ i)))
                          (setq i (1+ i)))
                        (setq i (1+ i))
                        (when (zerop depth) (setq done t)))
                       ((memq c '(?\( ?\[)) (setq depth (1+ depth) i (1+ i)))
                       ((memq c '(?\) ?\])) (setq depth (1- depth) i (1+ i))
                        (when (zerop depth) (setq done t)))
                       ((eq c ?\;) (while (and (< i len) (/= (aref text i) ?\n))
                                     (setq i (1+ i))))
                       (t (setq i (1+ i))))))
                  i)))
      (while (string-match "[^ \t\n]" text pos)
        (let ((start (match-beginning 0)))
          (if (eq (aref text start) ?\;)
              (setq pos (or (string-search "\n" text start) len))
            (let ((end (read-form start)))
              ;; Absorb trailing captures and quantifiers.
              (while (and (string-match "[ \t\n]*[@?*+][^ \t\n()]*" text end)
                          (= (match-beginning 0) end))
                (setq end (match-end 0)))
              (push (substring text start end) patterns)
              (setq pos end))))))
    (nreverse patterns)))

(defun eucalypt-mode--strip-comments (text)
  "Return TEXT without `;' comments (outside strings)."
  (with-temp-buffer
    (insert text)
    (goto-char (point-min))
    (while (not (eobp))
      (cond ((eq (char-after) ?\")
             (forward-char)
             (while (not (eq (char-after) ?\"))
               (when (eq (char-after) ?\\) (forward-char))
               (forward-char))
             (forward-char))
            ((eq (char-after) ?\;)
             (delete-region (point) (line-end-position)))
            (t (forward-char))))
    (buffer-string)))

(defun eucalypt-mode--translate-predicates (pattern)
  "Rewrite the standard predicates in PATTERN for Emacs.
`#eq?' becomes `#equal', `#match?' becomes `#match', and `#any-of?'
becomes a `#match' on an anchored alternation of the quoted strings."
  (let ((case-fold-search nil))
    (setq pattern
          (replace-regexp-in-string
           "(#eq\\?[ \t\n]+\\(@[^ )]+\\)[ \t\n]+" "(#equal \\1 " pattern t))
    (setq pattern
          (replace-regexp-in-string
           "(#match\\?[ \t\n]+\\(@[^ )]+\\)[ \t\n]+\\(\"[^\"]*\"\\)[ \t\n]*)" "(#match \\2 \\1)"
           pattern t))
    (while (string-match
            "(#any-of\\? \\(@[^ )]+\\)\\(\\(?:[ \t\n]+\"[^\"]*\"\\)+\\)[ \t\n]*)"
            pattern)
      (let ((capture (match-string 1 pattern))
            (from (match-beginning 0)) (to (match-end 0))
            (names nil) (args (match-string 2 pattern)) (i 0))
        (while (string-match "\"\\([^\"]*\\)\"" args i)
          (push (regexp-quote (match-string 1 args)) names)
          (setq i (match-end 0)))
        (setq pattern
              (concat (substring pattern 0 from)
                      (format "(#match %S %s)"
                              (concat "^\\(?:"
                                      (mapconcat #'identity (nreverse names)
                                                 "\\|")
                                      "\\)$")
                              capture)
                      (substring pattern to)))))
    pattern))

(defvar eucalypt-mode--unmapped-captures nil
  "Captures of `highlights.scm' with no entry in `eucalypt-mode--capture-faces'.")

(defun eucalypt-mode--rename-captures (pattern)
  "Return (FEATURE . PATTERN) with captures in PATTERN renamed to faces.
FEATURE is that of the first mapped capture, or nil when there is none.
Helper captures (`@_name') are left alone.  A pattern with an unmapped
capture is skipped (FEATURE nil) and the capture recorded in
`eucalypt-mode--unmapped-captures', so a capture added to the shared scm
for another editor cannot take the mode down."
  (let ((feature nil) (i 0) (out "") (ok t))
    (while (string-match "@\\([A-Za-z_][A-Za-z0-9_.-]*\\)" pattern i)
      (let* ((name (match-string 1 pattern))
             (entry (assoc name eucalypt-mode--capture-faces)))
        (setq out (concat out (substring pattern i (match-beginning 0))))
        (cond ((string-prefix-p "_" name) (setq out (concat out "@" name)))
              (entry (setq feature (or feature (nth 2 entry))
                           out (concat out "@" (symbol-name (nth 1 entry)))))
              (t (setq ok nil)
                 (cl-pushnew name eucalypt-mode--unmapped-captures
                             :test #'equal)))
        (setq i (match-end 0))))
    (cons (and ok feature) (concat out (substring pattern i)))))

(defvar eucalypt-mode--font-lock-settings nil
  "Font-lock settings for `eucalypt-mode', derived from `highlights.scm'.")

(defvar eucalypt-mode--font-lock-sources nil
  "Alist (FEATURE . QUERY-STRING) derived from `highlights.scm'.")

(defvar eucalypt-mode--doc-query nil
  "Query string of every `highlights.scm' pattern capturing documentation.
Whatever feature such a pattern belongs to, the markdown fontifier uses it.")

(defun eucalypt-mode--derive-sources ()
  "Derive the per-feature queries from `highlights.scm'.
Set `eucalypt-mode--font-lock-sources' and `eucalypt-mode--doc-query'.
The patterns of each feature are joined in file order; features are in the
order of `eucalypt-mode--capture-faces'."
  (let ((by-feature nil) (doc nil)
        (text (eucalypt-mode--strip-comments
               (with-temp-buffer
                 (insert-file-contents (eucalypt-mode--highlights-file))
                 (buffer-string)))))
    (setq eucalypt-mode--unmapped-captures nil)
    (dolist (pattern (eucalypt-mode--split-patterns text))
      (unless (string-match
               (concat "\\`(" (regexp-opt eucalypt-mode--replaced-nodes)
                       ")")
               pattern)
        (let ((renamed (eucalypt-mode--rename-captures
                        (eucalypt-mode--translate-predicates pattern))))
          (when (car renamed)
            (push (cdr renamed) (alist-get (car renamed) by-feature))
            (when (string-match-p "@font-lock-doc-face" (cdr renamed))
              (push (cdr renamed) doc))))))
    (dolist (entry eucalypt-mode--capture-faces)
      (let ((feature (nth 2 entry)))
        (when (and (alist-get feature by-feature)
                   (not (assq feature eucalypt-mode--font-lock-sources)))
          (push (cons feature (mapconcat #'identity
                                         (reverse (alist-get feature by-feature))
                                         "\n"))
                eucalypt-mode--font-lock-sources))))
    (setq eucalypt-mode--font-lock-sources
          (nreverse eucalypt-mode--font-lock-sources)
          eucalypt-mode--doc-query
          (mapconcat #'identity (nreverse doc) "\n"))
    (when eucalypt-mode--unmapped-captures
      (display-warning
       'eucalypt (format "highlights.scm captures %s have no face; add them to \
`eucalypt-mode--capture-faces'"
                         (mapconcat (lambda (c) (concat "@" c))
                                    eucalypt-mode--unmapped-captures ", "))))))

(defvar eucalypt-mode--warned-missing nil
  "Non-nil once the missing-`highlights.scm' warning has been shown.")

(defun eucalypt-mode--ensure-font-lock ()
  "Derive the font-lock settings from `highlights.scm' once.
When `highlights.scm' cannot be found the mode still loads, without
highlighting, and a warning says how to fix that."
  (unless eucalypt-mode--font-lock-settings
    (setq eucalypt-mode--font-lock-sources nil eucalypt-mode--doc-query nil)
    (condition-case err
        (progn
          (eucalypt-mode--derive-sources)
          (setq eucalypt-mode--font-lock-settings
                (append
                 (apply #'append
                        (mapcar
                         (lambda (source)
                           (treesit-font-lock-rules
                            :language 'eucalypt
                            :feature (car source)
                            :override (and (memq (car source)
                                                 eucalypt-mode--override-features)
                                           t)
                            (cdr source)))
                         eucalypt-mode--font-lock-sources))
                 (apply #'treesit-font-lock-rules
                        eucalypt-mode--extra-queries))))
      (file-missing
       (unless eucalypt-mode--warned-missing
         (setq eucalypt-mode--warned-missing t)
         (display-warning 'eucalypt (error-message-string err)))
       nil))))

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
Uses tree-sitter to find string nodes (string, c_string, r_string,
t_string, s_string)
and applies syntax class 15 (string fence) to the opening and closing
quote characters.  This allows `syntax-ppss' to detect string context
without relying on the buffer syntax table."
  (let ((root (treesit-buffer-root-node)))
    (dolist (type '("string" "c_string" "r_string" "t_string" "s_string"))
      (dolist (node (treesit-query-capture
                     root
                     (format "((%s) @s)" type)
                     start end))
        (let* ((n (cdr node))
               (ns (treesit-node-start n))
               (ne (treesit-node-end n)))
          ;; Find the opening quote (last " before content)
          (when (>= ne start)
            ;; Opening quote: for c", r", t", s", the quote is at ns+1; for plain ", at ns
            (let ((q-start (save-excursion
                             (goto-char ns)
                             (if (memq (char-after) '(?c ?r ?t ?s))
                                 (1+ ns)
                               ns))))
              (put-text-property q-start (1+ q-start)
                                 'syntax-table '(15 . nil))
              ;; Closing quote
              (put-text-property (1- ne) ne
                                 'syntax-table '(15 . nil)))))))))


;;; Command integration

;; Every command runs `eu' with `default-directory' set to the visited
;; file's directory and names the file relative to it, so imports and
;; error locations resolve from the file's location wherever Emacs was
;; started.  `NO_COLOR' keeps ANSI escape codes out of the output, and the
;; error output goes to a `compilation-mode' buffer so locations are
;; clickable and `next-error' works.

(defconst eucalypt--command-output-buffer
  "*Eucalypt Output*"
  "Name of buffer for `eu' command output.")

(defconst eucalypt--command-error-buffer
  "*Eucalypt Errors*"
  "Name of buffer for `eu' command error output.")

(defconst eucalypt--command-environment
  '("NO_COLOR=1" "TERM=dumb")
  "Environment entries added for every `eu' process.")

(defvar eucalypt--last-process nil
  "The most recent `eu' process started by `eucalypt-run'.")

(defvar eucalypt-evaluate-history nil
  "Minibuffer history for `eucalypt-evaluate'.")

(defvar eucalypt--targets nil
  "The targets `eucalypt-run-target' last listed.")

(defvar eucalypt-target-history nil
  "Minibuffer history for `eucalypt-run-target'.")

(defun eucalypt--executable ()
  "Return the `eu' executable, signalling an error if it cannot be found.
When `default-directory' is remote, `eu' is looked for on that host and
its name is returned local to it."
  (or (executable-find eucalypt-eu-command t)
      (error "Cannot find `%s' executable" eucalypt-eu-command)))

(defun eucalypt--global-args ()
  "Return `eucalypt-eu-global-opts' as a list of arguments."
  (split-string-shell-command eucalypt-eu-global-opts))

(defun eucalypt--form-command (opts)
  "Formulate a shell command line calling `eu' with OPTS."
  (mapconcat #'shell-quote-argument
             (append (list (eucalypt--executable))
                     (eucalypt--global-args)
                     eucalypt-eu-file-opts
                     (list opts))
             " "))

(defun eucalypt--infer-output-format-from-args (args &optional default)
  "Infer the output format from the argument list ARGS.
Recognises `-j', `--json', `-x FMT', `-xFMT', `--export FMT' and
`--export=FMT'; anything else, file names included, is ignored.
Without any of these, the format is DEFAULT, or `yaml'."
  (let ((format (or default 'yaml)))
    (while args
      (let ((arg (pop args)))
        (cond
         ((member arg '("-j" "--json")) (setq format 'json))
         ((and (member arg '("-x" "--export")) args)
          (setq format (intern (pop args))))
         ((string-match "\\`\\(?:-x\\|--export=\\)\\(.+\\)\\'" arg)
          (setq format (intern (match-string 1 arg)))))))
    format))

(defun eucalypt--infer-output-format (command)
  "Infer the output format from the shell command line COMMAND."
  (eucalypt--infer-output-format-from-args
   (split-string-shell-command command)))

(defun eucalypt--select-output-mode (format)
  "Select an appropriate major mode for FORMAT."
  (cond
   ((and (eq format 'yaml) (commandp 'yaml-mode)) #'yaml-mode)
   ((and (eq format 'yaml) (commandp 'yaml-ts-mode)
         (treesit-language-available-p 'yaml))
    #'yaml-ts-mode)
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

;;; Errors buffer

(defconst eucalypt--compilation-error-regexp
  (concat
   ;; A `warning' header on the line before marks a warning.  (`^' is
   ;; special only at the start of a regexp or an alternative.)
   "\\(?:^\\(warning\\)[^\n]*\n\\|^\\)"
   ;; codespan-reporting's location line: `┌─ FILE:LINE:COL'.
   "[ \t]*┌─ \\([^ \t\n:][^\n:]*\\):\\([0-9]+\\):\\([0-9]+\\)")
  "Regexp matching an `eu' diagnostic location.")

(defvar eucalypt-compilation-error-regexp-alist
  `((eucalypt ,eucalypt--compilation-error-regexp 2 3 4 (1)))
  "`compilation-error-regexp-alist' for `eucalypt-compilation-mode'.")

(define-compilation-mode eucalypt-compilation-mode "Eucalypt"
  "Mode for `eu' output, with clickable diagnostic locations."
  (setq-local compilation-error-regexp-alist-alist
              eucalypt-compilation-error-regexp-alist)
  (setq-local compilation-error-regexp-alist
              (mapcar #'car eucalypt-compilation-error-regexp-alist)))

(defun eucalypt--show-errors (directory)
  "Put the errors buffer into `eucalypt-compilation-mode' and show it.
DIRECTORY is where relative locations in the errors resolve from.
Does nothing if the errors buffer is empty."
  (when-let ((buf (get-buffer eucalypt--command-error-buffer)))
    (with-current-buffer buf
      (if (= (buffer-size) 0)
          nil
        (let ((inhibit-read-only t))
          (eucalypt-compilation-mode))
        (setq default-directory directory)
        (display-buffer buf)))))

(defun eucalypt--reset-buffer (name directory)
  "Return buffer NAME emptied, with `default-directory' DIRECTORY."
  (let ((buf (get-buffer-create name)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (fundamental-mode)
        (erase-buffer))
      ;; A previous run may have left the buffer in a read-only mode.
      (setq buffer-read-only nil)
      (setq default-directory directory))
    buf))

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

;;; File commands

(defun eucalypt--file ()
  "Return the visited file's name, saving the buffer first if asked.
Signals an error if the buffer is not visiting a file."
  (unless buffer-file-name
    (user-error "Buffer is not visiting a file; use `eucalypt-render-buffer'"))
  (when (and (buffer-modified-p)
             (y-or-n-p (format "Save %s first? " (buffer-name))))
    (save-buffer))
  buffer-file-name)

(defun eucalypt--file-directory (file)
  "Return the directory `eu' runs in for FILE."
  (file-name-directory (expand-file-name file)))

(defun eucalypt--run-args (file &rest extra)
  "Return the `eu' argument list to run FILE with EXTRA arguments.
FILE is named relative to its own directory."
  (append (eucalypt--global-args)
          eucalypt-eu-file-opts
          (list (file-name-nondirectory file))
          extra))

(defun eucalypt--edit-args (args)
  "Let the user edit the `eu' argument list ARGS in the minibuffer."
  (split-string-shell-command
   (read-shell-command
    "Run eu with: " (mapconcat #'shell-quote-argument args " "))))

(defun eucalypt--start (args directory &optional format)
  "Run `eu' with ARGS in DIRECTORY, rendering stdout and errors.
Standard output goes to `eucalypt--command-output-buffer' in a mode
for its FORMAT (a symbol; by default inferred from ARGS); standard
error goes to `eucalypt--command-error-buffer' in
`eucalypt-compilation-mode'.  A run still in progress is stopped
first.  DIRECTORY may be remote.  Returns the process."
  (when (process-live-p eucalypt--last-process)
    (set-process-sentinel eucalypt--last-process #'ignore)
    (delete-process eucalypt--last-process))
  ;; Its standard-error pipe too, so it writes nothing more.
  (when-let ((buf (get-buffer eucalypt--command-error-buffer))
             (pipe (get-buffer-process buf)))
    (delete-process pipe))
  (let* ((default-directory directory)
         (exe (eucalypt--executable))
         (format (or format (eucalypt--infer-output-format-from-args args)))
         (out (eucalypt--reset-buffer eucalypt--command-output-buffer directory))
         (err (eucalypt--reset-buffer eucalypt--command-error-buffer directory))
         (process-environment (append eucalypt--command-environment
                                      process-environment)))
    (setq eucalypt--last-process
          (make-process
           :name "eu"
           :buffer out
           :command (cons exe args)
           :stderr err
           :noquery t
           :connection-type 'pipe
           :file-handler t
           :sentinel
           (lambda (proc _event)
             (unless (process-live-p proc)
               ;; Let the stderr pipe drain before reading it.
               (when-let ((stderr (get-buffer-process err)))
                 (while (accept-process-output stderr 0.05)))
               (when (buffer-live-p out)
                 (with-current-buffer out
                   (funcall (eucalypt--select-output-mode format))
                   (setq default-directory directory))
                 (display-buffer out))
               (eucalypt--show-errors directory)
               (message "eu %s"
                        (if (zerop (process-exit-status proc))
                            "finished"
                          (format "exited with status %d"
                                  (process-exit-status proc))))))))
    ;; The standard-error pipe's default sentinel would write
    ;; "Process eu stderr finished" into the errors buffer.
    (when-let ((pipe (get-buffer-process err)))
      (set-process-sentinel pipe #'ignore))
    ;; Inputs come from the command line, never from standard input.
    (process-send-eof eucalypt--last-process)
    eucalypt--last-process))

(defun eucalypt-run (prefix)
  "Run `eu' on the visited file, from the file's directory.
With PREFIX argument, edit the arguments first."
  (interactive "P")
  (let* ((file (eucalypt--file))
         (args (eucalypt--run-args file)))
    (eucalypt--start (if prefix (eucalypt--edit-args args) args)
                     (eucalypt--file-directory file))))

(defun eucalypt--parse-targets (text)
  "Parse `eu list-targets' output TEXT into an alist.
Each entry is (NAME . PLIST), with :format and :doc keys."
  (let (targets)
    (with-temp-buffer
      (insert text)
      (goto-char (point-min))
      (let ((end (save-excursion
                   (if (re-search-forward "^From inputs" nil t)
                       (match-beginning 0)
                     (point-max)))))
        (while (re-search-forward
                (concat "^[ \t]*- \\([^ \t\n]+\\) "
                        "\\(?:(as: \\([^)]*\\)) \\)?"
                        "\\[path: [^]\n]*\\][ \t]*\\(.*\\)$")
                end t)
          (push (list (match-string 1)
                      :format (match-string 2)
                      :doc (string-trim (match-string 3)))
                targets))))
    (nreverse targets)))

(defun eucalypt-list-targets (file)
  "Return the targets FILE defines, as `eucalypt--parse-targets' does.
`eucalypt-eu-global-opts' are run options, so they are not passed."
  (let* ((default-directory (eucalypt--file-directory file))
         (process-environment (append eucalypt--command-environment
                                      process-environment))
         (errfile (make-nearby-temp-file "eu-list-targets")))
    (unwind-protect
        (with-temp-buffer
          (let ((status (process-file (eucalypt--executable) nil
                                      (list t errfile) nil
                                      "list-targets"
                                      (file-name-nondirectory file))))
            (unless (eql status 0)
              (error "eu list-targets failed (status %s): %s" status
                     (with-temp-buffer
                       (insert-file-contents errfile)
                       (string-trim (buffer-string)))))
            (eucalypt--parse-targets (buffer-string))))
      (delete-file errfile))))

(defun eucalypt-run-target (target &optional prefix)
  "Run `eu' on the visited file, rendering TARGET.
Interactively, complete TARGET from `eu list-targets'.  With PREFIX
argument, edit the arguments first."
  (interactive
   (let* ((targets (setq eucalypt--targets
                         (eucalypt-list-targets (eucalypt--file))))
          (annotate (lambda (name)
                      (let ((doc (plist-get (cdr (assoc name targets)) :doc)))
                        (if (and doc (not (string-empty-p doc)))
                            (concat "  " doc)
                          "")))))
     (unless targets
       (user-error "%s defines no targets" (buffer-name)))
     (list (let ((completion-extra-properties
                  (list :annotation-function annotate)))
             (completing-read "Target: " targets nil t nil
                              'eucalypt-target-history))
           current-prefix-arg)))
  (let* ((file (eucalypt--file))
         (args (eucalypt--run-args file "-t" target))
         (args (if prefix (eucalypt--edit-args args) args))
         ;; A target's own `format' sets its output format unless the
         ;; arguments choose one.
         (as (plist-get (cdr (assoc target eucalypt--targets)) :format)))
    (eucalypt--start args (eucalypt--file-directory file)
                     (eucalypt--infer-output-format-from-args
                      args (and as (intern as))))))

(defun eucalypt-evaluate (expression &optional prefix)
  "Evaluate EXPRESSION with `eu -e' against the visited file.
With PREFIX argument, edit the arguments first."
  (interactive
   (list (read-string "Evaluate: " nil 'eucalypt-evaluate-history)
         current-prefix-arg))
  (let* ((file (eucalypt--file))
         (args (eucalypt--run-args file "-e" expression)))
    (eucalypt--start (if prefix (eucalypt--edit-args args) args)
                     (eucalypt--file-directory file))))

(defun eucalypt--compile (subcommand prefix)
  "Run `eu SUBCOMMAND' on the visited file in `eucalypt-compilation-mode'.
With PREFIX, edit the command first.  Returns the compilation buffer.
`eucalypt-eu-global-opts' and `eucalypt-eu-file-opts' are run options
and inputs, so they are not passed."
  (let* ((file (eucalypt--file))
         (default-directory (eucalypt--file-directory file))
         (command (mapconcat #'shell-quote-argument
                             (list (eucalypt--executable) subcommand
                                   (file-name-nondirectory file))
                             " "))
         (command (if prefix (read-shell-command "Command: " command) command))
         ;; `compilation-start' keeps this in the buffer, so `recompile'
         ;; (`g') runs with it too.
         (compilation-environment (append eucalypt--command-environment
                                          compilation-environment)))
    (compilation-start command #'eucalypt-compilation-mode
                       (lambda (_mode) (format "*eucalypt %s*" subcommand)))))

(defun eucalypt-test (prefix)
  "Run `eu test' on the visited file, from the file's directory.
With PREFIX argument, edit the command first."
  (interactive "P")
  (eucalypt--compile "test" prefix))

(defun eucalypt-check (prefix)
  "Run `eu check' on the visited file, from the file's directory.
With PREFIX argument, edit the command first."
  (interactive "P")
  (eucalypt--compile "check" prefix))

;;; Render commands

(defun eucalypt--process-region (min max command)
  "Process region from MIN to MAX with COMMAND and display output.
The command runs in the visited file's directory, if any, so imports
resolve from the file's location."
  (let* ((directory (if buffer-file-name
                        (eucalypt--file-directory buffer-file-name)
                      default-directory))
         (default-directory directory)
         (process-environment (append eucalypt--command-environment
                                      process-environment))
         (output-format (eucalypt--infer-output-format command))
         (output-mode (eucalypt--select-output-mode output-format)))
    (eucalypt--reset-buffer eucalypt--command-error-buffer directory)
    (shell-command-on-region min max command
                             eucalypt--command-output-buffer
                             nil
                             eucalypt--command-error-buffer
                             t)
    (when-let ((buf (get-buffer eucalypt--command-output-buffer)))
      (with-current-buffer buf
        (funcall output-mode)))
    (eucalypt--show-errors directory)))

(defun eucalypt-render-buffer (prefix)
  "Process the entire buffer through `eu' and display output.
The buffer's text is piped to `eu', so unsaved changes are included.
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
  in → ∈  (set member)
  ni → ∉  (not set member)
  -o → ⊝  (bitwise NOT / circled dash)
  |= → ▶  (debug trace)
  << → «  >> → »  (angle brackets)
  (( → ⟨  )) → ⟩  (mathematical angle brackets)
  [[ → ⟦  ]] → ⟧  (double square brackets)
  |_ → ⌈  _| → ⌉  (ceiling brackets)
  |. → ⌊  .| → ⌋  (floor brackets)
  <. → ‹  .> → ›  (lens brackets)"
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
 ("in"  ?∈)
 ("ni"  ?∉)
 ;; Bitwise
 ("-o"  ?⊝)
 ;; Debug
 ("|="  ?▶)
 ;; Natural numbers
 ("0N"  ?ℕ)
 ;; Brackets
 ("(("  ?⟨)
 ("))"  ?⟩)
 ("[["  ?⟦)
 ("]]"  ?⟧)
 ("<<"  ?«)
 (">>"  ?»)
 ;; Ceiling / floor
 ("|_"  ?⌈)
 ("_|"  ?⌉)
 ("|."  ?⌊)
 (".|"  ?⌋)
 ;; Lens
 ("<."  ?‹)
 (".>"  ?›))

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
  ["Sets"
   ("e" "∅ empty-set"  (lambda () (interactive) (insert "∅")))
   ("i" "∈ member"     (lambda () (interactive) (insert "∈")))
   ("I" "∉ not-member" (lambda () (interactive) (insert "∉")))]
  ["Brackets"
   ("f" "⌊⌋ floor"     (lambda () (interactive) (insert "⌊") (save-excursion (insert "⌋"))))
   ("F" "⌈⌉ ceiling"   (lambda () (interactive) (insert "⌈") (save-excursion (insert "⌉"))))
   ("l" "‹› lens"      (lambda () (interactive) (insert "‹") (save-excursion (insert "›"))))
   ("[" "⟦⟧ idiot"     (lambda () (interactive) (insert "⟦") (save-excursion (insert "⟧"))))
   ("(" "⟨⟩ angle"     (lambda () (interactive) (insert "⟨") (save-excursion (insert "⟩"))))
   ("q" "«» guillemet" (lambda () (interactive) (insert "«") (save-excursion (insert "»"))))]
  ["Other"
   ("c" "∘ compose"    (lambda () (interactive) (insert "∘")))
   ("t" "▶ debug"      (lambda () (interactive) (insert "▶")))
   ("~" "⊝ bitnot"     (lambda () (interactive) (insert "⊝")))
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

;;; Code folding (treesit-fold)
;;
;; When the optional `treesit-fold' package is available, register fold
;; ranges for the main structural node types.  Install via:
;;   (package-install 'treesit-fold)     ; MELPA
;; or:
;;   (use-package treesit-fold :ensure t)
;;
;; Foldable constructs:
;;   block       — { declarations }
;;   list        — [ elements ]
;;   bracket_expr — ⟦ ... ⟧  (custom bracket pairs, including monadic blocks)

(defvar eucalypt-mode--treesit-fold-ranges
  '((block        . treesit-fold-range-seq)
    (list         . treesit-fold-range-seq)
    (bracket_expr . treesit-fold-range-seq))
  "Fold range rules for `eucalypt-mode', keyed by tree-sitter node type.")

(with-eval-after-load 'treesit-fold
  (add-to-list 'treesit-fold-range-alist
               (cons 'eucalypt-mode eucalypt-mode--treesit-fold-ranges)))

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
  "Apply markdown highlighting within docstring regions between BEG and END.
The docstrings are whatever `highlights.scm' captures as documentation
\(`@comment.documentation'), restricted to string nodes."
  (when (and eucalypt-mode--doc-query
             (treesit-ready-p 'eucalypt t)
             (treesit-buffer-root-node 'eucalypt))
    (dolist (cap (treesit-query-capture
                  (treesit-buffer-root-node 'eucalypt)
                  eucalypt-mode--doc-query beg end))
      (when (and (eq (car cap) 'font-lock-doc-face)
                 (member (treesit-node-type (cdr cap))
                         '("docstring" "string" "c_string" "r_string")))
        (eucalypt--apply-markdown-faces
         (treesit-node-start (cdr cap))
         (treesit-node-end (cdr cap)))))))

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
  (eucalypt-mode--ensure-font-lock)
  (setq-local treesit-font-lock-settings eucalypt-mode--font-lock-settings)
  (setq-local treesit-font-lock-feature-list
              eucalypt-mode--font-lock-feature-list)

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
