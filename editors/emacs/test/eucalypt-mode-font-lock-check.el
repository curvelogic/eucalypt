;;; eucalypt-mode-font-lock-check.el --- Batch check of eucalypt-mode's tree-sitter queries -*- lexical-binding: t; -*-

;;; Commentary:

;; Run by scripts/check-emacs-mode.sh; not loaded by the mode itself.
;;
;; Usage:
;;   emacs --batch -Q -L editors/emacs -l eucalypt-mode-font-lock-check.el \
;;     -f eucalypt-check-run FILE.eu...
;;
;; `treesit-extra-load-path' must already name a directory holding the
;; compiled grammar (the script passes it with --eval).
;;
;; The check:
;;   0. the mode's capture-to-face table covers every capture
;;      highlights.scm uses, rules were derived, and their features match
;;      the mode's feature list;
;;   1. compiles every font-lock query in `eucalypt-mode--font-lock-settings'
;;      (derived from highlights.scm, which the markdown-in-docstrings
;;      fontifier also uses) against the grammar, by
;;      running each against a parsed buffer (a query naming a node or
;;      structure the grammar lacks raises `treesit-query-error');
;;   2. fontifies each FILE at `treesit-font-lock-level' 1 to 4, failing on
;;      any error, and checks that the level-3 and level-4 features
;;      actually apply faces somewhere in the files given;
;;   3. checks faces of literals, anaphora, juxtaposed calls and bracket
;;      delimiters, imenu names, the input method's rules, and that
;;      `indent-region' is idempotent on each FILE and leaves its string
;;      literals unchanged.
;;
;; Exits 0 when everything passes, 1 otherwise.

;;; Code:

(require 'cl-lib)
(require 'treesit)
(require 'imenu)
(require 'quail)
(require 'eucalypt-mode)

(defvar eucalypt-check--failures 0
  "Number of failures recorded so far.")

(defun eucalypt-check--fail (fmt &rest args)
  "Record a failure, reporting FMT with ARGS."
  (setq eucalypt-check--failures (1+ eucalypt-check--failures))
  (princ (concat "FAIL: " (apply #'format fmt args) "\n")))

(defun eucalypt-check--try-query (root label query)
  "Run QUERY against ROOT, recording a failure named LABEL on error."
  (condition-case err
      (treesit-query-capture root query)
    (error (eucalypt-check--fail "%s: %S" label err))))

(defun eucalypt-check--features ()
  "Check the derived rules and the feature list name the same features."
  (eucalypt-mode--ensure-font-lock)
  (unless eucalypt-mode--font-lock-settings
    (eucalypt-check--fail "no font-lock rules derived from highlights.scm"))
  (dolist (c eucalypt-mode--unmapped-captures)
    (eucalypt-check--fail "highlights.scm capture @%s has no face" c))
  (let ((listed (apply #'append eucalypt-mode--font-lock-feature-list))
        (used (delete-dups (mapcar (lambda (s) (nth 2 s))
                                   eucalypt-mode--font-lock-settings))))
    (dolist (f used)
      (unless (memq f listed)
        (eucalypt-check--fail "feature `%s' has rules but is in no level" f)))
    (dolist (f listed)
      (unless (memq f used)
        (eucalypt-check--fail "feature `%s' is in a level but has no rules" f)))))

(defun eucalypt-check--highlights-lookup ()
  "Check `highlights.scm' is found from a buffer visiting a file elsewhere.
The mode looked next to `buffer-file-name' (the `.eu' file), not next to
itself, when a buffer first entered it (eu-oaz72.8)."
  (let ((eucalypt-highlights-file nil)
        (load-file-name nil))
    (with-temp-buffer
      (setq buffer-file-name (expand-file-name "x.eu" temporary-file-directory))
      (condition-case err
          (eucalypt-mode--highlights-file)
        (error (eucalypt-check--fail "highlights.scm not found from %s: %S"
                                     buffer-file-name err)))
      (setq buffer-file-name nil))))

(defun eucalypt-check--compile-queries ()
  "Check that every query the mode uses compiles against the grammar."
  (with-temp-buffer
    (insert "x: 1\n")
    (treesit-parser-create 'eucalypt)
    (let ((root (treesit-buffer-root-node 'eucalypt))
          (count 0))
      ;; Each setting is (QUERY ENABLE FEATURE OVERRIDE); QUERY may be a
      ;; compiled query object or (lazily compiled) source.
      (dolist (setting eucalypt-mode--font-lock-settings)
        (setq count (1+ count))
        (eucalypt-check--try-query
         root (format "font-lock feature `%s'" (nth 2 setting)) (car setting)))
      (princ (format "compiled %d queries\n" count)))))

(defconst eucalypt-check--expected-faces
  '((2 . (font-lock-keyword-face))
    (3 . (font-lock-variable-name-face font-lock-function-call-face))
    (4 . (font-lock-bracket-face font-lock-operator-face
          font-lock-doc-face font-lock-preprocessor-face)))
  "Faces that must appear somewhere in the corpus at each level.")

(defun eucalypt-check--faces-in-buffer ()
  "Return the list of faces applied anywhere in the current buffer."
  (let ((faces nil) (pos (point-min)))
    (while (< pos (point-max))
      (let ((f (get-text-property pos 'face)))
        (dolist (x (if (listp f) f (list f)))
          (when (and x (symbolp x)) (cl-pushnew x faces))))
      (setq pos (next-single-property-change pos 'face nil (point-max))))
    faces))

(defun eucalypt-check--fontify (files)
  "Fontify each of FILES at every level, checking for errors and faces."
  (let ((seen (make-hash-table)))
    (dolist (level '(1 2 3 4))
      (puthash level nil seen))
    (dolist (file files)
      (with-temp-buffer
        (insert-file-contents file)
        (setq buffer-file-name nil)
        (eucalypt-mode)
        (dolist (level '(1 2 3 4))
          (setq-local treesit-font-lock-level level)
          (treesit-font-lock-recompute-features)
          (font-lock-mode 1)
          (condition-case err
              ;; Call the tree-sitter fontifier directly: through
              ;; font-lock-ensure, jit-lock would demote a query error to a
              ;; message and the check would pass.
              (progn
                (remove-text-properties (point-min) (point-max) '(face nil))
                (treesit-font-lock-fontify-region (point-min) (point-max))
                (eucalypt--fontify-markdown-in-docstrings
                 (point-min) (point-max)))
            (error (eucalypt-check--fail "%s at level %d: %S"
                                         file level err)))
          (puthash level
                   (append (eucalypt-check--faces-in-buffer)
                           (gethash level seen))
                   seen))))
    (pcase-dolist (`(,level . ,faces) eucalypt-check--expected-faces)
      (dolist (face faces)
        (unless (memq face (gethash level seen))
          (eucalypt-check--fail "no %s applied at level %d" face level))))
    (princ (format "fontified %d files at levels 1-4\n" (length files)))))

;; Each literal kind must be fontified: a grammar node the mode has no
;; rule for would otherwise fall back to the default face unnoticed (the
;; s_string node, eu-ioz21.7). Each entry is (SOURCE TEXT FACE): TEXT,
;; inside SOURCE, must carry FACE at every character.
(defconst eucalypt-check--literal-faces
  '(("x: \"plain\"\n" "plain" font-lock-string-face)
    ("x: c\"cee\"\n" "cee" font-lock-string-face)
    ("x: r\"raw\"\n" "raw" font-lock-string-face)
    ("x: s\"{a: number}\"\n" "s\"{a: number}\"" font-lock-string-face)
    ("x: t\"2024-03-15\"\n" "2024-03-15" font-lock-constant-face)
    ;; Faces that come from the capture mapping over highlights.scm.
    ("x: if(a, b, c)\n" "if" font-lock-keyword-face)
    ("f(a): g(a)\n" "g" font-lock-function-call-face)
    ("` {doc: \"hello\"}\nx: 1\n" "hello" font-lock-doc-face)
    ("` \"hello\"\nx: 1\n" "hello" font-lock-doc-face)
    ("x: \"a{b}\"\n" "b" font-lock-variable-use-face)
    ;; Emacs-specific addition, and markdown layered on docstrings.
    ("x: \u27e6 a \u27e7\n" "\u27e6" eucalypt-unicode-bracket-face)
    ("x: \u27e6 a \u27e7\n" "\u27e7" eucalypt-unicode-bracket-face)
    ;; Anaphora outside strings lex as identifiers (eu-oaz72.8).
    ("x: map(_ + 1)\n" "_" font-lock-type-face)
    ("x: map(_0 + 1)\n" "_0" font-lock-type-face)
    ("x: map(\u2022 + 1)\n" "\u2022" font-lock-type-face)
    ("x: map(\u20221 + 1)\n" "\u20221" font-lock-type-face)
    ;; Juxtaposed calls (eu-oaz72.8).
    ("x: f[1, 2]\n" "f" font-lock-function-call-face)
    ("x: g{a: 1}\n" "g" font-lock-function-call-face)
    ("` \"see `code`\"\nx: 1\n" "code" font-lock-constant-face)
    ;; Markdown also applies to doc strings in block-valued metadata.
    ("` {doc: \"see `code`\"}\nx: 1\n" "code" font-lock-constant-face)
    ("{ {doc: \"see `code`\"} x: 1 }\n" "code" font-lock-constant-face)
    ("{ doc: \"see `code`\" }\nx: 1\n" "code" font-lock-constant-face)
    ;; User bindings, keys and symbols keep their faces although they
    ;; share a name with a prelude function.
    ("f(key, value): key\n" "key" font-lock-variable-name-face)
    ("x: { first: 1 count: 3 }\n" "count" font-lock-variable-name-face)
    ("x: :count\n" "count" font-lock-constant-face)
    ("map(f, xs): xs\n" "map" font-lock-function-name-face)
    ;; Whole-node rules must not repaint the node's contents.
    ("x: \u27e6 \"s\" \u27e7\n" "s" font-lock-string-face)
    ("x: \u00ab f(1) \u00bb\n" "1" font-lock-number-face)
    ;; A call of a prelude name keeps its keyword face.
    ("x: map(f, xs)\n" "map" font-lock-keyword-face))
  "Literals and the face each must carry at level 4.")

;; Each entry is (SOURCE TEXT FACE): TEXT, inside SOURCE, must not carry
;; FACE anywhere.
(defconst eucalypt-check--absent-faces
  '(;; The bracket face is for the delimiters, not the contents.
    ("x: \u27e6 a \u27e7\n" "a" eucalypt-unicode-bracket-face)
    ("x: \u27e6 a b \u27e7\n" " a b " eucalypt-unicode-bracket-face)
    ;; An identifier merely starting with _ is not an anaphor.
    ("x: _x + 1\n" "_x" font-lock-type-face)
    ;; Nor is the name of a symbol.
    ("x: :_\n" "_" font-lock-type-face))
  "Text that must not carry a face at level 4.")

(defun eucalypt-check--fontify-level-4 (source)
  "Insert SOURCE into the current buffer and fontify it at level 4."
  (insert source)
  (eucalypt-mode)
  (setq-local treesit-font-lock-level 4)
  (treesit-font-lock-recompute-features)
  (treesit-font-lock-fontify-region (point-min) (point-max))
  (eucalypt--fontify-markdown-in-docstrings (point-min) (point-max)))

(defun eucalypt-check--literals ()
  "Check that each literal kind gets its face."
  (pcase-dolist (`(,source ,text ,face) eucalypt-check--literal-faces)
    (with-temp-buffer
      (eucalypt-check--fontify-level-4 source)
      (goto-char (point-min))
      (search-forward text)
      (let ((pos (match-beginning 0)))
        (while (< pos (match-end 0))
          (let ((f (get-text-property pos 'face)))
            (unless (memq face (if (listp f) f (list f)))
              (eucalypt-check--fail "%S: %S at %d has face %S, not %S"
                                    source text pos f face)
              (setq pos (match-end 0))))
          (setq pos (1+ pos))))))
  (princ (format "checked %d literal faces\n"
                 (length eucalypt-check--literal-faces))))

(defun eucalypt-check--absent ()
  "Check that no text in `eucalypt-check--absent-faces' carries its face."
  (pcase-dolist (`(,source ,text ,face) eucalypt-check--absent-faces)
    (with-temp-buffer
      (eucalypt-check--fontify-level-4 source)
      (goto-char (point-min))
      (search-forward text)
      (let ((pos (match-beginning 0)))
        (while (< pos (match-end 0))
          (let ((f (get-text-property pos 'face)))
            (when (memq face (if (listp f) f (list f)))
              (eucalypt-check--fail "%S: %S at %d has face %S"
                                    source text pos face)
              (setq pos (match-end 0))))
          (setq pos (1+ pos))))))
  (princ (format "checked %d absent faces\n"
                 (length eucalypt-check--absent-faces))))

;; Imenu and defun names (eu-oaz72.8: every declaration was `Anonymous').
(defconst eucalypt-check--imenu-source
  "f(x): x\ny: 2\n(a ++ b): a\n(! x): x\n` \"d\"\nz: 3\n\u27e6 x \u27e7: x\n\
(\u27e8 x \u27e9): x\n( \u27ea x \u27eb ): x\n'quoted name': 1\n\
ns: { inner: 1 }\n"
  "Source whose declarations `eucalypt-check--imenu' names.")

(defconst eucalypt-check--imenu-names
  '("f" "y" "++" "!" "z" "\u27e6\u27e7" "\u27e8\u27e9" "\u27ea\u27eb"
    "'quoted name'" "ns" "inner")
  "The names imenu must list for `eucalypt-check--imenu-source', in order.")

(defun eucalypt-check--imenu-flatten (index)
  "Return the names in the imenu INDEX, depth first, without placeholders."
  (let ((names nil))
    (dolist (entry index)
      (cond ((imenu--subalist-p entry)
             (unless (equal (car entry) "Declaration")
               (push (car entry) names))
             (setq names (append (reverse (eucalypt-check--imenu-flatten
                                           (cdr entry)))
                                 names)))
            ((not (equal (car entry) " ")) (push (car entry) names))))
    (nreverse names)))

(defun eucalypt-check--imenu ()
  "Check that imenu names every declaration."
  (with-temp-buffer
    (insert eucalypt-check--imenu-source)
    (eucalypt-mode)
    (let ((names (eucalypt-check--imenu-flatten
                  (funcall imenu-create-index-function))))
      (unless (equal names eucalypt-check--imenu-names)
        (eucalypt-check--fail "imenu names %S, not %S"
                              names eucalypt-check--imenu-names))))
  (princ "checked imenu names\n"))

;; Indentation (eu-oaz72.8): `indent-region' is idempotent and leaves the
;; contents of multi-line strings alone.
(defconst eucalypt-check--indent-cases
  '(;; A top-level bracket-pair declaration drifted 2 columns per pass.
    ("` \"d\"\n\u27e6{}\u27e7: { a: 1 }\n" . "` \"d\"\n\u27e6{}\u27e7: { a: 1 }\n")
    ("m: {\n\u27e6 x \u27e7: x\n    \u27e8 x \u27e9: x\n}\n"
     . "m: {\n  \u27e6 x \u27e7: x\n  \u27e8 x \u27e9: x\n}\n")
    ;; A docstring's continuation lines are its contents.
    ("` \"first\n      second\n third\"\nx: 1\n"
     . "` \"first\n      second\n third\"\nx: 1\n")
    ;; Elements align with a first element on the opener's line.
    ("` { target: :t\nimport: \"a\" }\nx: 1\n"
     . "` { target: :t\n    import: \"a\" }\nx: 1\n")
    ("x: [ a\n, b\n]\n" . "x: [ a\n   , b\n   ]\n")
    ;; A comment after the opener is not a first element.
    ("x: { # c\na: 1\nb: 2\n}\n" . "x: { # c\n  a: 1\n  b: 2\n}\n")
    ;; A comment between declarations stays with the next one.
    ("a: 1\n\n  ## c\nb: 2\n" . "a: 1\n\n## c\nb: 2\n")
    ("` \"d\"\n  # c\nb: 2\n" . "` \"d\"\n# c\nb: 2\n"))
  "Pairs (SOURCE . INDENTED): `indent-region' must turn SOURCE into INDENTED.")

(defun eucalypt-check--string-texts ()
  "Return the text of every string literal in the current buffer."
  (mapcar (lambda (c) (treesit-node-text (cdr c) t))
          (treesit-query-capture
           (treesit-buffer-root-node)
           (mapconcat (lambda (type) (format "(%s) @s" type))
                      '("string" "c_string" "r_string" "t_string"
                        "s_string" "docstring")
                      " "))))

(defun eucalypt-check--indent (files)
  "Check indentation of `eucalypt-check--indent-cases' and of FILES."
  (pcase-dolist (`(,source . ,expected) eucalypt-check--indent-cases)
    (with-temp-buffer
      (insert source)
      (eucalypt-mode)
      (dotimes (pass 2)
        (indent-region (point-min) (point-max))
        (unless (equal (buffer-string) expected)
          (eucalypt-check--fail "indent pass %d of %S gave %S, not %S"
                                (1+ pass) source (buffer-string) expected)))))
  (dolist (file files)
    (with-temp-buffer
      (insert-file-contents file)
      (eucalypt-mode)
      (let ((strings (eucalypt-check--string-texts))
            (inhibit-message t)
            once)
        (indent-region (point-min) (point-max))
        (setq once (buffer-string))
        (unless (equal (eucalypt-check--string-texts) strings)
          (eucalypt-check--fail "%s: indent-region changed a string literal"
                                file))
        (indent-region (point-min) (point-max))
        (unless (equal (buffer-string) once)
          (eucalypt-check--fail "%s: indent-region is not idempotent" file)))))
  (princ (format "checked indentation of %d cases and %d files\n"
                 (length eucalypt-check--indent-cases) (length files))))

;; The input method must not translate ordinary code (eu-oaz72.8).
(defconst eucalypt-check--quail-untranslated
  '("in" "ni" "-o" "[[" "]]" "((" "))" "{}" "!!" "<<" ">>" ".." "**" "\\ni")
  "Key sequences that are eucalypt code and must not translate.")

(defconst eucalypt-check--quail-translated
  '(("&&" . ?\u2227) ("|||" . ?\u2016) ("\\in" . ?\u2208) ("\\!in" . ?\u2209)
    ("\\[[" . ?\u27e6) ("\\{}" . ?\u2205) ("\\!!" . ?\u2713) ("\\<<" . ?\u00ab))
  "Key sequences and the character each must translate to.")

(defun eucalypt-check--quail ()
  "Check the `eucalypt' input method's rules."
  (with-temp-buffer
    (activate-input-method "eucalypt")
    (dolist (keys eucalypt-check--quail-untranslated)
      (let ((map (quail-lookup-key keys)))
        (when (and (consp map) (car map))
          (eucalypt-check--fail "input method translates %S" keys))))
    (pcase-dolist (`(,keys . ,char) eucalypt-check--quail-translated)
      (let ((map (quail-lookup-key keys)))
        (unless (and (consp map) (eq (car map) char))
          (eucalypt-check--fail "input method gives %S for %S, not %c"
                                (and (consp map) (car map)) keys char)))))
  ;; Nothing translates inside a string or comment: `eucalypt--input-method'
  ;; passes the key through there and hands it to quail elsewhere.
  (pcase-dolist (`(,source ,before ,text-p)
                 '(("x: \"\\\\((d)\\\\)\"\n" "((d" t)
                   ("x: c\"a\" # \\\\in\n" "in" t)
                   ("x: a\n" "a" nil)
                   ;; A string still being typed (eu-oaz72.8 round 2).
                   ("x: f(\"\\\\(()\n" "\\\\(()" t)
                   ("x: [\"\\\\(]\n" "\\\\(]" t)
                   ("x: { a: \"\\\\( }\n" "\\\\( }" t)
                   ("x: re-match(\"\\\\(" "\\\\(" t)
                   ("x: \"" "\"" nil)
                   ;; At the end of the buffer the parser closes the string
                   ;; with a missing quote (round 3).
                   ("x: \"a" "" t)
                   ("x: \"\\\\d+" "" t)
                   ("x: c\"a\\\"b" "" t)
                   ;; A quoted name holding a quote, on an erroneous line.
                   ("x: f('a\"b', c && d" "&& d" nil)
                   ;; Code in an interpolation, also while typing.
                   ("x: \"{a && b}\"\n" "&& b" nil)
                   ("x: \"{a && b" "&& b" nil)
                   ;; Code after a closed string on an erroneous line.
                   ("x: f(\"s\", a && b\n" "&& b" nil)
                   ("x: \"s\" \n" " \n" nil)))
    (with-temp-buffer
      (insert source)
      (eucalypt-mode)
      (activate-input-method "eucalypt")
      (unless (eq input-method-function #'eucalypt--input-method)
        (eucalypt-check--fail "input method not wrapped: %S"
                              input-method-function))
      (if (string-empty-p before)
          (goto-char (point-max))
        (goto-char (point-min))
        (search-forward before)
        (goto-char (match-beginning 0)))
      (unless (eq (and (eucalypt--in-string-or-comment-p) t) text-p)
        (eucalypt-check--fail "%S at %S: in string or comment should be %S"
                              source before text-p))
      ;; Only where the key should pass through: elsewhere quail would wait
      ;; for more keys, which a batch Emacs reads from stdin.
      (when (and text-p (eucalypt--in-string-or-comment-p))
        (unless (equal (eucalypt--input-method ?\\) '(?\\))
          (eucalypt-check--fail "%S: input method translates in text"
                                source)))))
  (princ "checked input method\n"))

(defun eucalypt-check-run ()
  "Entry point: check queries, then fontify `command-line-args-left'."
  (unless (treesit-language-available-p 'eucalypt)
    (princ "FAIL: eucalypt grammar not found on treesit-extra-load-path\n")
    (kill-emacs 1))
  (let ((files command-line-args-left))
    (setq command-line-args-left nil)
    (eucalypt-check--highlights-lookup)
    (eucalypt-check--features)
    (eucalypt-check--compile-queries)
    (eucalypt-check--literals)
    (eucalypt-check--absent)
    (eucalypt-check--imenu)
    (eucalypt-check--indent files)
    (eucalypt-check--quail)
    (when files (eucalypt-check--fontify files))
    (if (zerop eucalypt-check--failures)
        (progn (princ "PASS\n") (kill-emacs 0))
      (princ (format "%d failure(s)\n" eucalypt-check--failures))
      (kill-emacs 1))))

;;; eucalypt-mode-font-lock-check.el ends here
