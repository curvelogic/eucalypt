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
;;   1. compiles every font-lock query in `eucalypt-mode--font-lock-settings'
;;      and every docstring query used by
;;      `eucalypt--fontify-markdown-in-docstrings' against the grammar, by
;;      running each against a parsed buffer (a query naming a node or
;;      structure the grammar lacks raises `treesit-query-error');
;;   2. fontifies each FILE at `treesit-font-lock-level' 1 to 4, failing on
;;      any error, and checks that the level-3 and level-4 features
;;      actually apply faces somewhere in the files given.
;;
;; Exits 0 when everything passes, 1 otherwise.

;;; Code:

(require 'cl-lib)
(require 'treesit)
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
      (dolist (query eucalypt--docstring-doc-queries)
        (setq count (1+ count))
        (eucalypt-check--try-query root "docstring doc query" (list query)))
      (princ (format "compiled %d queries\n" count)))))

(defconst eucalypt-check--expected-faces
  '((3 . (font-lock-variable-name-face font-lock-function-call-face))
    (4 . (font-lock-builtin-face font-lock-bracket-face
          font-lock-operator-face)))
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
    ("x: t\"2024-03-15\"\n" "2024-03-15" font-lock-constant-face))
  "Literals and the face each must carry at level 4.")

(defun eucalypt-check--literals ()
  "Check that each literal kind gets its face."
  (pcase-dolist (`(,source ,text ,face) eucalypt-check--literal-faces)
    (with-temp-buffer
      (insert source)
      (eucalypt-mode)
      (setq-local treesit-font-lock-level 4)
      (treesit-font-lock-recompute-features)
      (treesit-font-lock-fontify-region (point-min) (point-max))
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

(defun eucalypt-check-run ()
  "Entry point: check queries, then fontify `command-line-args-left'."
  (unless (treesit-language-available-p 'eucalypt)
    (princ "FAIL: eucalypt grammar not found on treesit-extra-load-path\n")
    (kill-emacs 1))
  (let ((files command-line-args-left))
    (setq command-line-args-left nil)
    (eucalypt-check--compile-queries)
    (eucalypt-check--literals)
    (when files (eucalypt-check--fontify files))
    (if (zerop eucalypt-check--failures)
        (progn (princ "PASS\n") (kill-emacs 0))
      (princ (format "%d failure(s)\n" eucalypt-check--failures))
      (kill-emacs 1))))

;;; eucalypt-mode-font-lock-check.el ends here
