;;; eucalypt-ts-mode.el --- Tree-sitter support for Eucalypt -*- lexical-binding: t; -*-

;; Copyright © 2024 Greg Hawkins

;; Author: Greg Hawkins
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: languages, eucalypt, tree-sitter
;; URL: https://github.com/curvelogic/eucalypt

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This package provides tree-sitter based syntax highlighting and
;; indentation for Eucalypt files (.eu, .eucalypt).
;;
;; To use this mode, you need:
;; 1. Emacs 29.1 or later (with tree-sitter support)
;; 2. The tree-sitter-eucalypt grammar installed
;;
;; Installation of the grammar:
;;   (add-to-list 'treesit-language-source-alist
;;                '(eucalypt "https://github.com/curvelogic/tree-sitter-eucalypt"))
;;   (treesit-install-language-grammar 'eucalypt)
;;
;; Then add to your init file:
;;   (require 'eucalypt-ts-mode)

;;; Code:

(require 'treesit)
(require 'prog-mode)

(defgroup eucalypt-ts nil
  "Major mode for editing Eucalypt files using tree-sitter."
  :prefix "eucalypt-ts-"
  :group 'languages
  :link '(url-link :tag "GitHub" "https://github.com/curvelogic/eucalypt"))

(defcustom eucalypt-ts-indent-offset 2
  "Number of spaces for each indentation level."
  :type 'integer
  :group 'eucalypt-ts)

(defvar eucalypt-ts-mode--font-lock-settings
  (treesit-font-lock-rules
   :language 'eucalypt
   :feature 'comment
   '((comment) @font-lock-comment-face)

   :language 'eucalypt
   :feature 'string
   '((string) @font-lock-string-face
     (string_content) @font-lock-string-face
     (escape_sequence) @font-lock-escape-face)

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
     ["`"] @font-lock-preprocessor-face)

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
       (parameter_list))))

   :language 'eucalypt
   :feature 'operator-declaration
   '((operator_declaration
      (operator) @font-lock-function-name-face))

   :language 'eucalypt
   :feature 'parameter
   '((parameter_list (identifier) @font-lock-variable-name-face)
     (operator_declaration (identifier) @font-lock-variable-name-face))

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
      (:match "^\\(cons\\|head\\|tail\\|first\\|second\\|map\\|filter\\|foldl\\|foldr\\|and\\|or\\|not\\|merge\\|concat\\|identity\\|const\\|compose\\)$"
              @font-lock-builtin-face)))

   :language 'eucalypt
   :feature 'anaphor
   '((anaphor) @font-lock-type-face)

   :language 'eucalypt
   :feature 'metadata
   '((metadata) @font-lock-preprocessor-face
     (unit_metadata) @font-lock-doc-face))
  "Font-lock settings for `eucalypt-ts-mode'.")

(defvar eucalypt-ts-mode--indent-rules
  `((eucalypt
     ;; Top-level declarations at column 0
     ((parent-is "source_file") column-0 0)
     ;; Inside blocks, indent declarations
     ((parent-is "block") parent-bol eucalypt-ts-indent-offset)
     ;; Inside lists, indent elements
     ((parent-is "list") parent-bol eucalypt-ts-indent-offset)
     ;; Inside parentheses, indent contents
     ((parent-is "paren_expr") parent-bol eucalypt-ts-indent-offset)
     ((parent-is "argument_list") parent-bol eucalypt-ts-indent-offset)
     ((parent-is "parameter_list") parent-bol eucalypt-ts-indent-offset)
     ;; Soup continuation
     ((parent-is "soup") parent-bol eucalypt-ts-indent-offset)
     ;; Declaration body continuation
     ((parent-is "declaration") parent-bol eucalypt-ts-indent-offset)
     ;; Closing brackets align with opening
     ((node-is "}") parent-bol 0)
     ((node-is "]") parent-bol 0)
     ((node-is ")") parent-bol 0)
     ;; Default: no change
     (no-node parent-bol 0)))
  "Indentation rules for `eucalypt-ts-mode'.")

(defun eucalypt-ts-mode--defun-name (node)
  "Return the name of the declaration at NODE."
  (when (string= (treesit-node-type node) "declaration")
    (let ((head (treesit-node-child-by-field-name node "declaration_head")))
      (when head
        (let ((name (treesit-node-child head 0)))
          (when (string= (treesit-node-type name) "identifier")
            (treesit-node-text name t)))))))

;;;###autoload
(define-derived-mode eucalypt-ts-mode prog-mode "Eucalypt"
  "Major mode for editing Eucalypt files, powered by tree-sitter.

\\{eucalypt-ts-mode-map}"
  :group 'eucalypt-ts
  :syntax-table nil

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
  (setq-local tab-width eucalypt-ts-indent-offset)
  (setq-local treesit-simple-indent-rules eucalypt-ts-mode--indent-rules)

  ;; Font-lock
  (setq-local treesit-font-lock-settings eucalypt-ts-mode--font-lock-settings)
  (setq-local treesit-font-lock-feature-list
              '((comment string)
                (number symbol keyword builtin)
                (declaration operator-declaration parameter function-call)
                (bracket operator interpolation anaphor metadata prelude)))

  ;; Navigation
  (setq-local treesit-defun-type-regexp "declaration")
  (setq-local treesit-defun-name-function #'eucalypt-ts-mode--defun-name)

  ;; Imenu
  (setq-local treesit-simple-imenu-settings
              '(("Declaration" "\\`declaration\\'" nil eucalypt-ts-mode--defun-name)))

  ;; Enable tree-sitter features
  (treesit-major-mode-setup))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.eu\\'" . eucalypt-ts-mode))
;;;###autoload
(add-to-list 'auto-mode-alist '("\\.eucalypt\\'" . eucalypt-ts-mode))
;;;###autoload
(add-to-list 'auto-mode-alist '("Eufile\\'" . eucalypt-ts-mode))

(provide 'eucalypt-ts-mode)

;;; eucalypt-ts-mode.el ends here
