;;; eucalypt-mode-commands-test.el --- ERT tests for eucalypt-mode's eu commands -*- lexical-binding: t; -*-

;;; Commentary:

;; Run by scripts/check-emacs-mode.sh; not loaded by the mode itself.
;;
;; Usage:
;;   emacs --batch -Q -L editors/emacs \
;;     -l editors/emacs/test/eucalypt-mode-commands-test.el \
;;     -f ert-run-tests-batch-and-exit
;;
;; The tests need no network, no grammar and no built `eu': they drive
;; the commands against a stub `eu' (a shell script written to a
;; temporary directory) that records its arguments, working directory
;; and environment, prints coloured output unless NO_COLOR is set, and
;; reports a codespan-style error location.
;;
;; When the environment variable EUCALYPT_TEST_EU names a real `eu'
;; binary, the tests tagged `real-eu' also run the commands against it.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'tramp)
(require 'tramp-sh)
(require 'eucalypt-mode)

(defconst eucalypt-test--stub
  "#!/bin/sh
log=\"$EUCALYPT_STUB_LOG\"
{ printf 'cwd=%s\\n' \"$(pwd)\"
  printf 'no_color=%s\\n' \"${NO_COLOR-unset}\"
  for a in \"$@\"; do printf 'arg=%s\\n' \"$a\"; done
  if [ \"$1\" = eu@- ]; then printf 'stdin=%s\\n' \"$(tr '\\n' '|')\"; fi
  printf 'end\\n'; } >> \"$log\"
if [ -n \"$NO_COLOR\" ]; then red=''; off=''; else red=$(printf '\\033[31m'); off=$(printf '\\033[0m'); fi
case \"$*\" in *slow*) sleep 1; printf 'late\\n'; printf 'late\\n' >&2; exit 0 ;; esac
case \"$1\" in
  list-targets)
    if [ \"$2\" = broken.eu ]; then printf 'boom\\n' >&2; exit 1; fi
    printf 'Available targets\\n\\n'
    printf ' - bar [path: bar]  \\n'
    printf ' - foo (as: json) [path: foo]  the foo\\n'
    printf '\\nFrom inputs\\n\\n  - eu@[prelude]\\n  - eu@x.eu\\n'
    exit 0 ;;
  test|check)
    printf '%sm...PASS%s\\n' \"$red\" \"$off\"
    printf '%serror%s: unresolved variable\\n  \\342\\224\\214\\342\\224\\200 m.eu:2:4\\n' \"$red\" \"$off\"
    exit 1 ;;
esac
printf 'a: 1\\n'
printf '%serror%s: unresolved variable '\"'\"'nosuch'\"'\"'\\n' \"$red\" \"$off\" >&2
printf '  \\342\\224\\214\\342\\224\\200 m.eu:2:4\\n' >&2
exit 1
"
  "A stub `eu' that logs how it was called.")

(defmacro eucalypt-test--with-stub (&rest body)
  "Run BODY with a stub `eu' and a project in a temporary directory.
Binds `dir' (the project's directory, holding m.eu), `log' (the stub's
log file) and `elsewhere' (an unrelated directory BODY runs from)."
  (declare (indent 0) (debug t))
  `(let* ((root (file-name-as-directory (make-temp-file "eucalypt-test" t)))
          (dir (file-name-as-directory (expand-file-name "proj/sub" root)))
          (elsewhere (file-name-as-directory (expand-file-name "elsewhere" root)))
          (log (expand-file-name "stub.log" root))
          (stub (expand-file-name "eu-stub" root))
          (process-environment (cons (concat "EUCALYPT_STUB_LOG=" log)
                                     process-environment))
          (eucalypt-eu-command stub)
          (eucalypt-eu-global-opts "")
          (eucalypt-eu-file-opts nil))
     (unwind-protect
         (progn
           (make-directory dir t)
           (make-directory elsewhere t)
           (with-temp-file stub (insert eucalypt-test--stub))
           (set-file-modes stub #o755)
           (with-temp-file (expand-file-name "m.eu" dir)
             (insert "a: 1\nb: nosuch\n"))
           (let ((default-directory elsewhere))
             ,@body))
       (dolist (b (list eucalypt--command-output-buffer
                        eucalypt--command-error-buffer
                        "*eucalypt test*" "*eucalypt check*"))
         (when (get-buffer b) (kill-buffer b)))
       (dolist (b (buffer-list))
         (when (and (buffer-file-name b)
                    (string-prefix-p root (buffer-file-name b)))
           (kill-buffer b)))
       (delete-directory root t))))

(defun eucalypt-test--log (log)
  "Return the calls recorded in LOG, each an alist of (KEY . VALUE)."
  (let (calls current)
    (with-temp-buffer
      (insert-file-contents log)
      (dolist (line (split-string (buffer-string) "\n" t))
        (if (equal line "end")
            (progn (push (nreverse current) calls) (setq current nil))
          (when (string-match "\\`\\([a-z_]+\\)=\\(.*\\)\\'" line)
            (push (cons (match-string 1 line) (match-string 2 line))
                  current)))))
    (nreverse calls)))

(defun eucalypt-test--args (call)
  "Return the argument list recorded in CALL."
  (mapcar #'cdr (cl-remove-if-not (lambda (e) (equal (car e) "arg")) call)))

(defun eucalypt-test--wait (proc)
  "Wait for PROC and its sentinel to finish."
  (while (process-live-p proc)
    (accept-process-output proc 0.1))
  ;; Let the sentinel run.
  (accept-process-output nil 0.1))

(defun eucalypt-test--visit (dir)
  "Visit m.eu in DIR in `fundamental-mode', returning the buffer.
The commands do not need the major mode (or the grammar) to run.  The
buffer's `default-directory' is set to DIR's parent, as after `cd', so
the tests show the commands run from the file's directory regardless."
  (let* ((auto-mode-alist nil)
         (buf (find-file-noselect (expand-file-name "m.eu" dir))))
    (with-current-buffer buf
      (setq default-directory
            (file-name-directory (directory-file-name dir))))
    buf))

(defun eucalypt-test--goto-first-error (buffer)
  "Visit the first error in BUFFER; return (FILE . LINE) reached.
Fails, rather than prompting, if the location does not resolve to a file."
  (cl-letf (((symbol-function 'read-file-name)
             (lambda (&rest _) (error "Error location did not resolve"))))
    (with-current-buffer buffer
      (goto-char (point-min))
      (compilation-next-error 1)
      (compile-goto-error)
      (cons (buffer-file-name) (line-number-at-pos)))))

;;; Output-format inference

(ert-deftest eucalypt-test-infer-output-format ()
  "Output format comes from option tokens, never from file names."
  (should (eq (eucalypt--infer-output-format "eu /tmp/a-j/x.eu") 'yaml))
  (should (eq (eucalypt--infer-output-format "eu /tmp/x-json.eu") 'yaml))
  (should (eq (eucalypt--infer-output-format "eu x.eu -j") 'json))
  (should (eq (eucalypt--infer-output-format "eu --json x.eu") 'json))
  (should (eq (eucalypt--infer-output-format "eu -x toml x.eu") 'toml))
  (should (eq (eucalypt--infer-output-format "eu -xtext x.eu") 'text))
  (should (eq (eucalypt--infer-output-format "eu --export=edn x.eu") 'edn))
  (should (eq (eucalypt--infer-output-format "eu '/tmp/my -j dir/x.eu'") 'yaml)))

;;; Target parsing

(ert-deftest eucalypt-test-parse-targets ()
  "`eu list-targets' output parses to names, formats and docs."
  (let ((targets (eucalypt--parse-targets
                  (concat "Available targets\n\n"
                          " - bar [path: bar]  \n"
                          " - foo (as: json) [path: a.foo]  the foo\n"
                          "\nFrom inputs\n\n  - eu@x.eu\n"))))
    (should (equal (mapcar #'car targets) '("bar" "foo")))
    (should (equal (plist-get (cdr (assoc "foo" targets)) :format) "json"))
    (should (equal (plist-get (cdr (assoc "foo" targets)) :doc) "the foo"))
    (should (null (plist-get (cdr (assoc "bar" targets)) :format)))))

;;; Error locations

(ert-deftest eucalypt-test-error-regexp ()
  "The error regexp finds codespan locations and marks warnings."
  (with-temp-buffer
    (insert "warning: type mismatch calling 'f'\n  ┌─ ty.eu:3:6\n  │\n"
            "error: unresolved variable 'nosuch'\n  ┌─ sub/imp.eu:12:4\n")
    (goto-char (point-min))
    (should (re-search-forward eucalypt--compilation-error-regexp nil t))
    (should (equal (match-string 1) "warning"))
    (should (equal (match-string 2) "ty.eu"))
    (should (equal (match-string 3) "3"))
    (should (equal (match-string 4) "6"))
    (should (re-search-forward eucalypt--compilation-error-regexp nil t))
    (should (null (match-string 1)))
    (should (equal (match-string 2) "sub/imp.eu"))
    (should (equal (match-string 3) "12"))))

;;; Commands against the stub

(ert-deftest eucalypt-test-run-from-file-directory ()
  "`eucalypt-run' runs on the file from its directory without colour."
  (eucalypt-test--with-stub
    (with-current-buffer (eucalypt-test--visit dir)
      (eucalypt-test--wait (eucalypt-run nil)))
    (let ((call (car (eucalypt-test--log log))))
      (should (equal (cdr (assoc "cwd" call)) (directory-file-name dir)))
      (should (equal (cdr (assoc "no_color" call)) "1"))
      (should (equal (eucalypt-test--args call) '("m.eu"))))
    (with-current-buffer eucalypt--command-output-buffer
      (should (equal (buffer-string) "a: 1\n")))
    (let ((errors (get-buffer eucalypt--command-error-buffer)))
      (with-current-buffer errors
        (should (derived-mode-p 'eucalypt-compilation-mode))
        (should-not (string-search "\e[" (buffer-string)))
        (should (string-search "unresolved variable" (buffer-string)))
        (should-not (string-search "Process" (buffer-string))))
      (should (equal (eucalypt-test--goto-first-error errors)
                     (cons (expand-file-name "m.eu" dir) 2))))))

(ert-deftest eucalypt-test-run-target ()
  "`eucalypt-run-target' lists targets and passes `-t' from the file's directory."
  (eucalypt-test--with-stub
    (with-current-buffer (eucalypt-test--visit dir)
      (should (equal (mapcar #'car (eucalypt-list-targets buffer-file-name))
                     '("bar" "foo")))
      (eucalypt-test--wait (eucalypt-run-target "foo")))
    (let ((calls (eucalypt-test--log log)))
      (should (equal (eucalypt-test--args (nth 0 calls)) '("list-targets" "m.eu")))
      (should (equal (cdr (assoc "cwd" (nth 0 calls))) (directory-file-name dir)))
      (should (equal (eucalypt-test--args (nth 1 calls)) '("m.eu" "-t" "foo")))
      (should (equal (cdr (assoc "cwd" (nth 1 calls))) (directory-file-name dir))))))

(ert-deftest eucalypt-test-run-target-format ()
  "A target's `(as: FMT)' sets the output mode unless the arguments choose one."
  (eucalypt-test--with-stub
    (with-current-buffer (eucalypt-test--visit dir)
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (&rest _) "foo")))
        (eucalypt-test--wait (call-interactively #'eucalypt-run-target)))
      (with-current-buffer eucalypt--command-output-buffer
        (should (eq major-mode 'js-mode)))
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (&rest _) "bar")))
        (eucalypt-test--wait (call-interactively #'eucalypt-run-target)))
      (with-current-buffer eucalypt--command-output-buffer
        (should (eq major-mode 'text-mode))))))

(ert-deftest eucalypt-test-list-targets-options-and-errors ()
  "Listing targets ignores run options and reports eu's error output."
  (eucalypt-test--with-stub
    (let ((eucalypt-eu-global-opts "-j"))
      (should (equal (mapcar #'car (eucalypt-list-targets
                                    (expand-file-name "m.eu" dir)))
                     '("bar" "foo"))))
    (let ((err (should-error (eucalypt-list-targets
                              (expand-file-name "broken.eu" dir)))))
      (should (string-search "boom" (error-message-string err))))))

(ert-deftest eucalypt-test-run-replaces-previous-run ()
  "A new run stops one still in progress, which then writes nothing."
  (eucalypt-test--with-stub
    (with-current-buffer (eucalypt-test--visit dir)
      (let ((slow (eucalypt-evaluate "slow")))
        (eucalypt-test--wait (eucalypt-run nil))
        (should-not (process-live-p slow))))
    (sleep-for 1.2)
    (with-current-buffer eucalypt--command-output-buffer
      (should (equal (buffer-string) "a: 1\n")))
    (with-current-buffer eucalypt--command-error-buffer
      (should-not (string-search "late" (buffer-string)))
      (should-not (string-search "Process" (buffer-string))))))

(ert-deftest eucalypt-test-evaluate ()
  "`eucalypt-evaluate' passes `-e EXPR' after the file."
  (eucalypt-test--with-stub
    (with-current-buffer (eucalypt-test--visit dir)
      (eucalypt-test--wait (eucalypt-evaluate "a + 1")))
    (should (equal (eucalypt-test--args (car (eucalypt-test--log log)))
                   '("m.eu" "-e" "a + 1")))))

(ert-deftest eucalypt-test-global-and-file-opts ()
  "Global and file options precede the file; output format follows -j."
  (eucalypt-test--with-stub
    (let ((eucalypt-eu-global-opts "-j")
          (eucalypt-eu-file-opts '("data.yaml")))
      (with-current-buffer (eucalypt-test--visit dir)
        (eucalypt-test--wait (eucalypt-run nil))))
    (should (equal (eucalypt-test--args (car (eucalypt-test--log log)))
                   '("-j" "data.yaml" "m.eu")))
    (with-current-buffer eucalypt--command-output-buffer
      (should-not (eq major-mode 'fundamental-mode)))))

(defun eucalypt-test--compile-command (command name)
  "Run COMMAND in the visited buffer and wait for compilation buffer NAME."
  (let ((buf (funcall command nil)))
    (should (equal (buffer-name buf) name))
    (eucalypt-test--wait (get-buffer-process buf))
    buf))

(ert-deftest eucalypt-test-test-and-check ()
  "`eucalypt-test' and `eucalypt-check' compile from the file's directory."
  (eucalypt-test--with-stub
    (dolist (spec '((eucalypt-test "*eucalypt test*" "test")
                    (eucalypt-check "*eucalypt check*" "check")))
      (let ((buf (with-current-buffer (eucalypt-test--visit dir)
                   (eucalypt-test--compile-command (nth 0 spec) (nth 1 spec)))))
        (with-current-buffer buf
          (should (derived-mode-p 'eucalypt-compilation-mode))
          (should (string-search "m...PASS" (buffer-string)))
          (should-not (string-search "\e[" (buffer-string))))
        (should (equal (eucalypt-test--goto-first-error buf)
                       (cons (expand-file-name "m.eu" dir) 2)))
        ;; `g' runs it again, still without colour.
        (with-current-buffer buf
          (recompile)
          (eucalypt-test--wait (get-buffer-process buf))
          (should-not (string-search "\e[" (buffer-string))))))
    (let ((calls (eucalypt-test--log log)))
      (should (equal (mapcar #'eucalypt-test--args calls)
                     '(("test" "m.eu") ("test" "m.eu")
                       ("check" "m.eu") ("check" "m.eu"))))
      (dolist (call calls)
        (should (equal (cdr (assoc "cwd" call)) (directory-file-name dir)))
        (should (equal (cdr (assoc "no_color" call)) "1"))))))

(ert-deftest eucalypt-test-render-buffer-from-file-directory ()
  "`eucalypt-render-buffer' pipes the buffer from the file's directory."
  (eucalypt-test--with-stub
    (with-current-buffer (eucalypt-test--visit dir)
      (eucalypt-render-buffer nil))
    (let ((call (car (eucalypt-test--log log))))
      (should (equal (cdr (assoc "cwd" call)) (directory-file-name dir)))
      (should (equal (cdr (assoc "no_color" call)) "1"))
      (should (equal (eucalypt-test--args call) '("eu@-")))
      (should (equal (cdr (assoc "stdin" call)) "a: 1|b: nosuch|")))
    (with-current-buffer eucalypt--command-error-buffer
      (should (derived-mode-p 'eucalypt-compilation-mode))
      (should-not (string-search "\e[" (buffer-string))))))

(ert-deftest eucalypt-test-commands-repeat ()
  "Commands run again after a run has left the errors buffer read-only."
  (eucalypt-test--with-stub
    (with-current-buffer (eucalypt-test--visit dir)
      (eucalypt-test--wait (eucalypt-run nil))
      (with-current-buffer eucalypt--command-error-buffer
        (should buffer-read-only))
      (eucalypt-render-buffer nil)
      (eucalypt-test--wait (eucalypt-run nil)))
    (should (= (length (eucalypt-test--log log)) 3))
    (with-current-buffer eucalypt--command-error-buffer
      (should (= (how-many "unresolved variable" (point-min) (point-max)) 1)))))

(defun eucalypt-test--add-mock-method ()
  "Add a TRAMP `mock' method that runs a local shell, as TRAMP's tests do."
  (unless (assoc "mock" tramp-methods)
    (add-to-list
     'tramp-methods
     `("mock"
       (tramp-login-program ,tramp-default-remote-shell)
       (tramp-login-args (("-i")))
       (tramp-remote-shell ,tramp-default-remote-shell)
       (tramp-remote-shell-args ("-c"))
       (tramp-connection-timeout 10)))))

(ert-deftest eucalypt-test-remote-file ()
  "Over TRAMP, the commands run `eu' on the remote host in the file's directory."
  :tags '(tramp)
  (eucalypt-test--with-stub
    (eucalypt-test--add-mock-method)
    ;; Name `eu' bare and put it only on the remote path, so it must be
    ;; looked for on the remote host.
    (let* ((tramp-verbose 0)
           (remote (concat "/mock::" dir))
           (eucalypt-eu-command (file-name-nondirectory stub))
           (tramp-remote-path (cons (file-name-directory stub)
                                    tramp-remote-path)))
      (unwind-protect
          (with-current-buffer (eucalypt-test--visit remote)
            (should (file-remote-p default-directory))
            (eucalypt-test--wait (eucalypt-run nil))
            (should (equal (mapcar #'car (eucalypt-list-targets buffer-file-name))
                           '("bar" "foo")))
            (let ((buf (eucalypt-test--compile-command #'eucalypt-check
                                                       "*eucalypt check*")))
              (with-current-buffer buf
                (should (string-search "m...PASS" (buffer-string))))))
        (tramp-cleanup-all-connections)))
    (let ((calls (eucalypt-test--log log)))
      (should (equal (mapcar #'eucalypt-test--args calls)
                     '(("m.eu") ("list-targets" "m.eu") ("check" "m.eu"))))
      (dolist (call calls)
        (should (equal (cdr (assoc "cwd" call)) (directory-file-name dir)))
        (should (equal (cdr (assoc "no_color" call)) "1"))))
    (with-current-buffer eucalypt--command-output-buffer
      (should (equal (buffer-string) "a: 1\n")))))

(ert-deftest eucalypt-test-requires-file ()
  "The file commands refuse a buffer that visits no file."
  (with-temp-buffer
    (should-error (eucalypt-run nil) :type 'user-error)
    (should-error (eucalypt-test nil) :type 'user-error)))

;;; Commands against a real eu (optional)

(ert-deftest eucalypt-test-real-eu ()
  "Against a real `eu': imports resolve from the file and errors are clickable."
  :tags '(real-eu)
  (let ((eu (getenv "EUCALYPT_TEST_EU")))
    (when (or (null eu) (string-empty-p eu))
      (ert-skip "EUCALYPT_TEST_EU is not set"))
    (should (and (file-regular-p eu) (file-executable-p eu)))
    (let* ((root (file-name-as-directory (make-temp-file "eucalypt-real" t)))
           (dir (file-name-as-directory (expand-file-name "proj" root)))
           (eucalypt-eu-command eu)
           (eucalypt-eu-global-opts "")
           (eucalypt-eu-file-opts nil)
           (default-directory root))
      (unwind-protect
          (progn
            (make-directory (expand-file-name "lib" dir) t)
            (with-temp-file (expand-file-name "lib/l.eu" dir)
              (insert "y: { z: 3 }\n"))
            (with-temp-file (expand-file-name "m.eu" dir)
              (insert "{ import: \"lib/l.eu\" }\n"
                      "` { target: :zed doc: \"just z\" }\n"
                      "a: y.z\n"))
            (with-temp-file (expand-file-name "bad.eu" dir)
              (insert "a: 1\nb: nosuch\n"))
            (with-current-buffer (eucalypt-test--visit dir)
              (should (equal (mapcar #'car (eucalypt-list-targets buffer-file-name))
                             '("zed")))
              (eucalypt-test--wait (eucalypt-run-target "zed")))
            (with-current-buffer eucalypt--command-output-buffer
              (should (string-match-p "^3$" (buffer-string))))
            (with-current-buffer
                (let ((auto-mode-alist nil))
                  (find-file-noselect (expand-file-name "bad.eu" dir)))
              (eucalypt-test--wait (eucalypt-run nil)))
            (let ((errors (get-buffer eucalypt--command-error-buffer)))
              (with-current-buffer errors
                (should-not (string-search "\e[" (buffer-string))))
              (should (equal (eucalypt-test--goto-first-error errors)
                             (cons (expand-file-name "bad.eu" dir) 2)))))
        (dolist (b (buffer-list))
          (when (and (buffer-file-name b)
                     (string-prefix-p root (buffer-file-name b)))
            (kill-buffer b)))
        (delete-directory root t)))))

(provide 'eucalypt-mode-commands-test)

;;; eucalypt-mode-commands-test.el ends here
