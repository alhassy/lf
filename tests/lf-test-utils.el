;;; lf-test-utils.el --- Shared machinery for tests/*-tests.el: fixtures and relations, via snap  -*- lexical-binding: t; -*-

;; Each tests/lf-*-tests.el loads this file first, guarded by (fboundp 'define-lf-call-test).
;; Tests are written with snap: https://github.com/alhassy/snap
;;   - `deftest' ---a plain test, named by its sentence; it undefines our scratch names around it.
;;   - `define-lf-call-test'  ---FORM ↦ its value.
;;   - `define-lf-error-test' ---FORM ↦ the error it signals, sans its context-dependent CALLED FROM trailer.
;;   - `define-lf-dump-test'  ---FORM ↦ what it prints, sans its context-dependent CALLED FROM trailer.
;; C-x C-e after a test defines and runs it; C-u C-x C-e refreshes a relation's snapshot.

(require 'ert)
(require 'cl-lib)
(require 's)
(require 'dash)

(load (expand-file-name "../lf.el" (file-name-directory (or load-file-name buffer-file-name))) nil t)
(unless (require 'snap nil t)
  (load-file (expand-file-name "~/snap/snap.el")))

;;; Scratch names

(defconst lf-test--scratch-names
  '(age name foods x f id go speak next between greet search-url sum-all change-subject gerrit-change-p
    safe-nth bt-inner bt-middle bt-outer bt-inner2 bt-outer2 dump-bt-inner dump-bt-outer)
  "Names that tests define with `lf-define' or `defun'; every `deftest' undefines them before and after.")

(defun lf-test--undefine-scratch ()
  "Undefine every one of `lf-test--scratch-names', as variables and functions."
  (eval `(lf-undefine ,@lf-test--scratch-names)))

;;; Fixtures

(snap-define-fixture deftest
  "A test that starts and ends with `lf-test--scratch-names' undefined, so no test sees another's definitions."
  (unwind-protect (progn (lf-test--undefine-scratch) &body)
    (lf-test--undefine-scratch)))

(snap-define-fixture deftest-declaring-types
  "A test that may `lf-declare-type'; afterwards, the declarations and the advice on `eval' are removed."
  (unwind-protect (progn &body)
    (setq lf--declared-type-env nil)
    (advice-remove 'eval #'lf--type-check-eval-advice)))

;;; Errors and printed messages, sans the CALLED FROM trailer

(defun lf-test--sans-trailer (message)
  "MESSAGE without its trailing \"CALLED FROM: …\" call chain, which differs between batch, `M-x ert', etc."
  (s-trim-right (replace-regexp-in-string "\n*CALLED FROM:.*\\'" "" message)))

(defmacro lf-test--signalled (form)
  "The message of the error that FORM signals, sans trailer; or the symbol `no-error'.

FORM is expanded only when run, via `eval': `lf-define' checks a variable's constraints while it
expands, so a violation expanded at load time would abort loading the whole test file."
  `(condition-case err (progn (eval ',form) 'no-error)
     (error (lf-test--sans-trailer (error-message-string err)))))

(defmacro lf-test--printed (form)
  "The messages that FORM prints, joined by newlines and sans trailers; FORM's own value is ignored."
  `(let ((printed nil))
     (cl-letf (((symbol-function 'message)
                (lambda (fmt &rest args) (push (apply #'format fmt args) printed))))
       ,form)
     (mapconcat #'lf-test--sans-trailer (nreverse printed) "\n")))

;;; Relations
;;
;; Forms are evaluated with dynamic binding, as the original tests.el was: `lf-string' interpolates
;; ${…} by `eval' at runtime, so it sees only dynamically bound variables.

(defun lf-test--isolated-eval (form)
  "Evaluate FORM dynamically, with `lf-test--scratch-names' undefined before and after."
  (unwind-protect (progn (lf-test--undefine-scratch) (eval form))
    (lf-test--undefine-scratch)))

(snap-define-relation lf-call (call returns)
  "Evaluating CALL yields RETURNS."
  (let ((actual (lf-test--isolated-eval call)))
    (should (equal returns actual))
    (list :returns actual)))

(snap-define-relation lf-error (call signals)
  "Evaluating CALL signals an error whose message, sans CALLED FROM trailer, is SIGNALS ---modulo runs of
whitespace, since `pp-to-string' breaks the forms it shows according to the environment's width."
  (let ((actual (lf-test--isolated-eval `(lf-test--signalled ,call))))
    (should (equal (s-collapse-whitespace (format "%s" signals)) (s-collapse-whitespace (format "%s" actual))))
    (list :signals actual)))

(snap-define-relation lf-dump (call prints)
  "Evaluating CALL prints PRINTS, sans CALLED FROM trailers."
  (let ((actual (lf-test--isolated-eval `(lf-test--printed ,call))))
    (should (equal prints actual))
    (list :prints actual)))

(provide 'lf-test-utils)
;;; lf-test-utils.el ends here
