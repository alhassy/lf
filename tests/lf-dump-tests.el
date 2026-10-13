;;; lf-dump-tests.el --- Tests for lf-dump, lf-dump-with-backtrace, lf-dump*, lf-dump-if, lf-dump*if  -*- lexical-binding: t; -*-

(unless (fboundp 'define-lf-call-test)
  (load (expand-file-name "lf-test-utils.el" (file-name-directory (or load-file-name buffer-file-name))) nil t))

;; `lf-specify''s :requires failure message has always listed its arguments as "NAME = VALUE"; these
;; macros pull that idea out into a general-purpose family for ad-hoc debugging prints.  `message' is
;; captured, not printed, by `lf-test--printed'; the context-dependent CALLED FROM trailer is dropped.

(define-lf-dump-test "lf-dump prints literal forms bare, without a value, all in one message"
  :call (lf-dump 42 "str" 'foo)
  :prints "42
\"str\"
'foo")

(define-lf-dump-test "lf-dump prints a non-literal form's value alongside the form"
  :call (lf-dump (+ 1 2))
  :prints "(+ 1 2) = 3")

(define-lf-dump-test "lf-dump mixes literal and non-literal forms in one dump"
  :call (lf-dump 42 (+ 1 2) "str")
  :prints "42
(+ 1 2) = 3
\"str\"")

(define-lf-dump-test "lf-dump with no forms prints nothing"
  :call (lf-dump)
  :prints "")

(deftest "lf-dump evaluates each form exactly once"
  (let ((counter 0))
    (should (equal (lf-test--printed (lf-dump (cl-incf counter))) "(cl-incf counter) = 1"))
    (should (equal counter 1))))

(deftest "lf-dump-with-backtrace appends a CALLED FROM trailer naming the real caller chain"
  (defun dump-bt-inner () (lf-dump-with-backtrace (+ 1 2)))
  (defun dump-bt-outer () (dump-bt-inner))
  (let ((printed nil))
    (cl-letf (((symbol-function 'message) (lambda (fmt &rest args) (push (apply #'format fmt args) printed))))
      (dump-bt-outer))
    (should (string-match-p "CALLED FROM:.*dump-bt-inner.*dump-bt-outer" (car printed)))))

(deftest "lf-dump-if dumps, and evaluates its forms, only when its condition holds"
  (let ((counter 0))
    (should (equal (lf-test--printed (lf-dump-if nil (cl-incf counter))) ""))
    (should (equal counter 0)))
  (should (equal (lf-test--printed (lf-dump-if t (+ 1 2))) "(+ 1 2) = 3")))

(deftest "lf-dump* and lf-dump*if enter the debugger after dumping, the latter only when its condition holds"
  ;; `debug' is stubbed: batch ERT cannot drive a recursive edit.
  (let ((debugged 0))
    (cl-letf (((symbol-function 'debug) (lambda (&rest _) (cl-incf debugged))))
      (should (equal (lf-test--printed (lf-dump* (+ 1 2))) "(+ 1 2) = 3"))
      (should (equal debugged 1))
      (lf-dump*if nil (+ 1 2))
      (should (equal debugged 1))
      (lf-test--printed (lf-dump*if t (+ 1 2)))
      (should (equal debugged 2)))))

(provide 'lf-dump-tests)
;;; lf-dump-tests.el ends here
