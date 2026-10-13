;;; lf-assert-tests.el --- Tests for lf-assert: multi-expression assertions with full-context errors  -*- lexical-binding: t; -*-

(unless (fboundp 'define-lf-call-test)
  (load (expand-file-name "lf-test-utils.el" (file-name-directory (or load-file-name buffer-file-name))) nil t))

;; The ad-hoc, no-function-definition-required cousin of `lf-specify': assert a property of each of several
;; expressions in one call, with `it' bound to each expression's own value for its paired condition.

(define-lf-call-test "lf-assert returns t when every pair's condition holds"
  :call (lf-assert 4 (cl-evenp it)
                   "Hello" (and (stringp it) (> (length it) 3)))
  :returns t)

(define-lf-error-test "lf-assert's error names the failing expression, its value, and its type"
  :call (lf-assert (+ 1 2) (cl-evenp it))
  :signals "Error: lf-assert failed.

((+ 1 2) satisfies (cl-evenp it) = 3 : integer)")

(define-lf-error-test "lf-assert's error shows every pair, not just the failing one, for context"
  :call (lf-assert (+ 1 2) (cl-evenp it)
                   "Hello" (stringp it))
  :signals "Error: lf-assert failed.

((+ 1 2) satisfies (cl-evenp it) = 3 : integer)

(\"Hello\" satisfies (stringp it) = \"Hello\" : string)")

(deftest "lf-assert evaluates each expression exactly once"
  (let ((counter 0))
    (should (lf-assert (cl-incf counter) (integerp it)))
    (should (equal counter 1))))

(provide 'lf-assert-tests)
;;; lf-assert-tests.el ends here
