;;; lf-declare-type-tests.el --- Tests for lf-declare-type: call-site type checks from signatures  -*- lexical-binding: t; -*-

(unless (fboundp 'define-lf-call-test)
  (load (expand-file-name "lf-test-utils.el" (file-name-directory (or load-file-name buffer-file-name))) nil t))

;; `lf-declare-type' works via `macroexpand-all' on `eval'; it applies to user-defined functions and macros,
;; not to built-in C functions like `nth', whose calls never pass through `macroexpand-all'.  So we use thin
;; user-defined wrappers, keeping the tests deterministic.  Each `deftest-declaring-types' removes the
;; declarations, and the advice on `eval', afterwards.

(defun nth-wrapper (n list) (nth n list))

;; `nth' itself demands an integer first argument, so a first parameter left unconstrained by `lf' would still
;; trip over it; `tag-wrapper' has no such builtin expectation.
(defun tag-wrapper (tag list) (cons tag list))

(deftest-declaring-types "lf-declare-type installs advice on eval"
  (lf-declare-type nth-wrapper :: integer list -> alpha)
  (should (advice-member-p #'lf--type-check-eval-advice 'eval)))

(deftest-declaring-types "lf-declare-type lets well-typed calls through unchanged"
  (lf-declare-type nth-wrapper :: integer list -> alpha)
  (should (equal (eval '(nth-wrapper 0 '(a b c))) 'a))
  (should (equal (eval '(nth-wrapper 2 '(a b c))) 'c)))

(deftest-declaring-types "lf-declare-type reports a non-integer index in a friendly error"
  (lf-declare-type nth-wrapper :: integer list -> alpha)
  (should (equal (lf-test--signalled (eval '(nth-wrapper "zero" '(a b c))))
                 "(nth-wrapper ... \"zero\" ...) ---expected ‘integerp’, got \"zero\" (string).")))

(deftest-declaring-types "lf-declare-type reports a non-list argument in a friendly error"
  (lf-declare-type nth-wrapper :: integer list -> alpha)
  (should (equal (lf-test--signalled (eval '(nth-wrapper 0 "not-a-list")))
                 "(nth-wrapper ... \"not-a-list\" ...) ---expected ‘listp’, got \"not-a-list\" (string).")))

(deftest-declaring-types "lf-declare-type reports the wrong number of arguments in a friendly error"
  (lf-declare-type nth-wrapper :: integer list -> alpha)
  (should (equal (lf-test--signalled (eval '(nth-wrapper 0 '(a b c) 99)))
                 "(nth-wrapper 0 '(a b c) 99) ---expected arg count divisible by 2; got 3.")))

(deftest-declaring-types "lf-declare-type works identically with the leading :: omitted"
  (lf-declare-type nth-wrapper integer list -> alpha)
  (should (equal (eval '(nth-wrapper 0 '(a b c))) 'a))
  (should (equal (lf-test--signalled (eval '(nth-wrapper "zero" '(a b c))))
                 "(nth-wrapper ... \"zero\" ...) ---expected ‘integerp’, got \"zero\" (string).")))

(deftest-declaring-types "lf-declare-type signals a clear error at declaration time when -> is missing"
  (should (equal (lf-test--signalled (lf-declare-type nth-wrapper :: integer list))
                 "lf-declare-type: missing ‘->‘ in type spec (integer list)")))

(deftest-declaring-types "lf-declare-type signals a clear error at declaration time for an unknown type name"
  (should (equal (lf-test--signalled (lf-declare-type nth-wrapper :: frobnicatable list -> alpha))
                 "lf-declare-type: no predicate for type ‘frobnicatable’ (tried ‘frobnicatablep’)")))

(deftest-declaring-types "lf-declare-type leaves a Greek-letter input type truly unconstrained"
  (lf-declare-type tag-wrapper :: alpha list -> beta)
  (should (equal (eval '(tag-wrapper "any string at all" '(a b c))) '("any string at all" a b c)))
  (should (equal (eval '(tag-wrapper 'any-symbol-at-all '(a b c))) '(any-symbol-at-all a b c)))
  ;; The second parameter is still enforced, unaffected by the first's being a type variable.
  (should (equal (lf-test--signalled (eval '(tag-wrapper "tag" "not-a-list")))
                 "(tag-wrapper ... \"not-a-list\" ...) ---expected ‘listp’, got \"not-a-list\" (string).")))

(deftest-declaring-types "lf-declare-type also checks calls in forms evaluated within an evaluated form"
  ;; Regression: the reentrancy guard once covered the whole evaluation, not just the expansion, so any
  ;; `eval' nested within an evaluated form went unchecked.
  (lf-declare-type nth-wrapper :: integer list -> alpha)
  (should (equal (lf-test--signalled (eval '(progn (eval '(nth-wrapper "zero" '(a b c))))))
                 "(nth-wrapper ... \"zero\" ...) ---expected ‘integerp’, got \"zero\" (string).")))

(provide 'lf-declare-type-tests)
;;; lf-declare-type-tests.el ends here
