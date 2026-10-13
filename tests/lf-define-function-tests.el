;;; lf-define-function-tests.el --- Tests for lf-define on functions: docs, contracts, expansions  -*- lexical-binding: t; -*-

(unless (fboundp 'define-lf-call-test)
  (load (expand-file-name "lf-test-utils.el" (file-name-directory (or load-file-name buffer-file-name))) nil t))

;;; Documented and typed functions

(deftest "lf-define defines documented functions"
  (lf-define speak (name) "An English greeting." (format "Hello, %s!" name))
  (should (string-match-p "An English greeting\\." (documentation #'speak))))

(deftest "lf-define's typed functions keep the user's docstring and arglist, and name their contract advice"
  (lf-define speak (name age)
             [:requires (and (stringp name) (integerp age))
              :ensures (stringp result)]
             "Greet person NAME with their AGE."
             (format "Hello %s year-old %s!" age name))
  (let ((doc (documentation #'speak)))
    (should (string-match-p "Greet person NAME with their AGE\\." doc))
    (should (string-match-p "lf--specify/speak" doc))
    (should (string-match-p "(fn NAME AGE)" doc))))

(define-lf-call-test "lf-define's typed functions return as usual for well-typed inputs"
  :call (progn (lf-define speak (name age)
                          [:requires (and (stringp name) (integerp age))
                           :ensures (stringp result)]
                          "Greet person NAME with their AGE."
                          (format "Hello %s year-old %s!" age name))
               (speak "musa" 29))
  :returns "Hello 29 year-old musa!")

(define-lf-error-test "lf-define's typed functions reject ill-typed inputs, showing what was required and given"
  :call (progn (lf-define speak (name age)
                          [:requires (and (stringp name) (integerp age))
                           :ensures (stringp result)]
                          "Greet person NAME with their AGE."
                          (format "Hello %s year-old %s!" age name))
               (speak 'musa 29))
  :signals "Error: Requirements for “speak” have been violated.

REQUIRED:
(and (stringp name) (integerp age))

GIVEN:
((name = musa : symbol) (age = 29 : integer))")

(define-lf-error-test "lf-define's typed functions panic when their result breaks the claimed guarantee"
  :call (progn (lf-define speak (name age)
                          [:requires (and (stringp name) (integerp age))
                           :ensures (stringp result)]
                          12)
               (speak "musa" 29))
  :signals "Panic! There is an error in the implementation of “speak”.

Claimed guarantee: (stringp result)

Actual result value: 12
 ---typed: integer")

(define-lf-error-test "lf-define's functions panic when poorly implemented, showing the guarantee and actual result"
  :call (progn (lf-define speak (name age)
                          [:ensures (vectorp result)]
                          "Given a NAME and AGE, present a nice greeting."
                          (format "Hello, %s!" name))
               (speak "mesa" 12))
  :signals "Panic! There is an error in the implementation of “speak”.

Claimed guarantee: (vectorp result)

Actual result value: \"Hello, mesa!\"
 ---typed: string")

(define-lf-call-test "lf-define's post-conditions may relate the result to the inputs"
  :call (progn (lf-define next (x)
                          "This function returns a value strictly greater than input X."
                          [:requires (integerp x)
                           :ensures  (< x result)]
                          (1+ x))
               (next 2))
  :returns 3)

(define-lf-error-test "lf-define's contracts may signal custom error messages, via or-error"
  :call (progn (lf-define go (x) [:requires (or (integerp x) (error "I asked you for a bloody integer!"))] x)
               (go "3"))
  :signals "I asked you for a bloody integer!")

;;; Expansions

(define-lf-call-test "lf-define expands a function definition into a cl-defun with contract advice"
  :call (macroexpand '(lf-define add (x) (+ 1 x)))
  :returns (progn (cl-defun add (x) nil (+ 1 x)) (lf-specify add (x) :requires t :ensures t) 'add))

(define-lf-call-test "lf-define expands a documented function definition into a documented cl-defun"
  :call (macroexpand '(lf-define add (x) "Hola" (+ 1 x)))
  :returns (progn (cl-defun add (x) "Hola" (+ 1 x)) (lf-specify add (x) :requires t :ensures t) 'add))

(define-lf-call-test "lf-define expands a typed, documented function definition, passing its contract to lf-specify"
  :call (macroexpand '(lf-define add (x) [:requires (integerp x)] "My docs" (+ 1 x)))
  :returns (progn (cl-defun add (x) "My docs" (+ 1 x)) (lf-specify add (x) :requires (integerp x) :ensures t) 'add))

;;; Regressions: contracts see every argument bound, and arity is preserved
;;
;; A prior `lf-specify' advised with a bare (&rest ,args) parameter list ---e.g., `&rest (name age)'---
;; which `cl-defun' silently accepts as a *destructuring* pattern.  So: calls with exactly (length ARGS)
;; arguments seemed fine; any other count signalled `wrong-number-of-arguments'; and ARGS containing
;; &optional/&rest were not valid destructuring at all, so every call failed with void-variable.
;; `cl-destructuring-bind' over a dedicated &rest capture fixes all three.

(deftest "lf-define's two-argument :requires sees both arguments bound, not void-variable"
  (lf-define speak (name age)
             [:requires (and (stringp name) (integerp age))
              :ensures  (stringp result)]
             "Greet person NAME with their AGE."
             (format "Hello %s year-old %s!" age name))
  (should (equal (speak "musa" 29) "Hello 29 year-old musa!"))
  (should (string-match-p "GIVEN:" (lf-test--signalled (speak 'musa 29)))))

(deftest "lf-define's :requires sees every one of three or more arguments bound"
  (lf-define between (lo x hi)
             [:requires (and (integerp lo) (integerp x) (integerp hi))
              :ensures  (booleanp result)]
             "Non-nil iff X lies within [LO, HI]."
             (and (<= lo x) (<= x hi)))
  (should (between 0 5 10))
  (should-error (between 0 "five" 10)))

(deftest "lf-define preserves the wrapped function's real arity"
  (lf-define greet (name &optional greeting)
             [:requires (stringp name)
              :ensures  (stringp result)]
             "Greet NAME, defaulting GREETING to \"Hello\"."
             (format "%s, %s!" (or greeting "Hello") name))
  (should (equal (greet "musa") "Hello, musa!"))
  (should (equal (greet "musa" "Marhaba") "Marhaba, musa!"))
  (should-error (greet "musa" "Marhaba" "extra-arg-should-not-be-accepted")))

(deftest "lf-define binds &optional arguments, present or defaulted, inside :requires"
  (lf-define search-url (jql &optional fields max)
             [:requires (and (stringp jql)
                             (or (null fields) (stringp fields))
                             (or (null max) (integerp max)))
              :ensures  (stringp result)]
             "Build a search URL for JQL, FIELDS (default summary), MAX (default 50)."
             (format "jql=%s&fields=%s&max=%d" jql (or fields "summary") (or max 50)))
  (should (equal (search-url "status:open") "jql=status:open&fields=summary&max=50"))
  (should (equal (search-url "status:open" "title" 10) "jql=status:open&fields=title&max=10"))
  (should-error (search-url "status:open" "title" "ten")))

(deftest "lf-define binds &rest arguments to the trailing list inside :requires"
  (lf-define sum-all (label &rest nums)
             [:requires (and (stringp label) (-all-p #'integerp nums))
              :ensures  (integerp result)]
             "Sum NUMS; LABEL documents the call site only."
             (ignore label)
             (apply #'+ nums))
  (should (equal (sum-all "total" 1 2 3) 6))
  (should (equal (sum-all "empty") 0))
  (should-error (sum-all "total" 1 "two" 3)))

(provide 'lf-define-function-tests)
;;; lf-define-function-tests.el ends here
