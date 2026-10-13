;;; lf-define-variable-tests.el --- Tests for lf-define on variables and places  -*- lexical-binding: t; -*-

(unless (fboundp 'define-lf-call-test)
  (load (expand-file-name "lf-test-utils.el" (file-name-directory (or load-file-name buffer-file-name))) nil t))

;; Every `deftest' starts and ends with our scratch names ---age, name, foods, …--- undefined;
;; see `lf-test--scratch-names'.  `lf-define' checks a variable's constraints while it expands,
;; so a definition expected to violate them is deferred via `lf-test--signalled'.

;;; Defining, and documenting

(deftest "lf-define defines both variables and functions"
  (lf-define age 29)
  (should (equal age 29))
  (lf-define speak (name) (format "Hello, %s!" name))
  (should (equal (speak 'bob) "Hello, bob!")))

(deftest "lf-define documents variables"
  (lf-define age 29 "How old am I?")
  (should (equal (lf-documentation 'age) "How old am I?")))

(deftest "lf-define defines strings, with or without documentation"
  (lf-define name "Bobert")
  (should (equal name "Bobert"))
  (should (equal (lf-documentation 'name) nil))
  (lf-undefine name)
  (lf-define name "Bobert" "What is my name?")
  (should (equal (lf-documentation 'name) "What is my name?")))

(deftest "lf-define defines typed strings, with or without documentation"
  (lf-define name "Bobert" [:type string])
  (should (equal name "Bobert"))
  (should (equal (lf-documentation 'name) nil))
  (lf-undefine name)
  (lf-define name "Bobert" [:type string] "What is my name?")
  (should (equal (lf-documentation 'name) "What is my name?")))

(deftest "lf-define defines vectors, with or without documentation or types"
  (lf-define foods [shorba dolma] "What Iraqi foods do I like?")
  (should (equal foods [shorba dolma]))
  (should (equal (documentation-property 'foods 'variable-documentation) "What Iraqi foods do I like?"))
  (lf-undefine foods)
  (lf-define foods [shorba dolma])
  (should (equal (documentation-property 'foods 'variable-documentation) nil))
  (lf-undefine foods)
  (lf-define foods [shorba dolma] [:type vector])
  (should (equal foods [shorba dolma]))
  (should (equal (documentation-property 'foods 'variable-documentation) nil)))

;;; Places, and “zapping” with it

(deftest "lf-define uses setf when PLACE is a non-atomic form"
  (lf-define foods '(apple banana))
  (lf-define (car foods) 'pineapple)
  (should (equal foods '(pineapple banana))))

(deftest "lf-define zaps: (lf-define x (f it)) ≈ (setq x (f x))"
  (lf-define x 12)
  (lf-define x (+ it 1))
  (should (equal x 13)))

(deftest "lf-define zaps arbitrary places, such as a variable's documentation"
  (lf-define age 12)
  (lf-define (documentation-property 'age 'variable-documentation) (or "New way to update docs" it))
  (should (equal (documentation-property 'age 'variable-documentation) "New way to update docs"))
  (lf-define (documentation-property 'age 'variable-documentation) (format "woah, %s" it))
  (should (equal (documentation-property 'age 'variable-documentation) "woah, New way to update docs")))

;;; Constraints

(deftest "lf-define leaves a variable unbound when its initial value violates its new constraints"
  (lf-define age "12" [:type string])
  ;; The new value satisfies the old constraints, but those are dismissed in favour of the new ones.
  (should (equal (lf-test--signalled (lf-define age "12" [(and (integerp it) (stringp it))]))
                 "Error: Initial value “12” violates declared constraint:
	[(and (integerp it) (stringp it))]

As such, symbol “age” is now unbound and unconstrained."))
  (should-not (boundp 'age)))

(deftest "lf-define replaces a variable's constraints with any newly declared ones"
  (lf-define age 9 [(and (integerp it) (<= 0 it 10))])
  (should (equal age 9))
  (lf-undefine age)
  (lf-define age 29 [:type integer] "How old am I?")
  (lf-define age "twenty-nine" [:type string] "How old am I?")
  (lf-define age 'twenty-nine [:type symbol])
  (should (equal age 'twenty-nine)))

(deftest "lf-define's constraints are checked on every later setq"
  (lf-define age 9 [(integerp it)])
  (should (equal (lf-test--signalled (setq age "nine"))
                 "Error: Constraints for “age” have been violated.
Newvalue “nine” (string) does not satisfy: [(integerp it)]

As such, “age” retains its old value “9”."))
  (should (equal age 9)))

(deftest "lf-define's constraints survive byte-compilation, being registered when the definition runs"
  ;; Regression: constraints were once registered while `lf-define' expanded ---i.e., in the compiling
  ;; Emacs--- so a loaded .elc had none, and any value could then be setq'd.
  (let* ((source (make-temp-file "lf-constrained-" nil ".el"
                                 ";;; -*- lexical-binding: t; -*-\n(require 'lf)\n(lf-define age 9 [(integerp it)])\n"))
         (compiled (byte-compile-dest-file source)))
    (unwind-protect
        (progn
          (let ((byte-compile-log-warning-function #'ignore)) (byte-compile-file source))
          (lf-test--undefine-scratch) ;; Forget anything the compiling session did.
          (load compiled nil t)
          (should (equal age 9))
          (should (stringp (lf-test--signalled (setq age "nine"))))
          (should (equal age 9)))
      (delete-file source)
      (when (file-exists-p compiled) (delete-file compiled)))))

;; Three forms claimed equivalent in `lf-define''s docstring: a compound CL type-specifier alone, a bare type
;; plus a separate (satisfies …), and the two joined with `and'.  All ride on `cl-typep'.

(deftest "lf-define with [:type (integer 0 100)] accepts in-range and rejects out-of-range values"
  (lf-define age 50 [:type (integer 0 100)])
  (should (equal age 50))
  (should (stringp (lf-test--signalled (lf-define age 150 [:type (integer 0 100)])))))

(deftest "lf-define with [:type integer (satisfies …)] accepts in-range and rejects out-of-range values"
  (lf-define age 50 [:type integer (satisfies (lambda (value) (<= 0 value 100)))])
  (should (equal age 50))
  (should (stringp (lf-test--signalled (lf-define age 150 [:type integer (satisfies (lambda (value) (<= 0 value 100)))])))))

(deftest "lf-define with [:type (and integer (satisfies …))] accepts in-range and rejects out-of-range values"
  (lf-define age 50 [:type (and integer (satisfies (lambda (value) (<= 0 value 100))))])
  (should (equal age 50))
  (should (stringp (lf-test--signalled (lf-define age 150 [:type (and integer (satisfies (lambda (value) (<= 0 value 100))))])))))

;;; An empty arglist defines a variable, not an empty function

(deftest "lf-define with an empty arglist and no body defines the variable as nil"
  (lf-define f ())
  (should (equal f nil))
  (should (equal (documentation-property 'f 'variable-documentation) nil)))

(deftest "lf-define with an empty arglist and a docstring defines a documented variable as nil"
  (lf-define f () "Docs")
  (should (equal f nil))
  (should (equal (documentation-property 'f 'variable-documentation) "Docs"))
  (lf-undefine f)
  (lf-define f () "Docs" nil)
  (should (equal f nil))
  (should (equal (documentation-property 'f 'variable-documentation) "Docs")))

(deftest "lf-define defines an empty function indirectly, via a body that evaluates to nil"
  (lf-define f () "Woah!" (and nil))
  (should (equal (f) nil))
  (should (string-match-p "Woah!" (documentation #'f)))
  (lf-undefine f)
  (lf-define f (x y) "Woah!" (and nil))
  (should (equal (f 1 2) nil))
  (should (string-match-p "Woah!" (documentation #'f))))

(provide 'lf-define-variable-tests)
;;; lf-define-variable-tests.el ends here
