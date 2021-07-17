;; WARNING: This file is intended for use with the repo's Github Actions.
;; We install some packages, possibly unneeded ones, then run the tests.

(setq needed-libraries
      '(s cl-lib dash seq))

(require 'package)
(push '("melpa" . "https://melpa.org/packages/") package-archives)
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))
(dolist (pkg needed-libraries)
  (unless (package-installed-p pkg)
    (package-install pkg)))

(load-file "lf.el")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Personal Testing Utils ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defmacro deftest (desc tags &rest body)
  "Declare tests with meaningful string names, that reflect the test's main goal.

DESC is a string, TAGS is a vector.

The first tag should be the name of the main function being tested;
this name is prepended to the name of underlying ert-deftest.
This way, tests are grouped/namespaced when running ert from the command line.

Example ERT call: (ert '(tag my-cool-tag))"
  `(ert-deftest ,(intern
                  (concat
                   (format "%s::" (seq-elt tags 0))
                   (seq-map (lambda (c) (if (<= 65 c 122) c ?_))
                                   desc))) ()
     :tags (quote ,(seq--into-list tags))
     ,@body))
  ;; Convert all non-letters to ‘_’; A = 65, z = 122.
  ;; Without the replace, “M-x ert” crashes when it comes to selecting the test
  ;; to run.

(defmacro ≋ (lhs rhs)
    "A shorthand for (should (equal LHS RHS))."
    `(should (equal ,lhs ,rhs)))

(defmacro ↯ (form &optional message)
    "The given FORM should error (with MESSAGE, ignoring whitespace).

Example: (↯ (error \"hola\") \"hola\")

Pictogram explanation: You've made such a big mistake, that the
heavens strike you down with a lightening bolt ↯↯"
  (if message
      `(should (equal (s-collapse-whitespace (cl-second (should-error ,form)))
                      (s-collapse-whitespace ,message)))
    `(should-error ,form)))


;; I like the shapes: (× some-crashing-form) and (↯ crashes with-this-message)
(defalias '✓ 'should)
(defalias '× '↯)


(defmacro ⇝ (expr &rest regexp)
    "The given EXPR should match the given REGEXP, which is wrapped by ‘rx’.

REGEXP could also be a string, in which case we are doing string equality.
Either way, whitespace is ignored in both arguments.

The symbol “⇝” should be read “rewrites to” or “elaborates to”.

I prefer this form since it has the main form we're asserting
against-at the forefront and makes it clear we're matching
against strings.

For example,

  (should (s-matches? \"An English greeting.\" (documentation 'speak)))

Becomes:

  (⇝ (documentation 'speak) \"An English greeting.\")

Pictogram explanation: A given expression “rewrites, reduces,” to
a given matching pattern. Such arrows are popular in Term Rewriting Systems."
  `(should (s-matches? (s-collapse-whitespace (rx ,(cons 'seq regexp)))
                       (s-collapse-whitespace ,expr))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; lf-extract-optionals-from-rest ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro ⇒ (x y z  _  a b c)
    "Shorthand: (⇒ inputs :become outputs)"
  `(≋ (lf-extract-optionals-from-rest ,x ,y ,z) (quote (,a ,b ,c))))

(deftest "What if we don't have a vector first? Then we're starting the rest!"
  [lf-extract-optionals-from-rest]
  (⇒ nil nil nil :becomes nil nil nil)
  (⇒ 'x nil nil :becomes nil nil (x))
  (⇒ 'x 'y nil :becomes nil nil (x y))
  (⇒ 'x 'y '(z) :becomes nil nil (x y z)))

(deftest "What if we don't have a vector first, but have a string instead!"
  [lf-extract-optionals-from-rest]
  (⇒ "x" 'y '(z) :becomes nil "x" (y z))
  (⇒ "x" 'y nil  :becomes nil "x" (y))
  (⇒ "x" nil '(z) :becomes nil "x" (z))
  (⇒ "x" nil nil  :becomes nil "x" nil))

(deftest "What if we do have a vector first?"
  [lf-extract-optionals-from-rest]
  ;; … followed by a string
  (⇒ [] "" '(z) :becomes [] "" (z))
  (⇒ [] "" nil :becomes [] "" nil)
  ;; … followed by a non-string; i.e., the start of the rest
  (⇒ [] 'y '(z) :becomes [] nil (y z)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; lf-define ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftest "We can define both variables and functions"
  [lf-define]

  (lf-undefine age speak) ;; Ensure we have undefined names to use.
  (lf-define age 29)
  (≋ age 29)

  (lf-define speak (name) (format "Hello, %s!" name))
  (≋ (speak 'bob) "Hello, bob!"))

(deftest "If PLACE is a non-atomic form, then we default to using ‘setf’."
  [lf-define setf]
  (lf-undefine foods)
  (lf-define foods '(apple banana))
  (lf-define (car foods) 'pineapple)
  (≋ foods '(pineapple banana)))

(deftest "It “zaps”:  (lf-define x (f it))  ≈  (setq x (f x))"
  [lf-define]

  ;; A super simple zap!
  (lf-define x 12)
  (lf-define x (+ it 1))  ;; Note the ‘it’
  (≋ x 13)

  ;; Let's see a lengthier example, without zapping
  (lf-undefine age)
  (lf-define age 12)
  (setf (documentation-property 'age 'variable-documentation)
        (or "Ugly way to update docs" (documentation-property 'age 'variable-documentation)))
  (≋ "Ugly way to update docs" (documentation-property 'age 'variable-documentation))

  ;; Zap!
  (lf-define (documentation-property 'age 'variable-documentation)
             (or "New way to update docs" it))
  (≋ "New way to update docs" (documentation-property 'age 'variable-documentation))

  ;; Another zap!
  (lf-define (documentation-property 'age 'variable-documentation)
             (format "woah, %s" it))
  (≋ "woah, New way to update docs" (documentation-property 'age 'variable-documentation)))

(deftest "It documents variables"
  [lf-define]
  (lf-undefine age) ;; Ensure we have undefined names to use.
  (lf-define age 29 "How old am I?")
  (≋ (lf-documentation 'age) "How old am I?"))

(deftest "It can define strings, which can also be documented"
  [lf-define]
  (lf-undefine name)
  (lf-define name "Bobert" "What is my name?")
  (≋ (lf-documentation 'name) "What is my name?"))

(deftest "It can define strings, which do not need documentation"
  [lf-define]
  (lf-undefine name)
  (lf-define name "Bobert")
  (≋ name "Bobert")
  (≋ (lf-documentation 'name) nil))

(deftest "It can define strings, with typing and docstrings"
  [lf-define]
  (lf-undefine name)
  (lf-define name "Bobert" [:type string] "What is my name?")
  (≋ (lf-documentation 'name) "What is my name?"))

(deftest "It can define strings with types, but do docstring"
  [lf-define]
  (lf-undefine name)
  (lf-define name "Bobert" [:type string])
  (≋ (lf-documentation 'name) nil)
  (≋ name "Bobert"))

;; The new constraints are not checked against the new value!
(ert-deftest lf/define/new-constraints ()
   (lf-undefine age)
   (lf-define age "12" [:type string])
   (↯ (lf-define agr "12" [(and (integerp it) (stringp it))]))   ;; See, new constraints not checked!
   (↯ (lf-define age "123"
              [(and (integerp it) (<= 0 it 10))])))

;;  Type specifier vs predicates; both work 😄
(ert-deftest  lf/define/type-specifiers ()
  (lf-undefine age x)

  (✓ (lf-define age 12 [:type (integer 0 100)]))
  (× (lf-define age 123 [:type (integer 0 100)]))

  (✓ (lf-define age 12 [(and (integerp it) (<= 0 it 100))]))
  (× (lf-define age 123 [(and (integerp it) (<= 0 it 100))]))

  ;; (✓ (lf-define x 0 [:type (not null)] "hiya"))
  ;; (should-error (lf-define x nil)) ;; Error: New value violates existing constraints.
  ;; (✓ (lf-define x nil [:any-type] "oki")) ;; OK, redefine x to have always truthy constraints, “:any-type”. FIXME
)

(ert-deftest lf/define/vars/vectors/documented ()
  :tags '(define)
  (lf-undefine foods) ;; Ensure we have undefined names to use.

  (lf-define foods [shorba dolma] "What Iraqi foods do I like?")
  (≋ foods [shorba dolma])
  (≋ (documentation-property 'foods 'variable-documentation) "What Iraqi foods do I like?"))

(ert-deftest lf/define/vars/vectors/no-docs ()
  :tags '(define)
  (lf-undefine foods) ;; Ensure we have undefined names to use.

  (lf-define foods [shorba dolma])
  (≋ foods [shorba dolma])
  (≋ (documentation-property 'foods 'variable-documentation) nil))

(ert-deftest lf/define/vars/vectors/not-documented-but-typed ()
  :tags '(define)
  (lf-undefine foods) ;; Ensure we have undefined names to use.

  (lf-define foods [shorba dolam] [:type vector])
  (≋ (documentation-property 'foods 'variable-documentation) nil)
  (≋ foods [shorba dolam]))

(ert-deftest lf/define/documentation/functions ()
  :tags '(define)
  (lf-undefine speak) ;; Ensure we have undefined names to use.

  (lf-define speak (name) "An English greeting." (format "Hello, %s!" name))
  (⇝ (documentation-property 'speak 'function-documentation)
     (* anything)
     "An English greeting."
     (* anything)))

(ert-deftest lf/define/functions/no-empty-functions ()
  :tags '(define)

  (lf-undefine f)
  (lf-define f ()) ;; Warning: This is not the empty function; but is the empty list!
  (≋ f nil)
  (≋ (documentation-property 'f 'variable-documentation) nil)

  (lf-undefine f)
  (lf-define f () "Docs") ;; Warning: This is not the empty function; but is a documented empty list!
  (≋ f nil)
  (≋ (documentation-property 'f 'variable-documentation) "Docs")

  (lf-undefine f)
  (lf-define f () "Docs" nil) ;; Warning: This is not the empty function; but is a documented empty list!
  (≋ f nil)
  (≋ (documentation-property 'f 'variable-documentation) "Docs")

  (lf-undefine id) ;; Ensure we have undefined names to use.
  (lf-define id (x) "Identity" x)
  (⇝ (documentation 'id) (* anything) "Identity" (* anything))

  ;; We also cannot have empty functions that take arguments...
  ;; (lf-undefine f)
  ;; (lf-define f (x) "Docs") ;; Warning: This is not an empty function; but is a function call! We call “x” with no arguments!
  ;; (lf-define f (x y)) ;; This sets “f” to be the result of “x” applied to “y”!

  (lf-undefine f)
  (lf-define f () "Woah!" (and nil)) ;; This is an /indirect/ definition of an empty function
  (≋ (f) nil)
  (⇝ (documentation 'f ) (* anything) "Woah!" (* anything))

  (lf-undefine f)
  (lf-define f (x y) "Woah!" (and nil)) ;; This is an /indirect/ definition of an empty function, with args.
  (≋ (f 1 2) nil)
  (⇝ (documentation 'f ) (* anything) "Woah!" (* anything))
)

(ert-deftest lf/define/types/functions ()
  :tags '(define)
  (lf-undefine speak) ;; Ensure we have undefined names to use.

  (lf-define speak (name age)
             [:requires (and (stringp name) (integerp age))
              :ensures (stringp result)]
             "Greet person NAME with their AGE."
             (format "Hello %s year-old %s!" age name))

  ;; Docstring is present
  (⇝ (documentation-property 'speak 'function-documentation)
     "This function has :around advice: ‘lf--typing-advice/speak’.

      Greet person NAME with their AGE.

      (fn NAME AGE)")

  ;; Type-satisfied inputs yield an output
  (≋ (speak "musa" 29) "Hello 29 year-old musa!")

  ;; Type-invalid inputs yield an error
  (↯ (speak 'musa 29)           ;; i.e., symbol ≠ string.
     "Error: Requirements for “speak” have been violated.

      REQUIRED:
      (and
       (stringp name)
       (integerp age))

      GIVEN:
      ((name = musa : symbol)
       (age = 29 : integer))   ")

  ;; Uh-oh! We accidentally have an erroneous return type ;-|
  (lf-define speak (name age)
             [:requires (and (stringp name) (integerp age))
              :ensures (stringp result)]
             12)
  ;;
  (↯ (speak "musa" 29)
     "Panic! There is an error in the implementation of “speak”.
     Claimed guarantee: (stringp result)
     Actual result value: 12 ---typed: integer"))

(ert-deftest lf/define/function/expansion ()
  (lf-undefine add)

  (lf-define add (x) (+ 1 x))
  (≋ (add 4) 5)

  ;; Expansion is a cl-defun along with some typing advice added.
  (⇝ (pp-to-string (macroexpand '(lf-define add (x) (+ 1 x))))
     (* anything)
     "(cl-defun add
      (x)
    nil
    (+ 1 x))"
   (* anything)
   "(advice-add #'add :around 'lf--typing-advice/add)"))

(ert-deftest lf/define/function/docs/expansion ()
  (lf-undefine add)

  ;; Expansion is a cl-defun along with some typing advice added.
  (⇝ (pp-to-string (macroexpand '(lf-define add (x) "Hola" (+ 1 x))))
     (* anything)
     "(cl-defun add
      (x)
    \"Hola\"
    (+ 1 x))"
   (* anything)
   "(advice-add #'add :around 'lf--typing-advice/add)"))

(ert-deftest lf/define/function/docs-and-types/expansion ()
  (lf-undefine add)

  ;; Expansion is a cl-defun along with some typing advice added.
  (⇝ (pp-to-string (macroexpand '(lf-define add (x) [:requires (integerp x)] "My docs" (+ 1 x))))
     (* anything)
     "(cl-defun add
      (x)
    \"My docs\"
    (+ 1 x))"
   (* anything)
   "(advice-add #'add :around 'lf--typing-advice/add)"))

(ert-deftest lf/define/ensures/failure ()
  (lf-undefine speak)

  (lf-define speak (name age)
           [ :ensures (vectorp result) ]
          "Given a NAME and AGE, present a nice greeting."
           (format "Hello, %s!" name))

  (↯ (speak "mesa" 12)
     "Panic! There is an error in the implementation of “speak”.

      Claimed guarantee: (vectorp result)

      Actual result value: \"Hello, mesa!\" ---typed: string"))


(ert-deftest lf/define/ensures/success ()
  (lf-undefine speak)

  (lf-define speak (name age)
           [:requires (and (stringp name) (integerp age))
            :ensures (stringp result) ]
           "Given a NAME and AGE, present a nice greeting."
           (format "Hello, %s!" name))

  (should (speak "mesa" 12))

  (should-error (speak 'whoops 12)))
    ;; "Error: Requirements for “speak” have been violated.

    ;; REQUIRED:
    ;; (and
    ;;  (stringp name)
    ;;  (integerp age))

    ;; GIVEN:
    ;; ((name has value whoops which is typed: symbol)
    ;;  (age has value 12 which is typed: integer))"
