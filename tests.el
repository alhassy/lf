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
     :tags (quote ,(append tags nil))
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

(deftest "It works as expected, for multi-line strings"
  [lf-string]
  (≋ (lf-string    "0. This is the first line, it has zero indentation.
                     1. This line indicates the indentation offset for the remaining lines.
                     2. As such, this line has no indentation.
                        3. But this one is clearly indentated (relative to line 1)")

"0. This is the first line, it has zero indentation.
1. This line indicates the indentation offset for the remaining lines.
2. As such, this line has no indentation.
   3. But this one is clearly indentated (relative to line 1)"))

(deftest "It works as expected, for single-line strings"
  [lf-string]
  (≋ (lf-string "Hello my friends") "Hello my friends"))

(deftest "It allows interpolation, on a single line"
  [lf-string]
  (≋
   (let ((you 'Jasim))
     (lf-string "me and ${you} like the number ${(+ 2 3)}"))
   "me and Jasim like the number 5"))

(deftest "It allows interpolation, spread over multiple"
  [lf-string]
  (⇝
   (lf-string "Did you know that
               ${(documentation

                    'apply)}
               is super cool!")
   "Did you know that"
   (* anychar)
   "Call FUNCTION with our remaining args"
   (* anychar)
   "is super cool!"))

(deftest "It allows interpolation, involving string literals"
  [lf-string]
  (≋
   (let ((you 'Jasim))
     (lf-string "${(concat \"m\" \"e\")} and ${you}"))
     "me and Jasim"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; lf-extract-optionals-from-rest ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro ⇒ (x y z  _  a b c)
    "Shorthand: (⇒ inputs :become outputs)"
  `(≋ (lf-extract-optionals-from-rest ,x #'vectorp ,y #'stringp ,z) (quote (,a ,b ,c))))

(deftest "[vector-then-string]What if we don't have a vector first? Then we're starting the rest!"
  [lf-extract-optionals-from-rest]
  (⇒ nil nil nil :becomes nil nil nil)
  (⇒ 'x nil nil :becomes nil nil (x))
  (⇒ 'x 'y nil :becomes nil nil (x y))
  (⇒ 'x 'y '(z) :becomes nil nil (x y z)))

(deftest "[vector-then-string]What if we don't have a vector first, but have a string instead!"
  [lf-extract-optionals-from-rest]
  (⇒ "x" 'y '(z) :becomes nil "x" (y z))
  (⇒ "x" 'y nil  :becomes nil "x" (y))
  (⇒ "x" nil '(z) :becomes nil "x" (z))
  (⇒ "x" nil nil  :becomes nil "x" nil))

(deftest "[vector-then-string]What if we do have a vector first?"
  [lf-extract-optionals-from-rest]
  ;; … followed by a string
  (⇒ [] "" '(z) :becomes [] "" (z))
  (⇒ [] "" nil :becomes [] "" nil)
  ;; … followed by a non-string; i.e., the start of the rest
  (⇒ [] 'y '(z) :becomes [] nil (y z)))


(defmacro ⇒₂ (x y z  _  a b c)
    "Shorthand: (⇒ inputs :become outputs)"
  `(≋ (lf-extract-optionals-from-rest ,x #'stringp ,y #'numberp ,z) (quote (,a ,b ,c))))

(deftest "[string-then-number]What if we don't have a number first? Then we're starting the rest!"
  [lf-extract-optionals-from-rest]
  (⇒₂ nil nil nil :becomes nil nil nil)
  (⇒₂ 'x nil nil :becomes nil nil (x))
  (⇒₂ 'x 'y nil :becomes nil nil (x y))
  (⇒₂ 'x 'y '(z) :becomes nil nil (x y z)))

(deftest "[string-then-number]What if we don't have a string first, but have a number instead!"
  [lf-extract-optionals-from-rest]
  (⇒₂ 34 'y '(z) :becomes nil  34 (y z))
  (⇒₂ 34 'y nil  :becomes nil  34 (y))
  (⇒₂ 34 nil '(z) :becomes nil 34 (z))
  (⇒₂ 34 nil nil  :becomes nil 34 nil))

(deftest "[string-then-number]What if we do have a string first?"
  [lf-extract-optionals-from-rest]
  ;; … followed by a string
  (⇒₂ "" 0 '(z) :becomes "" 0 (z))
  (⇒₂ "" 0 nil :becomes  "" 0 nil)
  ;; … followed by a non-number; i.e., the start of the rest
  (⇒₂ "" 'y '(z) :becomes "" nil (y z)))

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
(deftest "New constraints on variables override any existing ones"
  [lf-define]
  (lf-undefine age)

  ;; Initial value satisfies declared constraints
  (lf-define age "12" [:type string])

  ;; New *initial* value satisifes OLD constraints, which are dismissed
  ;; in-favour of the newly declared constraints. The new value does not
  ;; satisfy the new constraints.
  (↯ (lf-define age "12" [(and (integerp it) (stringp it))])
      "Error: Initial value “12” violates declared constraint:
        [(and (integerp it) (stringp it))]

       As such, symbol “age” is now unbound and unconstrained.")

  ;; New initial value satisfies the newly declared constraints
  (✓ (lf-define age 9 [(and (integerp it) (<= 0 it 10))]))

  ;; Yet again...
  (lf-undefine age) ;; Ensure we have undefined names to use.
  (✓ (lf-define age 29 [:type integer] "How old am I?")) ;; OK
  (✓ (lf-define age "twenty-nine" [:type string] "How old am I?")) ;; OK
  (✓ (lf-define age 'twenty-nine [:type symbol])) ;; OK
  )

(deftest "It allows us to define documented vector variables"
  [lf-define]
  (lf-undefine foods) ;; Ensure we have undefined names to use.
  (lf-define foods [shorba dolma] "What Iraqi foods do I like?")
  (≋ foods [shorba dolma])
  (≋ (documentation-property 'foods 'variable-documentation) "What Iraqi foods do I like?"))

(deftest "It allows us to define undocumented vector variables"
  [lf-define]
  (lf-undefine foods) ;; Ensure we have undefined names to use.
  (lf-define foods [shorba dolma])
  (≋ foods [shorba dolma])
  (≋ (documentation-property 'foods 'variable-documentation) nil))

(deftest "It allows us to define undocumented but typed vector variables"
  [lf-define]
  (lf-undefine foods) ;; Ensure we have undefined names to use.
  (lf-define foods [shorba dolam] [:type vector])
  (≋ (documentation-property 'foods 'variable-documentation) nil)
  (≋ foods [shorba dolam]))

(deftest "It defines documented functions"
  [lf-define]
  (lf-undefine speak) ;; Ensure we have undefined names to use.
  (lf-define speak (name) "An English greeting." (format "Hello, %s!" name))
  (⇝ (documentation #'speak) "An English greeting."))

;; lf-define does not allow the direct definition of empty functions (always “nil”
;; functions), which is okay since they are seldom used in practice.
(deftest "It does not allow the direct definition of empty functions"
  [lf-define]

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
  (⇝ (documentation #'id) "Identity")

  ;; We also cannot have empty functions that take arguments...
  ;; (lf-undefine f)
  ;; (lf-define f (x) "Docs") ;; Warning: This is not an empty function; but is a function call! We call “x” with no arguments!
  ;; (lf-define f (x y)) ;; This sets “f” to be the result of “x” applied to “y”!

  (lf-undefine f)
  (lf-define f () "Woah!" (and nil)) ;; This is an /indirect/ definition of an empty function
  (≋ (f) nil)
  (⇝ (documentation #'f) "Woah!")

  (lf-undefine f)
  (lf-define f (x y) "Woah!" (and nil)) ;; This is an /indirect/ definition of an empty function, with args.
  (≋ (f 1 2) nil)
  (⇝ (documentation #'f ) "Woah!"))

(deftest "It can define typed functions"
  [lf-define]

  (lf-undefine speak) ;; Ensure we have undefined names to use.

  (lf-define speak (name age)
             [:requires (and (stringp name) (integerp age))
              :ensures (stringp result)]
             "Greet person NAME with their AGE."
             (format "Hello %s year-old %s!" age name))

  ;; Docstring is present (advice notice and user docstring may appear in either order)
  (let ((doc (documentation (function speak))))
    (✓ (s-matches? "Greet person NAME with their AGE\\." doc))
    (✓ (s-matches? "lf--specify/speak" doc))
    (✓ (s-matches? "(fn NAME AGE)" doc)))

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
  ;; Any use yields an error:
  (↯ (speak "musa" 29)
     "Panic! There is an error in the implementation of “speak”.
     Claimed guarantee: (stringp result)
     Actual result value: 12 ---typed: integer"))



;; Function definition expansions

(deftest "It expands function definitions to cl-defun's, with typing advice"
  [lf-define]
  (⇝ (pp-to-string (macroexpand '(lf-define add (x) (+ 1 x))))
     "(cl-defun add (x)
        nil
        (+ 1 x))

      (lf-specify add (x) :requires t :ensures t)"))

(deftest "It expands documented function definitions correctly, with typing advice"
  [lf-define]
  (⇝ (pp-to-string (macroexpand '(lf-define add (x) "Hola" (+ 1 x))))
     " (cl-defun add (x)
         \"Hola\"
         (+ 1 x))

      (lf-specify add (x) :requires t :ensures t)"))

(deftest "It expands typed, and documented, function definitions into cl-defun's, with typing advice"
  [lf-define]
  (⇝ (pp-to-string (macroexpand '(lf-define add (x) [:requires (integerp x)] "My docs" (+ 1 x))))
     "(cl-defun add (x)
        \"My docs\"
        (+ 1 x))

      (lf-specify add (x) :requires (integerp x) :ensures t)"))



;; When does a function panic/error out?

(deftest "Poorly implemented functions panic when used"
  [lf-define]
  (lf-undefine speak)
  (lf-define speak (name age)
           [ :ensures (vectorp result) ]
          "Given a NAME and AGE, present a nice greeting."
           (format "Hello, %s!" name))
  (↯ (speak "mesa" 12)
     "Panic! There is an error in the implementation of “speak”.

      Claimed guarantee: (vectorp result)

      Actual result value: \"Hello, mesa!\" ---typed: string"))

(deftest "Coorectly implemented functions do not panic when used with well-typed arguments"
  [lf-define]
  (lf-undefine speak)
  (lf-define speak (name age)
           [:requires (and (stringp name) (integerp age))
            :ensures (stringp result) ]
           "Given a NAME and AGE, present a nice greeting."
           (format "Hello, %s!" name))

  (✓ (speak "mesa" 12))
  (↯ (speak 'whoops 12)
     "Error: Requirements for “speak” have been violated.

      REQUIRED:
      (and
       (stringp name)
       (integerp age))

      GIVEN:
      ((name = whoops : symbol)
       (age = 12 : integer))     "))




;; 🔥 Post assertions can be relative to input and output arguments

(deftest "Post assertions can be relative to input and output arguments"
  [lf-define]
  (lf-undefine next)
  (lf-define next (x)
             "This function resturns a value strictly greater than input X."
             [:requires (integerp x)
              :ensures  (< x result)]
             (1+ x))
  (≋ (next 2) 3))




;; :fire: It lets you use pre- and post-conditions with custom error messages, by using or-error

(deftest "It lets you use pre- and post-conditions with custom error messages, by using or-error"
  [lf-define]
  (lf-undefine go)
  (lf-define go (x) [:requires (or (integerp x) (error "I asked you for a bloody integer!"))] x)
  (✓ (go 3))
  (↯ (go "3") "I asked you for a bloody integer!"))
