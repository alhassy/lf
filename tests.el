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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; lf-unindent ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; `lf-string' calls `lf-unindent' as its very first step, so the
;; multi-line-string tests below already re-derive most of this
;; behaviour --- but only implicitly, through `lf-string''s own
;; interpolation on top.  These test `lf-unindent' directly, so a
;; regression here points straight back at itself.

(deftest "lf-unindent leaves a single-line string unchanged"
  [lf-unindent]
  ;; No second line exists to derive a common indent from, so
  ;; `min-indent' falls back to 0 --- the string passes through as-is.
  (≋ (lf-unindent "Hello, world.") "Hello, world."))

(deftest "lf-unindent strips the common leading whitespace of a multi-line string"
  [lf-unindent]
  ;; The worked example from `lf-unindent''s own docstring.
  (≋ (lf-unindent
      "1. This is the first line, it has zero indentation.
       2. This line indicates the indentation offset for the remaining lines.
       3. As such, this line has no indentation.
          4. But this one is clearly indented (relative to line 2)")
     "1. This is the first line, it has zero indentation.
2. This line indicates the indentation offset for the remaining lines.
3. As such, this line has no indentation.
   4. But this one is clearly indented (relative to line 2)"))

(deftest "lf-unindent ignores blank lines when computing the common indent"
  [lf-unindent]
  ;; A wholly-blank line (however many/few spaces) must not drag the
  ;; computed minimum down to 0 --- `rest-non-blank' filters it out.
  (≋ (lf-unindent
      "1. First line, zero indentation.
       2. Second line sets the offset.

       3. Third line, same offset as the second.")
     "1. First line, zero indentation.
2. Second line sets the offset.

3. Third line, same offset as the second."))

(deftest "lf-unindent keeps a line's EXTRA indentation relative to the common minimum"
  [lf-unindent]
  ;; Line 4 (8 leading spaces) is 3 columns deeper than the common
  ;; minimum (5 leading spaces, set by lines 2-3) --- stripping the
  ;; common minimum must leave exactly that 3-column relative
  ;; indentation behind, not truncate the line flush to column 0.
  (≋ (lf-unindent
      "1. Zero indentation.
     2. Five-space indentation, sets the common minimum.
     3. Also five-space indentation.
        4. Eight-space indentation --- three deeper than the minimum.")
     "1. Zero indentation.
2. Five-space indentation, sets the common minimum.
3. Also five-space indentation.
   4. Eight-space indentation --- three deeper than the minimum."))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; lf-string ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;; `lf-define''s own docstring shows three forms claimed to be
;; equivalent: a compound CL type-specifier alone, a bare type plus a
;; separate `(satisfies ...)' clause, and the two joined with `and'.
;; All three ride on `:type''s `cl-typep' dispatch --- `satisfies' is
;; not something `lf-define' implements itself, it comes free from
;; `cl-typep' recognising that CL type-specifier form.

(deftest "[:type (integer 0 100)] accepts in-range and rejects out-of-range values"
  [lf-define]
  (lf-undefine age)
  (✓ (lf-define age 50 [:type (integer 0 100)]))
  (↯ (lf-define age 150 [:type (integer 0 100)])))

(deftest "[:type integer (satisfies ...)] accepts in-range and rejects out-of-range values"
  [lf-define]
  (lf-undefine age)
  (✓ (lf-define age 50 [:type integer (satisfies (lambda (value) (<= 0 value 100)))]))
  (↯ (lf-define age 150 [:type integer (satisfies (lambda (value) (<= 0 value 100)))])))

(deftest "[:type (and integer (satisfies ...))] accepts in-range and rejects out-of-range values"
  [lf-define]
  (lf-undefine age)
  (✓ (lf-define age 50
                [:type (and integer (satisfies (lambda (value) (<= 0 value 100))))]))
  (↯ (lf-define age 150
                [:type (and integer (satisfies (lambda (value) (<= 0 value 100))))])))

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

  ;; Type-invalid inputs yield an error.  `⇝', not `↯' with an exact
  ;; message, since `lf-specify' now appends a "CALLED FROM: ..."
  ;; call-chain trailer (see the `lf-specify' backtrace tests below) ---
  ;; that trailer's content is inherently unstable across invocation
  ;; contexts (`M-x ert' vs. batch vs. `emacsclient' each produce a
  ;; different chain above this very call), so only the REQUIRED/GIVEN
  ;; portion --- everything `lf-specify' itself controls --- is worth
  ;; asserting exactly here.
  (↯ (speak 'musa 29))           ;; i.e., symbol ≠ string.
  (⇝ (cl-second (should-error (speak 'musa 29)))
     "Error: Requirements for “speak” have been violated.

      REQUIRED:
      (and
       (stringp name)
       (integerp age))

      GIVEN:
      ((name = musa : symbol)
       (age = 29 : integer))")

  ;; Uh-oh! We accidentally have an erroneous return type ;-|
  (lf-define speak (name age)
             [:requires (and (stringp name) (integerp age))
              :ensures (stringp result)]
             12)
  ;; Any use yields an error:
  (⇝ (cl-second (should-error (speak "musa" 29)))
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
  (⇝ (cl-second (should-error (speak "mesa" 12)))
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
  (⇝ (cl-second (should-error (speak 'whoops 12)))
     "Error: Requirements for “speak” have been violated.

      REQUIRED:
      (and
       (stringp name)
       (integerp age))

      GIVEN:
      ((name = whoops : symbol)
       (age = 12 : integer))"))




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



;; 🐛 Regression: `:requires'/`:ensures' must see each ARG bound to its
;; real call-time value AND the wrapped function must keep its real
;; arity.  A prior implementation of `lf-specify' advised with a bare
;; `(&rest ,args)' parameter list --- e.g. for `(name age)' this
;; expanded to `&rest (name age)', which `cl-defun' silently accepted
;; as a *destructuring* `&rest' pattern (a `cl-lib' extension over
;; plain `defun'), not an error.  Two symptoms fell out of that, both
;; easy to miss with a single "does it basically work" smoke test:
;;
;;   1. Called with EXACTLY as many args as ARGS has names, destructuring
;;      happens to bind each name correctly --- the wrapped function
;;      *appears* to work for the common case.
;;   2. Called with any OTHER number of args (one too many, one too
;;      few), `cl-defun' signals `wrong-number-of-arguments' --- the
;;      advice silently turned a function of arbitrary/variadic arity
;;      into one rigidly demanding exactly `(length ARGS)' arguments.
;;   3. Whenever ARGS itself contains `&optional'/`&rest' (e.g. `(jql
;;      &optional fields max)'), `&rest (jql &optional fields max)' is
;;      not valid destructuring syntax at all, so every call reliably
;;      failed with `(void-variable jql)' or similar.
;;
;; `cl-destructuring-bind' over a *dedicated* `&rest' capture is what
;; fixes all three: it binds every name in ARGS to its real value,
;; understands `&optional'/`&rest' markers within ARGS, and never
;; changes the wrapped function's actual calling convention.

(deftest "Two-argument :requires sees both arguments bound, not void-variable"
  [lf-define]
  (lf-undefine speak)
  (lf-define speak (name age)
             [:requires (and (stringp name) (integerp age))
              :ensures  (stringp result)]
             "Greet person NAME with their AGE."
             (format "Hello %s year-old %s!" age name))
  (≋ (speak "musa" 29) "Hello 29 year-old musa!")
  (⇝ (cl-second (should-error (speak 'musa 29)))
     "Error: Requirements for “speak” have been violated.

      REQUIRED:
      (and
       (stringp name)
       (integerp age))

      GIVEN:
      ((name = musa : symbol)
       (age = 29 : integer))"))

(deftest "Three-or-more-argument :requires sees every argument bound"
  [lf-define]
  (lf-undefine between)
  (lf-define between (lo x hi)
             [:requires (and (integerp lo) (integerp x) (integerp hi))
              :ensures  (booleanp result)]
             "Non-nil iff X lies within [LO, HI]."
             (and (<= lo x) (<= x hi)))
  (✓ (between 0 5 10))
  (↯ (between 0 "five" 10)))

;; This is the sharpest regression pin: a destructuring `&rest' pattern
;; masquerading as the parameter list silently rewrites the wrapped
;; function's arity to a fixed count.  A `&optional' arg genuinely
;; being omitted (a normal, valid call) would previously raise
;; `wrong-number-of-arguments' instead of just defaulting.

(deftest "lf-define preserves the wrapped function's real arity"
  [lf-define]
  (lf-undefine greet)
  (lf-define greet (name &optional greeting)
             [:requires (stringp name)
              :ensures  (stringp result)]
             "Greet NAME, defaulting GREETING to \"Hello\"."
             (format "%s, %s!" (or greeting "Hello") name))
  (≋ (greet "musa") "Hello, musa!")
  (≋ (greet "musa" "Marhaba") "Marhaba, musa!")
  (↯ (greet "musa" "Marhaba" "extra-arg-should-not-be-accepted")))

;; `lf-define'/`lf-specify' over an ARGS list containing `&optional' ---
;; e.g. `(jql &optional fields max)' --- previously had no coverage at
;; all; `cl-destructuring-bind' must understand the marker (not just
;; plain symbols) for `:requires' to see FIELDS/MAX bound correctly
;; whether or not the caller supplied them.

(deftest "&optional arguments are bound (present or defaulted) inside :requires"
  [lf-define]
  (lf-undefine search-url)
  (lf-define search-url (jql &optional fields max)
             [:requires (and (stringp jql)
                             (or (null fields) (stringp fields))
                             (or (null max) (integerp max)))
              :ensures  (stringp result)]
             "Build a search URL for JQL, FIELDS (default summary), MAX (default 50)."
             (format "jql=%s&fields=%s&max=%d" jql (or fields "summary") (or max 50)))
  (≋ (search-url "status:open") "jql=status:open&fields=summary&max=50")
  (≋ (search-url "status:open" "title" 10) "jql=status:open&fields=title&max=10")
  (↯ (search-url "status:open" "title" "ten")))

(deftest "&rest arguments are bound to the trailing list inside :requires"
  [lf-define]
  (lf-undefine sum-all)
  (lf-define sum-all (label &rest nums)
             [:requires (and (stringp label) (-all-p #'integerp nums))
              :ensures  (integerp result)]
             "Sum NUMS, LABEL is unused (documents the call site only)."
             (ignore label)
             (apply #'+ nums))
  (≋ (sum-all "total" 1 2 3) 6)
  (≋ (sum-all "empty") 0)
  (↯ (sum-all "total" 1 "two" 3)))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; lf-declare-type ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro with-lf-declare-type (decl &rest body)
  "Evaluate BODY with DECL active, then clean up the env and advice."
  (declare (indent 1))
  `(unwind-protect
       (progn ,decl ,@body)
     (setq lf--declared-type-env nil)
     (advice-remove 'eval #'lf--type-check-eval-advice)))

(defmacro with-lf-declare-type-cleanup (&rest body)
  "Like `with-lf-declare-type', but for BODY that builds its own
`(eval \\='(lf-declare-type ...))' call --- for a DECL that errors at
macroexpansion time (e.g. a malformed type spec), before
`with-lf-declare-type' could ever wrap it, since the error fires
while reading the unevaluated `decl' argument itself."
  (declare (indent 0))
  `(unwind-protect (progn ,@body)
     (setq lf--declared-type-env nil)
     (advice-remove 'eval #'lf--type-check-eval-advice)))

;; lf-declare-type works via macroexpand-all; it applies to user-defined
;; macros and functions, not to built-in C functions like `nth' whose
;; calls are never processed through `macroexpand-all'.  We demo it
;; with a thin user-defined wrapper so the tests stay deterministic.

(defun nth-wrapper (n list) (nth n list))

(deftest "lf-declare-type for nth-wrapper installs eval advice"
  [lf-declare-type]
  (with-lf-declare-type (lf-declare-type nth-wrapper :: integer list -> alpha)
    (✓ (advice-member-p #'lf--type-check-eval-advice 'eval))))

(deftest "Good nth-wrapper calls still work after lf-declare-type"
  [lf-declare-type]
  (with-lf-declare-type (lf-declare-type nth-wrapper :: integer list -> alpha)
    (≋ (eval '(nth-wrapper 0 '(a b c))) 'a)
    (≋ (eval '(nth-wrapper 2 '(a b c))) 'c)))

(deftest "nth-wrapper with a non-integer index produces a friendly error"
  [lf-declare-type]
  (with-lf-declare-type (lf-declare-type nth-wrapper :: integer list -> alpha)
    (↯ (eval '(nth-wrapper "zero" '(a b c)))
       "(nth-wrapper ... \"zero\" ...) ---expected ‘integerp’, got \"zero\" (string).")))

(deftest "nth-wrapper with a non-list second arg produces a friendly error"
  [lf-declare-type]
  (with-lf-declare-type (lf-declare-type nth-wrapper :: integer list -> alpha)
    (↯ (eval '(nth-wrapper 0 "not-a-list"))
       "(nth-wrapper ... \"not-a-list\" ...) ---expected ‘listp’, got \"not-a-list\" (string).")))

(deftest "lf-declare-type works identically with the leading :: omitted"
  [lf-declare-type]
  ;; `(T1 T2 ... -> R)' and `(:: T1 T2 ... -> R)' are the same spec ---
  ;; `lf--parse-type-spec' only strips a LEADING `::' when present.
  (with-lf-declare-type (lf-declare-type nth-wrapper integer list -> alpha)
    (≋ (eval '(nth-wrapper 0 '(a b c))) 'a)
    (↯ (eval '(nth-wrapper "zero" '(a b c)))
       "(nth-wrapper ... \"zero\" ...) ---expected ‘integerp’, got \"zero\" (string).")))

(deftest "lf-declare-type signals a clear error at declaration time when -> is missing"
  [lf-declare-type]
  (with-lf-declare-type-cleanup
    (↯ (eval '(lf-declare-type nth-wrapper :: integer list))
       "lf-declare-type: missing ‘->‘ in type spec (integer list)")))

(deftest "lf-declare-type signals a clear error at declaration time for an unknown type name"
  [lf-declare-type]
  (with-lf-declare-type-cleanup
    (↯ (eval '(lf-declare-type nth-wrapper :: frobnicatable list -> alpha))
       "lf-declare-type: no predicate for type ‘frobnicatable’ (tried ‘frobnicatablep’)")))

(deftest "nth-wrapper called with the wrong number of arguments produces a friendly error"
  [lf-declare-type]
  ;; Distinct from the existing "right arity, wrong type" tests above:
  ;; here the arity itself --- not any argument's type --- is wrong.
  (with-lf-declare-type (lf-declare-type nth-wrapper :: integer list -> alpha)
    (↯ (eval '(nth-wrapper 0 '(a b c) 99))
       "(nth-wrapper 0 '(a b c) 99) ---expected arg count divisible by 2; got 3.")))

;; A Greek-letter type-variable used as an INPUT type (not just in
;; return position, where `alpha' has appeared in every test above but
;; is discarded/unchecked anyway).  `nth-wrapper' won't do here ---
;; `nth' itself demands an integer first argument independently of any
;; `lf' checking, so a genuinely-unconstrained-by-lf first argument
;; would still trip over `nth''s own type requirement and produce a
;; misleadingly-similar error.  `tag-wrapper' has no such builtin
;; expectation on its first argument.

(defun tag-wrapper (tag list) (cons tag list))

(deftest "A Greek-letter input type-variable is truly unconstrained"
  [lf-declare-type]
  (with-lf-declare-type (lf-declare-type tag-wrapper :: alpha list -> beta)
    (≋ (eval '(tag-wrapper "any string at all" '(a b c))) '("any string at all" a b c))
    (≋ (eval '(tag-wrapper 'any-symbol-at-all '(a b c))) '(any-symbol-at-all a b c))
    ;; The second parameter (`list') is still enforced, unaffected by
    ;; the first parameter's own variable-ness.
    (↯ (eval '(tag-wrapper "tag" "not-a-list"))
       "(tag-wrapper ... \"not-a-list\" ...) ---expected ‘listp’, got \"not-a-list\" (string).")))

;; lf-specify with bounds checking (index ≥ 0, index < length) ;;;;;;;;;;;;;;
;;
;; Note: lf-specify on built-in C functions like `nth' causes infinite
;; recursion (the advice re-enters the same C function).  We demo the
;; same bounds-checking pattern on a thin user-defined wrapper instead.

(deftest "lf-specify catches negative indices on a safe-nth wrapper"
  [lf-specify]
  (lf-undefine safe-nth)
  (defun safe-nth (n list) (nth n list))
  (lf-specify safe-nth (n list)
    :requires (and (integerp n) (listp list) (<= 0 n) (< n (length list))))
  (↯ (safe-nth -1 '(a b c)))
  (advice-remove 'safe-nth #'lf--specify/safe-nth))

(deftest "lf-specify catches out-of-bounds indices on a safe-nth wrapper"
  [lf-specify]
  (lf-undefine safe-nth)
  (defun safe-nth (n list) (nth n list))
  (lf-specify safe-nth (n list)
    :requires (and (integerp n) (listp list) (<= 0 n) (< n (length list))))
  (↯ (safe-nth 5 '(a b c)))
  (advice-remove 'safe-nth #'lf--specify/safe-nth))

(deftest "lf-specify allows valid in-bounds access on a safe-nth wrapper"
  [lf-specify]
  (lf-undefine safe-nth)
  (defun safe-nth (n list) (nth n list))
  (lf-specify safe-nth (n list)
    :requires (and (integerp n) (listp list) (<= 0 n) (< n (length list))))
  (≋ (safe-nth 1 '(a b c)) 'b)
  (advice-remove 'safe-nth #'lf--specify/safe-nth))

;; lf-specify's error messages include a call-chain backtrace ;;;;;;;;;;;;;;;
;;
;; Before this, an `lf-specify' error named only the immediately failing
;; function, with no indication of what called it --- fine for a
;; one-level call, unhelpful once real code has several layers.  These
;; tests exercise a genuine multi-level chain (outer -> middle -> inner)
;; and confirm the "CALLED FROM: ..." trailer actually names every
;; intermediate caller, not just the innermost failure.

(deftest "lf-specify's error names every function in a multi-level call chain"
  [lf-specify]
  (lf-undefine bt-outer bt-middle bt-inner)
  (lf-define bt-inner (x) [:requires (integerp x)] "innermost" (* x 2))
  (lf-define bt-middle (x) "middle calls inner" (bt-inner x))
  (lf-define bt-outer (x) "outer calls middle" (bt-middle x))
  (let ((msg (cl-second (should-error (bt-outer "not-an-int")))))
    (✓ (s-matches? "CALLED FROM:" msg))
    (✓ (s-matches? "bt-middle" msg))
    (✓ (s-matches? "bt-outer" msg))
    ;; The chain reads outer-most-caller-last (innermost function's own
    ;; advice frame comes first, since that's where the error fires) ---
    ;; `bt-middle' must appear before `bt-outer' in the trailer text.
    (✓ (< (string-match "bt-middle" msg) (string-match "bt-outer" msg)))))

(deftest "lf-specify's error omits the CALLED FROM trailer when the chain is empty"
  [lf-specify]
  ;; `lf--backtrace-string' returns nil (not an empty string) when
  ;; there is nothing above the marker function worth reporting ---
  ;; confirmed directly here, independent of any real error message.
  (≋ (lf--backtrace-string 'a-marker-function-nothing-calls) nil))

(deftest "lf-specify's :ensures (Panic!) error also includes the CALLED FROM trailer"
  [lf-specify]
  (lf-undefine bt-outer2 bt-inner2)
  (lf-define bt-inner2 (x)
             [:ensures (stringp result)]
             "innermost, deliberately wrong return type"
             (ignore x) 42)
  (lf-define bt-outer2 (x) "calls inner2" (bt-inner2 x))
  (let ((msg (cl-second (should-error (bt-outer2 1)))))
    (✓ (s-matches? "Panic!" msg))
    (✓ (s-matches? "CALLED FROM:" msg))
    (✓ (s-matches? "bt-outer2" msg))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; lf-dump / lf-dump-with-backtrace / lf-dump* / lf-dump-if / lf-dump*if ;;;;;;
;;
;; `lf-specify''s :requires failure message has always built a
;; "NAME = VALUE" list for its arguments --- these macros pull that
;; idea out into a standalone, general-purpose family (inspired by
;; eldev's `eldev-dump', ported/trimmed rather than depended on),
;; usable for ad-hoc debugging prints outside of `lf-specify' entirely.
;; `message' is captured here via a scoped `:before' advice rather
;; than asserted against `*Messages*' directly, so these tests don't
;; depend on --- or pollute --- the real message log.

(defmacro with-captured-messages (var &rest body)
  "Run BODY with `message' calls captured (not printed) into VAR, a list
of format-args results, newest last."
  (declare (indent 1))
  `(let ((,var nil))
     (advice-add 'message :before
                 (lambda (fmt &rest args)
                   (push (apply #'format fmt args) ,var)))
     (unwind-protect (progn ,@body)
       (advice-mapc (lambda (fn _) (advice-remove 'message fn)) 'message))
     (setq ,var (nreverse ,var))))

;; `lf-dump' is defined in terms of `lf-assert' (see `lf-dump''s
;; docstring) and so, like `lf-assert', its error/report path always
;; includes a "CALLED FROM: ..." trailer (see the backtrace tests
;; above) --- these tests check the DUMP portion of the message with
;; `s-matches?' (substring), not an exact `≋', so they don't have to
;; hardcode that trailer's inherently context-dependent content.

(deftest "lf-dump prints each literal form bare, without a value"
  [lf-dump]
  ;; All forms join into ONE `message' call (one multi-line string),
  ;; not one `message' per form --- `lf--dump-pairs' is a single
  ;; `mapconcat' over every pair.
  (with-captured-messages msgs
    (lf-dump 42 "str" 'foo)
    (≋ (length msgs) 1)
    (✓ (s-matches? "42\n\"str\"\n'foo" (car msgs)))))

(deftest "lf-dump prints a non-literal form's value alongside the form"
  [lf-dump]
  (with-captured-messages msgs
    (lf-dump (+ 1 2))
    (✓ (s-matches? "(\\+ 1 2) = 3" (car msgs)))))

(deftest "lf-dump mixes literal and non-literal forms in one dump"
  [lf-dump]
  (with-captured-messages msgs
    (lf-dump 42 (+ 1 2) "str")
    (✓ (s-matches? "42\n(\\+ 1 2) = 3\n\"str\"" (car msgs)))))

(deftest "lf-dump evaluates each form exactly once"
  [lf-dump]
  (let ((counter 0))
    (with-captured-messages msgs
      (lf-dump (cl-incf counter))
      (≋ counter 1)
      (✓ (s-matches? "(cl-incf counter) = 1" (car msgs))))))

(deftest "lf-dump with no forms prints nothing"
  [lf-dump]
  (with-captured-messages msgs
    (lf-dump)
    (≋ msgs nil)))

(deftest "lf-dump-with-backtrace appends a CALLED FROM trailer naming the real caller chain"
  [lf-dump]
  (lf-undefine dump-bt-outer dump-bt-inner)
  (defun dump-bt-inner () (lf-dump-with-backtrace (+ 1 2)))
  (defun dump-bt-outer () (dump-bt-inner))
  (with-captured-messages msgs
    (dump-bt-outer)
    (let ((msg (car msgs)))
      (✓ (s-matches? "(\\+ 1 2) = 3" msg))
      (✓ (s-matches? "CALLED FROM:" msg))
      (✓ (s-matches? "dump-bt-inner" msg))
      (✓ (s-matches? "dump-bt-outer" msg)))))

(deftest "lf-dump-if only dumps (and only evaluates its forms) when COND is non-nil"
  [lf-dump]
  (let ((counter 0))
    (with-captured-messages msgs
      (lf-dump-if nil (cl-incf counter))
      (≋ counter 0)                    ;; form never evaluated
      (≋ msgs nil)))                   ;; nothing printed
  (with-captured-messages msgs
    (lf-dump-if t (+ 1 2))
    (✓ (s-matches? "(\\+ 1 2) = 3" (car msgs)))))

(deftest "lf-dump* and lf-dump*if invoke the debugger after dumping"
  [lf-dump]
  ;; `debug' itself is stubbed (not actually entering a recursive
  ;; edit in batch ERT) --- confirms the invocation happens, without
  ;; needing an interactive debugger to drive.
  (let ((debug-called nil))
    (cl-letf (((symbol-function 'debug) (lambda (&rest _) (setq debug-called t))))
      (with-captured-messages msgs
        (lf-dump* (+ 1 2))
        (✓ debug-called)
        (✓ (s-matches? "(\\+ 1 2) = 3" (car msgs))))))
  (let ((debug-called nil))
    (cl-letf (((symbol-function 'debug) (lambda (&rest _) (setq debug-called t))))
      (lf-dump*if nil (+ 1 2))
      (≋ debug-called nil)))           ;; COND nil --- debugger never entered
  (let ((debug-called nil))
    (cl-letf (((symbol-function 'debug) (lambda (&rest _) (setq debug-called t))))
      (lf-dump*if t (+ 1 2))
      (✓ debug-called))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; lf-assert ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; The ad-hoc, no-function-definition-required cousin of `lf-specify':
;; assert a property of each of several expressions in one call, `it'
;; bound to each expression's own value for its paired condition.

(deftest "lf-assert returns t when every pair's condition holds"
  [lf-assert]
  (≋ (lf-assert 4 (cl-evenp it)
                "Hello" (and (stringp it) (> (length it) 3)))
     t))

(deftest "lf-assert signals an error naming the failing expression, its value, and type"
  [lf-assert]
  (let ((msg (cl-second (should-error (lf-assert (+ 1 2) (cl-evenp it))))))
    (✓ (s-matches? "lf-assert failed" msg))
    (✓ (s-matches? "(\\+ 1 2)" msg))
    (✓ (s-matches? "3" msg))
    (✓ (s-matches? "integer" msg))))

(deftest "lf-assert's error shows EVERY pair, not just the first failing one"
  [lf-assert]
  ;; The string clause here passes; it must still appear in the error
  ;; text for context, alongside the failing integer clause.
  (let ((msg (cl-second (should-error
                          (lf-assert (+ 1 2)  (cl-evenp it)
                                     "Hello"  (stringp it))))))
    (✓ (s-matches? "\\+ 1 2" msg))
    (✓ (s-matches? "Hello" msg))))

(deftest "lf-assert evaluates each expression exactly once"
  [lf-assert]
  (let ((counter 0))
    (✓ (lf-assert (cl-incf counter) (integerp it)))
    (≋ counter 1)))

;; :fire: It lets you use pre- and post-conditions with custom error messages, by using or-error

(deftest "It lets you use pre- and post-conditions with custom error messages, by using or-error"
  [lf-define]
  (lf-undefine go)
  (lf-define go (x) [:requires (or (integerp x) (error "I asked you for a bloody integer!"))] x)
  (✓ (go 3))
  (↯ (go "3") "I asked you for a bloody integer!"))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; lf-where / lf-where* / lf-where-explain ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; "Is OBJ a map with (at least) these keys, of these types?" ---inspired by
;; Ramda's `R.where': {String: (* -> Boolean)} -> {String: *} -> Boolean.
;; OBJ may be an alist, plist, hash-table, or `cl-defstruct' record.

(cl-defstruct (lf-test-person) name age)

;; --- basic satisfaction, across every map.el-plus-records shape ------------

(deftest "lf-where is satisfied by a symbol-keyed alist with matching types"
  [lf-where]
  (✓ (lf-where '((number . 1) (subject . "hello"))
               :number  #'integerp
               :subject #'stringp)))

(deftest "lf-where is satisfied by a keyword-keyed alist with matching types"
  [lf-where]
  (✓ (lf-where '((:number . 1) (:subject . "hello"))
               :number  #'integerp
               :subject #'stringp)))

(deftest "lf-where is satisfied by a plist with matching types"
  [lf-where]
  (✓ (lf-where '(:number 1 :subject "hello")
               :number  #'integerp
               :subject #'stringp)))

(deftest "lf-where is satisfied by a hash-table with matching types"
  [lf-where]
  (let ((h (make-hash-table)))
    (puthash 'number 1 h)
    (puthash 'subject "hello" h)
    (✓ (lf-where h 'number #'integerp 'subject #'stringp))))

(deftest "lf-where is satisfied by a cl-defstruct record with matching types"
  [lf-where]
  (✓ (lf-where (make-lf-test-person :name "musa" :age 29)
               :name #'stringp
               :age  #'integerp)))

;; --- failure is a plain nil, not an error --> composes inside and/or -------

(deftest "lf-where returns nil, not an error, when a predicate fails"
  [lf-where]
  (≋ (lf-where '((number . "not-a-number") (subject . "hello"))
               :number  #'integerp
               :subject #'stringp)
     nil))

(deftest "lf-where composes as a plain boolean inside `and'/`or'"
  [lf-where]
  (let ((good '((number . 1) (subject . "hello")))
        (bad  '((number . "nope") (subject . "hello"))))
    (✓ (and (lf-where good :number #'integerp) (lf-where good :subject #'stringp)))
    (✓ (not (and (lf-where bad :number #'integerp) (lf-where bad :subject #'stringp))))
    (✓ (or (lf-where bad :number #'integerp) (lf-where bad :subject #'stringp)))))

;; --- key-form normalisation: caller writes :key or 'key, either works ------
;;
;; Real alist data (easy-access/Gerrit-JSON style) commonly uses bare-symbol
;; keys, not keyword keys; without normalisation a caller writing `:number'
;; against a `(number . 1)'-keyed alist would silently look up nothing and
;; every check would report nil rather than erroring where the mismatch
;; could be noticed.  `lf-where' inspects OBJ's own key convention (or a
;; struct's slot names, which are never keywords) and coerces the spec's
;; key to match, so the caller does not need to know OBJ's convention ahead
;; of time.

(deftest "lf-where accepts a keyword spec key against a symbol-keyed alist"
  [lf-where]
  (✓ (lf-where '((number . 1)) :number #'integerp)))

(deftest "lf-where accepts a symbol spec key against a keyword-keyed alist"
  [lf-where]
  (✓ (lf-where '((:number . 1)) 'number #'integerp)))

(deftest "lf-where accepts a symbol spec key against a plist (conventionally keyword-keyed)"
  [lf-where]
  (✓ (lf-where '(:number 1) 'number #'integerp)))

(deftest "lf-where accepts a keyword spec key against a cl-defstruct record"
  [lf-where]
  (✓ (lf-where (make-lf-test-person :name "musa") :name #'stringp)))

(deftest "lf-where accepts a symbol spec key against a cl-defstruct record"
  [lf-where]
  (✓ (lf-where (make-lf-test-person :name "musa") 'name #'stringp)))

;; --- "at least these keys": extra keys on OBJ are ignored -------------------

(deftest "lf-where ignores keys on OBJ that are not named in the spec"
  [lf-where]
  (✓ (lf-where '((number . 1) (subject . "hello") (extra . "ignored-entirely"))
               :number #'integerp)))

;; --- absent key and nil-valued key are indistinguishable, like map-elt -----

(deftest "lf-where treats an absent key the same as a nil-valued key"
  [lf-where]
  (let ((permissive (lambda (v) (or (null v) (stringp v))))
        (strict     (lambda (v) (stringp v))))
    ;; :missing is not a key in this alist at all.
    (✓ (funcall permissive nil))
    (✓ (lf-where '((subject . "hello")) :missing permissive))
    (≋ (lf-where '((subject . "hello")) :missing strict) nil)
    ;; :subject IS a key here, just nil-valued --- same predicate outcome.
    (✓ (lf-where '((subject . nil)) :subject permissive))
    (≋ (lf-where '((subject . nil)) :subject strict) nil)))

;; --- lf-where*: boolean EXPRESSIONS with `it' bound, not predicate funcs ---

(deftest "lf-where* binds `it' to each key's looked-up value"
  [lf-where]
  (✓ (lf-where* '((number . 5) (subject . "hello"))
                :number  (and (integerp it) (> it 0))
                :subject (or (null it) (stringp it))))
  (≋ (lf-where* '((number . -5)) :number (and (integerp it) (> it 0))) nil))

(deftest "lf-where* is equivalent to lf-where with an explicit lambda"
  [lf-where]
  (let ((obj '((lastUpdated . nil))))
    (≋ (lf-where* obj :lastUpdated (or (null it) (numberp it)))
       (lf-where  obj :lastUpdated (lambda (it) (or (null it) (numberp it)))))))

;; --- lf-where-explain: signals a descriptive error naming the failing key --

(deftest "lf-where-explain returns t when every key is satisfied"
  [lf-where]
  (≋ (lf-where-explain '((number . 1) (subject . "hello"))
                       :number  #'integerp
                       :subject #'stringp)
     t))

(deftest "lf-where-explain signals an error naming the failing key, predicate, value, and type"
  [lf-where]
  (↯ (lf-where-explain '((number . "not-a-number") (subject . "hello"))
                       :number  #'integerp
                       :subject #'stringp)
     "Error: shape requirement violated at key “:number”.

      EXPECTED: integerp
      GIVEN: :number = \"not-a-number\" (string)"))

(deftest "lf-where-explain reports only the FIRST failing key, not every failing key"
  [lf-where]
  ;; Both :number and :subject are wrong-typed here; only :number --- the
  ;; first pair in the spec --- should appear in the signalled message.
  (↯ (lf-where-explain '((number . "nope") (subject . 42))
                       :number  #'integerp
                       :subject #'stringp)
     "Error: shape requirement violated at key “:number”.

      EXPECTED: integerp
      GIVEN: :number = \"nope\" (string)"))

;; --- end-to-end: a named shape predicate dropped into lf-define's :requires

(deftest "A named shape predicate built from lf-where composes into lf-define's :requires"
  [lf-where]
  (defun gerrit-change-p (c)
    (lf-where c
              :number         #'integerp
              :subject        #'stringp
              :commitMessage  (lambda (v) (or (null v) (stringp v)))))

  (✓ (gerrit-change-p '((number . 1) (subject . "Fix the thing") (commitMessage . nil))))
  (≋ (gerrit-change-p '((number . "not-a-number") (subject . "Fix the thing"))) nil)

  (lf-undefine change-subject)
  (lf-define change-subject (change)
             [:requires (gerrit-change-p change)
              :ensures  (stringp result)]
             "Return CHANGE's subject line."
             (or (cdr (assq 'subject change)) ""))
  (✓ (change-subject '((number . 1) (subject . "Fix the thing"))))
  (↯ (change-subject '((subject . "no number key")))))
