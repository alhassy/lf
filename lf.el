;;; lf.el --- A Language Features library for Emacs Lisp  -*- lexical-binding: t; -*-

;; Copyright (c) 2021 Musa Al-hassy

;; Author: Musa Al-hassy <alhassy@gmail.com>
;; Version: 1.0
;; Package-Requires: ((s "1.12.0") (dash "2.16.0") (emacs "27.1"))
;; Keywords: convenience, programming
;; Repo: https://github.com/alhassy/lf.el
;; Homepage: https://alhassy.github.io/lf.el/

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This library provides common desirable “L”anguage “F”eatures:
;;
;; 0. A unifed interface for defining both variables and functions. LF-DEFINE.
;;
;; 1. A way to define typed, constrained, variables. LF-DEFINE.
;;
;; 2. A way to define type specifed functions. LF-DEFINE.
;;
;; 3. A macro to ease variable updates:  (lf-define very-long-name (f it))
;;                                     ≋ (setq very-long-name (f very-long-name))
;;
;; 4. A more verbose, yet friendlier, alternative to SETF: LF-DEFINE.
;;
;;
;; Minimal Working Example:
;;
;;     (lf-define age 0 [(integerp it) (<= 0 it 100)])
;;
;;     (lf-define age 123) ;; ⇒ Error: Existing constraints for “age” violated!
;;                         ;;  “age” is not updated; it retains old value.
;;
;;     (lf-define age 29)  ;; OK, “age” is now 29.
;;
;;
;; This file has been tangled from a literate, org-mode, file.
;;
;; There are numerous examples in tests.el.

;;; Code:

;; String and list manipulation libraries
;; https://github.com/magnars/dash.el
;; https://github.com/magnars/s.el

(require 's)               ;; “The long lost Emacs string manipulation library”
(require 'dash)            ;; “A modern list library for Emacs”
(require 'cl-lib)          ;; New Common Lisp library; ‘cl-???’ forms.
(require 'macroexp)        ;; `macroexp-const-p', for `lf--dump-pairs'
(require 'map)              ;; `map-elt', for `lf--map-elt'
(require 'backtrace nil t) ;; `backtrace-get-frames'/`backtrace-frames', for `lf--backtrace-frames'

(defconst lf-version (package-get-version)) 
(defun lf-version () 
  "Print the current lf version in the minibuffer."
  (interactive) 
  (message lf-version)) 

;;;###autoload
(define-minor-mode lf-mode
    "A Language Features library for Emacs Lisp"
  nil nil nil)

(defun lf-unindent (str)
  "Strip common leading whitespace from every line of STR.

The indent level is the minimum leading whitespace among non-blank
lines after the first (line 1 abuts the opening quote, so its
indent is not representative).

By way of example:

   (lf-unindent
      \"1. This is the first line, it has zero indentation.
       2. This line indicates the indentation offset for the remaining lines.
       3. As such, this line has no indentation.
          4. But this one is clearly indented (relative to line 2)\")

Returns the string:

   1. This is the first line, it has zero indentation.
   2. This line indicates the indentation offset for the remaining lines.
   3. As such, this line has no indentation.
      4. But this one is clearly indented (relative to line 2)

Notice that the initial indentation (as determined by line 2) has been
stripped uniformly across all lines."
  (let* ((lines (s-lines str))
         (rest-non-blank (--filter (not (s-blank-p it)) (cdr lines)))
         (min-indent
          (if rest-non-blank
              (apply #'min (--map (length (car (s-match "^ *" it)))
                                  rest-non-blank))
            0)))
    (s-join "\n" (cons (car lines)
                       (--map (if (>= (length it) min-indent)
                                  (substring it min-indent)
                                it)
                              (cdr lines))))))

(defun lf-string (str)
"Allows unindented muliline strings and intepolates ${(lisp expressions)}.

Based on JavaScript's Template Strings, this metod permits writing
multi-line strings that ignore indentation (even though the string
seems to be indented), preserves whitespace/newlines, and
allow us to evaluate arbitrary code enclosed in ${expressions}.

This is useful for methods that insert large (template) strings
into buffers, docstrings, or generally large string outputs that
would otherwise look rather ugly (and difficult to maintain) when
presented as a sequence of `concat' calls.

For instance,

   (lf-unindent
      \"1. This is the first line, it has zero indentation.
       2. This line indicates the indentation offset for the remaining lines.
       3. As such, this line has no indentation.
          4. But this one is clearly indentated (relative to line 2)\")

Returns the string:

   1. This is the first line, it has zero indentation.
   2. This line indicates the indentation offset for the remaining lines.
   3. As such, this line has no indentation.
      4. But this one is clearly indentated (relative to line 2)

Notice that the initial indentation (as determined by line 2) has been stripped uniformally
across all lines.

Similarly,

   (let ((you 'Jasim))
     (lf-string \"me and ${you} like the number ${(+ 2 3)}\"))

Returns the string:

   me and Jasim like the number 5

Finally, ${expressions} may contain escaped literal string expressions."
  (let* ((unindented-str (lf-unindent str)))
    (cl-loop for (literal string-expr) in (s-match-strings-all "${\\([^}]+\\)}" unindented-str)
             for value = (eval (read string-expr))
             with result = unindented-str
             do (setq result (replace-regexp-in-string (regexp-quote literal) (format "%s" value) result))
             finally return result)))

(cl-defun lf-documentation (name &optional kind newdoc)
  "Essentially, `lf-documentation' ≈ `documentation' + `documentation-property'.

If the final argument NEWDOC is provided, then this function becomes a setter;
otherwise it is a getter.

By default, Emacs Lisp's `documentation' returns the function documentation of
a given symbol.  As such, ours will be biased towards variable documentation.

\(lf-documentation NAME 'function)  ≈  (documentation NAME).

Interestingly, Common Lisp's `documentation' primitive takes
two arguments, NAME and KIND.  We have based ours on Common Lisp's."

  (setq kind (if (equal 'function kind) 'function-documentation 'variable-documentation))
  (if newdoc
      (put name kind newdoc)
    (documentation-property name kind)))

(cl-defmacro lf-undefine (&body symbols)
  "Ensure SYMBOLS are undefined, as variables and functions.

Zeros out variable's plists and deletes associated variable watchers.
Useful for testing."
  `(mapc (lambda (sym)
           (ignore-errors
             (makunbound sym)
             (setf (symbol-plist sym) nil) ;; Empty-out a var's plist.
             (--map (remove-variable-watcher sym it) (get-variable-watchers sym))
             (fmakunbound sym)))
         (quote ,symbols)))

(defun lf-extract-optionals-from-rest (X thing1-p Y thing2-p rest)
  "Provide a way to support &optional and &rest arguments of a particular shape.

For example, in `defun' one may provide an optional docstring;
likewise in `lf-define' one may provide a docstring but no vector
of constraints, or any other such mixture.  This method ensures the
right variable refers to the right thing. Example usage:

(defmacro lf-define (place value &optional constraints docstring &rest body)
  (cl-destructuring-bind (constraints docstring body)
      (lf-extract-optionals-from-rest constraints #'vectorp
                                      docstring   #'stringp
                                      body)
       ...))

This method return a list of length 3: The first satisfying THING1-P or nil,
the second satisfying THING2-P or nil, and the last being a list.
The method concludes with `cl-assert`ions of these facts.

X and Y are the values of &optional arguments that
are intended to satisfy predicates THING1-P and THING2-P, respectively.
REST is the value of a &rest argument; i.e., a list.

The predicates THING1-P and THING2-P should be disjoint."

  (cl-assert (listp rest))
  (cl-assert (functionp thing1-p))
  (cl-assert (functionp thing2-p))

  (let ((result (cond
                 ;; Scenario: We have nothing
                 ((and (not X) (not Y) (not rest)) (list nil nil nil))

                 ;; Scenario: We have something as the first argument,
                 ;; but it's not a thing1-p nor a thing2-p: It's the start of the rest.
                 ((and (not (funcall thing1-p X)) (not (funcall thing2-p X)))
                  (cond (Y       (list nil nil (cons X (cons Y rest))))
                        ((not Y) (list nil nil (cons X rest)))))

                 ;; Scenario: We have not a thing1-p, but a thing2-p as first argument.
                 ((funcall thing2-p X) (cond (Y (list nil X (cons Y rest)))
                                     (:else (list nil X rest))))

                 ;; Scenario: We have a thing1-p as the first argument...
                 ((funcall thing1-p X) (cond
                                ;; (1) The second argument is a thing2-p...
                                ((funcall thing2-p Y) (list X Y rest))
                                ;; or (2) it's followed by a non-thing2-p; i.e., the start of the rest
                                (:else (list X nil (cons Y rest))))))))

    ;; Assertions: result ≈ (funcall thing1? thing2? list)
    (cl-assert (or (funcall thing1-p (cl-first result)) (null (cl-first result))))
    (cl-assert (or (funcall thing2-p (cl-second result)) (null (cl-second result))))
    (cl-assert (listp (cl-third result)))

    ;; Result value
    result))

;;; Call-chain backtraces for lf-specify's error messages ;;;;;;;;;;;;;;;;;;;;
;;
;; Small, self-contained (no `eldev' dependency --- inspired by
;; `eldev-backtrace-frames'/`eldev-backtrace-to-string' in eldev-util.el,
;; but copied/trimmed/modified in place rather than required) primitives
;; that let `lf-specify' say not just WHICH function's contract was
;; violated, but WHAT CALLED IT --- e.g. "outer -> middle -> inner"
;; instead of just "inner", once real code has more than one layer.

(defun lf--backtrace-frames (marker-function)
  "Capture the Lisp call stack above the innermost call to MARKER-FUNCTION.
Uses whichever of `backtrace-get-frames'/`backtrace-frames' is
`fboundp' on the running Emacs --- both ship with Emacs 27+, `lf.el''s
own floor, so no Emacs-24/25 fallback is needed here.  Frame shape
(old-style list vs. new-style struct) varies by Emacs version; callers
must go through `lf--backtrace-frame-function', not assume either
shape directly."
  (cond ((fboundp 'backtrace-get-frames) (backtrace-get-frames marker-function))
        ((fboundp 'backtrace-frames)     (backtrace-frames marker-function))
        (t nil)))

(defun lf--backtrace-frame-function (frame)
  "Return the function SYMBOL FRAME represents, or nil.
Handles both old-style (list) and new-style (struct) frame shapes,
which differ by Emacs version --- old-style frames are plain lists
whose second element is the function; new-style frames go through
`backtrace-frame-fun'.  A frame's \"function\" is not always a plain
symbol --- an advised function's frame can report a raw closure or
subr object instead (observed live: advising `eval' globally, as
`easy-access-mode' does, can surface such a frame) --- so this always
returns a `symbolp' value or nil, never the raw object, protecting
`lf--backtrace-string''s `symbol-name' call downstream."
  (let ((fn (if (listp frame)
                (nth 1 frame)
              (when (fboundp 'backtrace-frame-fun)
                (backtrace-frame-fun frame)))))
    (when (symbolp fn) fn)))

(defun lf--backtrace-string (marker-function)
  "Render the call chain above MARKER-FUNCTION as \"f1 -> f2 -> f3\".
Deliberately does NOT call `backtrace-to-string' --- on at least one
real Emacs (30.2, where `backtrace-get-frames' is absent and
`backtrace-frames' returns old-style list frames), `backtrace-to-string'
signals `wrong-type-argument' on those frames; walking frames with
`lf--backtrace-frame-function' and joining the distinct function names
sidesteps that version split entirely, and is all a one-line error
trailer needs (no width truncation, no multi-line dump, no face
highlighting --- see `eldev-backtrace-to-string' in eldev-util.el for
that fuller feature set, not reproduced here on purpose).

Consecutive duplicate names and Lisp's own call-dispatch/control-flow
plumbing --- special forms/macros every `cl-defun'/`lf-define'd
function or `lf-assert' call expands through (`apply', `funcall',
`progn', `let'/`let*', `catch', `condition-case', `if', `and', `or',
`when', `unless'), the `cl-lib' macro-expansion artifacts
(`cl-block'/`cl--block-wrapper'), and `lf.el''s own internals that
build the trailer itself (`concat', `message', `error',
`lf--backtrace-string') --- are dropped so the chain reads as the
user's own call chain, not `lf.el' leaking its own implementation.
Returns nil --- not an empty string --- when the chain is empty (e.g.
MARKER-FUNCTION was called directly, with nothing above it worth
reporting), so callers can skip the trailer entirely rather than
print a content-free \"CALLED FROM: \" line."
  (let* ((noise  '(apply funcall progn let let* catch condition-case
                   cl-block cl--block-wrapper and or when unless
                   concat message lf--backtrace-string lf--backtrace-trailer
                   error if))
         (frames (lf--backtrace-frames marker-function))
         (names  (cl-loop for frame in frames
                          for fn = (lf--backtrace-frame-function frame)
                          when (and fn (not (memq fn noise)))
                          collect fn))
         (deduped nil))
    (dolist (fn names)
      (unless (eq fn (car deduped))
        (push fn deduped)))
    (setq deduped (nreverse deduped))
    (when deduped
      (mapconcat #'symbol-name deduped " -> "))))

(defun lf--backtrace-trailer (marker-function)
  "The \"\\n\\nCALLED FROM: f1 -> f2 -> ...\" trailer for MARKER-FUNCTION,
or \"\" when `lf--backtrace-string' finds nothing worth reporting ---
shared by `lf-assert' and `lf-specify', both of which append this
exact trailer to their error/dump text."
  (if-let ((chain (lf--backtrace-string marker-function)))
      (format "\n\nCALLED FROM: %s" chain)
    ""))

;;; lf-dump: print forms alongside their values ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; `lf-specify''s :requires failure message has always built a
;; "NAME = VALUE" list for each argument --- the same job eldev's
;; `eldev-dump' does for ad-hoc debugging prints
;; (`eldev-dump'/`eldev-util.el').  Pulled out here into a small,
;; standalone, self-contained (no `eldev' dependency) family any
;; `lf.el' user can call directly, with `lf-specify' itself refactored
;; to reuse the same formatter rather than keep its own private copy.

(defconst lf--dump-show-type '(:lf-dump-show-type)
  "Sentinel CONDITION value for `lf--dump-pairs': \"show this entry's
type, but there is no real condition to report alongside it\" --- a
third state distinct from both t (terse, no type: `lf-dump''s native
rendering) and a real condition form (`lf-assert''s `satisfies ...'
rendering).  `lf-specify' passes this for its GIVEN section: every
violating argument's TYPE is exactly the diagnostic detail worth
keeping, even though there is no single per-argument `satisfies'
clause to name (`:requires' is one combined form over all of them,
not one condition per argument).  A private, uninterned-flavoured
list (not a keyword) specifically so it can never collide with a
real, user-written condition form.")

(defun lf--dump-pairs (entries)
  "ENTRIES is a list of (FORM VALUE . CONDITION) --- FORM already
evaluated to VALUE.  CONDITION selects the rendering for that entry:

  - t (no real constraint --- `lf-dump''s own entries, and any of
    `lf-assert''s entries that happened to pass a trivial `t' check):
    a self-evaluating-literal FORM (see `macroexp-const-p') prints
    bare; anything else prints \"FORM = VALUE\".  Terse, no type noise.

  - `lf--dump-show-type' (`lf-specify''s GIVEN entries): `pp-to-string'd
    as `(FORM = VALUE : TYPE)' --- the value's type is worth showing,
    but there is no single per-argument condition to name.

  - any other (real) form --- `lf-assert''s actual constraints:
    `pp-to-string'd as `(FORM satisfies CONDITION = VALUE : TYPE)',
    since a real assertion's failure is exactly the case where the
    value's type is worth seeing alongside what it was checked
    against."
  (if (--all-p (equal (nth 2 it) lf--dump-show-type) entries)
      ;; `lf-specify''s GIVEN section: one pretty-printed OUTER list of
      ;; `(FORM = VALUE : TYPE)' entries --- `lf-specify''s original,
      ;; pre-extraction format, preserved exactly.
      (pp-to-string
       (mapcar (lambda (entry) (list (nth 0 entry) '= (nth 1 entry) ': (type-of (nth 1 entry))))
               entries))
    (mapconcat
     (lambda (entry)
       (let ((form (nth 0 entry)) (value (nth 1 entry)) (cond (nth 2 entry)))
         (cond
          ((equal cond lf--dump-show-type)
           (pp-to-string (list form '= value ': (type-of value))))
          ((not (eq cond t))
           (pp-to-string (list form 'satisfies cond '= value ': (type-of value))))
          ((macroexp-const-p form) (format "%S" form))
          (t (format "%S = %S" form value)))))
     entries "\n")))

(defvar lf--dump-on-success nil
  "When non-nil, `lf-assert' prints its dump even when every condition
holds, not just on failure.  `lf-dump'/`lf-dump-with-backtrace' let-bind
this to t around a call to `lf-assert' with all-`t' conditions --- an
assertion that can never fail is, precisely, a dump.")

(defmacro lf-assert (&rest expr-cond-pairs)
  "Assert a property of each of several expressions, in one call.

EXPR-COND-PAIRS is a flat list of EXPR CONDITION pairs.  Each EXPR is
evaluated exactly once; CONDITION is then evaluated with `it' bound
to EXPR's value --- when CONDITION is nil, EXPR's assertion has
failed.  A bare `t' CONDITION is a plain dump entry (always
\"succeeds\", nothing to check) rather than a real constraint --- see
`lf-dump', below, which is defined in terms of exactly this.

If every pair's CONDITION holds, `lf-assert' returns t.  It also
prints its dump in that case when `lf--dump-on-success' is non-nil
\(nil by default --- a passing assertion is silent unless you ask
otherwise).  If any pair fails, signals a single error listing EVERY
pair (not just the first failing one) in the same \"EXPR = VALUE :
TYPE\" shape `lf-specify''s :requires violations use (see
`lf--dump-pairs'), with each failing pair naming the condition it
failed, so a multi-expression assertion shows its full context at a
glance rather than stopping at the first problem.

Example:

    (lf-assert (+ 1 2)      (cl-evenp it)
               \"Hello\"     (and (stringp it) (> (length it) 3)))
    ;; => error, because (+ 1 2) = 3 is not even; the string clause
    ;;    passes but is still shown for context.

This is the ad-hoc, no-function-definition-required cousin of
`lf-specify' --- reach for `lf-specify'/`lf-define' when a *function's*
contract deserves a name and lasting enforcement on every call; reach
for `lf-assert' for a one-off check inline in some other function's
body."
  (let* ((pairs (cl-loop for (expr cond) on expr-cond-pairs by #'cddr
                        collect (list expr cond (make-symbol "v"))))
         (bindings (mapcar (lambda (p) (list (nth 2 p) (nth 0 p))) pairs))
         (entries  (mapcar (lambda (p) `(list (quote ,(nth 0 p)) ,(nth 2 p) (quote ,(nth 1 p))))
                           pairs)))
    `(let ,bindings
       (if (and ,@(mapcar (lambda (p) `(let ((it ,(nth 2 p))) ,(nth 1 p))) pairs))
           (progn (when lf--dump-on-success
                    (message "%s" (concat (lf--dump-pairs (list ,@entries))
                                          (lf--backtrace-trailer 'lf--backtrace-trailer))))
                  t)
         (error "%s" (concat "Error: lf-assert failed.\n\n"
                             (lf--dump-pairs (list ,@entries))
                             (lf--backtrace-trailer 'lf--backtrace-trailer)))))))

(defmacro lf-dump (&rest forms)
  "Print each FORM in FORMS alongside its value via `message'.
Each FORM is evaluated exactly once.  Self-evaluating literals print
bare, without a value --- see `macroexp-const-p'.  Prints nothing (and
evaluates nothing) when FORMS is empty.

Defined as `lf-assert' over FORMS with a trivial `t' condition on
each --- an assertion that can never fail, always shown (via
`lf--dump-on-success'), is precisely a dump."
  (when forms
    `(let ((lf--dump-on-success t))
       (lf-assert ,@(mapcan (lambda (form) (list form t)) forms)))))

(defalias 'lf-dump-with-backtrace 'lf-dump
  "Alias for `lf-dump' --- `lf-assert' already includes the \"CALLED
FROM:\" backtrace trailer unconditionally, so there is no behavioral
difference; kept only for call-site clarity/discoverability.")

(defmacro lf-dump* (&rest forms)
  "Like `lf-dump', then drop into the debugger.
\"Stop here and show me the values of these expressions, and let me
debug myself\" --- prints the dump first (so the values are visible in
\\=*Messages\\=* even if a later step in the debugger re-runs code
that would shadow them), then calls the built-in `debug' to enter a
recursive edit at this exact point.  A one-shot breakpoint, not
`edebug'-style step-through instrumentation."
  `(progn (lf-dump ,@forms) (debug)))

(defmacro lf-dump-if (cond &rest forms)
  "Like `lf-dump', but only when COND is non-nil.
FORMS are not evaluated at all when COND is nil --- a true no-op, not
merely a silent one."
  (declare (indent 1))
  `(when ,cond (lf-dump ,@forms)))

(defmacro lf-dump*if (cond &rest forms)
  "Like `lf-dump*', but only when COND is non-nil.
FORMS are not evaluated at all --- and the debugger is not entered ---
when COND is nil."
  (declare (indent 1))
  `(when ,cond (lf-dump* ,@forms)))

(cl-defmacro lf-specify (name args &key (requires t) (ensures t))
  "Advice function NAME to perform runtime validation vis REQUIRES and ENSURES.

Advice the function NAME, consuming ARGS, to have pre-condition REQUIRES and post-condition ENSURES.

Both REQUIRES and ENSURES are forms that may reference any of the names within ARGS
or may mention the special name ‘result’, which refers to the result of calling NAME on ARGS.

The return value is the name of the specification advice
function, which is itself documented using the user's REQUIRES
and ENSURES clauses.

One possible use is to enable runtime validation for prexisting functions,
such as the built-in `identity' function:

     (lf-specify identity (x) :ensures (equal x result))

For your own personally defined function, there is a slicker form using `lf-define':

     (progn (lf-specify name args constraints) (defun name args body))
   ≈ (lf-define name args [constraints] body)

Warning: Using this with core functions, such as `car', will result in
Lisp nesting exceeds ‘max-lisp-eval-depth’ errors."
  (let* ((advice-name  (intern (format "lf--specify/%s" name)))
         (docstring (format "SPECIFIES: %s %s\n REQUIRES:\n%s\n\nENSURES:\n%s" name args requires ensures))
         ;; ARGS may itself contain &optional/&rest (e.g. (jql &optional
         ;; fields max)), so we can't bind it directly as a plain &rest
         ;; list --- `cl-destructuring-bind' understands those markers and
         ;; binds each named parameter to its real call-time value, which
         ;; plain `&rest ,args' never did (it bound the whole arg list to
         ;; whatever symbol happened to be first in ARGS, leaving every
         ;; other name in ARGS unbound --- `void-variable' at call time).
         (plain-names (--filter (not (memq it '(&optional &rest &key))) args)))
    `(progn
       (cl-defun ,advice-name (orig-fun &rest lf--specify-raw-args)
         ,docstring
         (cl-destructuring-bind ,args lf--specify-raw-args
           (unless ,requires
             (error "%s"
                    (concat
                     (format "Error: Requirements for “%s” have been violated.\n\nREQUIRED:\n%s\nGIVEN:\n%s"
                             (quote ,name)
                             (pp-to-string (quote ,requires))
                             (lf--dump-pairs (list ,@(--map `(list (quote ,it) ,it lf--dump-show-type) plain-names))))
                     (lf--backtrace-trailer (quote ,advice-name)))))

           (let ((result (apply orig-fun lf--specify-raw-args)))
             (unless ,ensures
               (error "%s"
                      (concat
                       (format (concat "Panic! There is an error in the implementation of “%s”."
                                       "\n\nClaimed guarantee: %s\nActual result value: %s ---typed: %s")
                               (quote ,name)
                               (pp-to-string (quote ,ensures))
                               (pp-to-string result)
                               (type-of result))
                       (lf--backtrace-trailer (quote ,advice-name)))))

             result)))

       (advice-add (function ,name) :around (quote ,advice-name))

       ;; Return value is the name of the specification
       (quote ,advice-name))))

;;; Shape checking: is OBJ a map with (at least) these keys, of these types? ;

(defun lf--bare-symbol (key)
  "KEY with any leading keyword colon stripped; non-keywords pass through.
`:number' -> `number'; `number' -> `number'."
  (if (keywordp key)
      (intern (substring (symbol-name key) 1))
    key))

(defun lf--keyword-of (key)
  "KEY coerced to a keyword; already-keywords pass through.
`number' -> `:number'; `:number' -> `:number'."
  (if (keywordp key)
      key
    (intern (concat ":" (symbol-name key)))))

(defun lf--map-elt (obj key)
  "Like `map-elt', but KEY's keyword/symbol form is normalised to match
OBJ's own convention before lookup, and `cl-defstruct' records are
supported (`map-elt' itself has neither).

Writing spec keys as keywords (`:number') is the natural `&key'-style
default, but real alist data very often uses bare symbols instead
(easy-access-style `(number . 1)', not `(:number . 1)'), and a bare
symbol never matches a keyword under `eq'/`equal' --- so a keyword key
against a symbol-keyed alist would otherwise silently look up nothing
and always report nil, rather than erroring where the mismatch could
be noticed.  Mirrors `easy-access''s own struct-key normalisation
(`easy-access--key-to-slot-name', which already strips the colon for
struct slots since slot names are never keywords) generalised to
alists too: inspect OBJ's first pair's actual key shape and coerce
KEY to match it, rather than requiring the caller to guess which
convention this particular OBJ happens to use.

  - Record: KEY is always coerced to a bare symbol (`lf--bare-symbol')
    --- `cl-defstruct' slot names are never keywords.
  - Alist whose first key is a keyword: KEY is coerced to a keyword.
  - Alist whose first key is a bare symbol: KEY is coerced to bare.
  - Plist: keys are conventionally keywords; KEY is coerced to a
    keyword the same way.
  - Hash-table: no ambient convention to detect from an empty table,
    so KEY is used as-is (whatever the caller wrote)."
  (cond
   ((recordp obj)
    (ignore-errors (cl-struct-slot-value (type-of obj) (lf--bare-symbol key) obj)))
   ((and (consp obj) (consp (car obj)))       ; alist: ((k . v) ...)
    (let ((first-key (caar obj)))
      (map-elt obj (if (keywordp first-key) (lf--keyword-of key) (lf--bare-symbol key)))))
   ((consp obj)                                ; plist: (k v k v ...)
    (map-elt obj (lf--keyword-of key)))
   (t (map-elt obj key))))

(cl-defun lf-where (obj &rest kvs)
  "Non-nil iff OBJ satisfies every :KEY PRED pair in KVS.

Each PRED is a 1-arg predicate function, applied to OBJ's value at
KEY.  OBJ may be an alist, plist, hash-table, or `cl-defstruct'
record --- anything `map-elt' understands, plus records (via
`lf--map-elt', since `map-elt' itself has no record support).  Write
KEY as a keyword (`:number') or a bare symbol (`number') as you
prefer --- `lf--map-elt' inspects OBJ's own key convention (or a
struct's slot names, which are never keywords) and coerces KEY to
match, so callers do not need to know ahead of time whether a given
alist happens to use symbol or keyword keys.  Extra keys on OBJ
beyond those named in KVS are ignored, so this checks \"has at least
these keys, of these types\", not an exact shape match.  A key absent
from OBJ and a key present with a nil value are indistinguishable
(both feed nil to PRED), matching `map-elt''s own semantics; write a
predicate that rejects nil when a key must genuinely be present, or
an `(or null ...)'-shaped predicate to tolerate either.

Inspired by Ramda's `R.where': {String: (* -> Boolean)} -> {String: *}
-> Boolean.

Example:

    (lf-where change
              :number  #'integerp
              :subject #'stringp)

See also `lf-where*' (boolean EXPRESSIONS with `it' bound, instead of
predicate functions) and `lf-where-explain' (same spec shape, signals
a descriptive error naming the first failing key instead of
returning nil)."
  (cl-loop for (key pred) on kvs by #'cddr
           always (funcall pred (lf--map-elt obj key))))

(cl-defmacro lf-where* (obj &rest kvs)
  "Like `lf-where', but each value in KVS is a boolean EXPRESSION with
`it' bound to that key's looked-up value, instead of a predicate
function --- lets a compound per-key check be written inline instead
of as a `(lambda (v) ...)'.

Example:

    (lf-where* change
               :lastUpdated (or (null it) (numberp it))
               :commitMessage (or (null it) (stringp it)))"
  (declare (indent 1))
  `(lf-where ,obj
             ,@(cl-loop for (key expr) on kvs by #'cddr
                        append (list key `(lambda (it) ,expr)))))

(cl-defun lf-where-explain (obj &rest kvs)
  "Like `lf-where', but on the FIRST failing :KEY PRED pair, signal a
descriptive error naming the key, its predicate, and the actual
value and type, instead of returning nil.  Returns t on success.

Drop this directly into a `:requires' clause (in place of `lf-where')
when a shape-check failure should explain itself, rather than falling
through to `lf-specify''s own generic REQUIRED/GIVEN message."
  (cl-loop for (key pred) on kvs by #'cddr
           for value = (lf--map-elt obj key)
           unless (funcall pred value)
           do (error "Error: shape requirement violated at key “%s”.\n\nEXPECTED: %s\nGIVEN: %s = %s (%s)"
                      key pred key (prin1-to-string value) (type-of value)))
  t)

;;; Type declarations for special forms ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar lf--declared-type-env nil
  "Alist of (SYMBOL . EXPANDER-FN) entries used by `lf-declare-type'.

Each EXPANDER-FN receives the unevaluated arguments of a call to
SYMBOL and must either signal an error or return a replacement
form.  Registered entries shadow SYMBOL inside every subsequent
`eval' call (via the `eval' advice installed by `lf-declare-type').")

(defvar lf--type-check-active nil 
  "Reentrancy guard for `lf--type-check-eval-advice'.")

(defconst lf--type-variables
  '(alpha beta gamma delta epsilon zeta eta theta iota kappa
    lambda mu nu xi omicron pi rho sigma tau upsilon phi chi psi omega)
  "Symbols treated as unconstrained type variables in `lf-declare-type' signatures.")

(defun lf--type-check-eval-advice (orig-fn form &optional lexical)
  "Preprocess FORM through `lf--declared-type-env' before passing to ORIG-FN.

Skips reprocessing when already inside a `macroexpand-all' triggered
by ourselves (reentrancy guard), so we never recurse infinitely."
  (if lf--type-check-active
      (funcall orig-fn form lexical)
    (let* ((lf--type-check-active t)
           (form (macroexpand-all form lf--declared-type-env)))
      (funcall orig-fn form lexical))))

(defun lf--type-variable-p (sym)
  "Non-nil if SYM is a type variable: single-character name or a Greek letter."
  (or (= 1 (length (symbol-name sym)))
      (memq sym lf--type-variables)))

(defun lf--type->predicate (typ)
  "TYP -> predicate symbol, or nil for type variables.

Concrete types map via the NAME → NAMEp convention (e.g. `symbol'
→ `symbolp').  Single-character or Greek-letter names are type
variables and map to nil (unconstrained)."
  (if (lf--type-variable-p typ)
      nil
    (let ((pred (intern (concat (symbol-name typ) "p"))))
      (if (fboundp pred) pred
        (error "lf-declare-type: no predicate for type `%s' (tried `%s')" typ pred)))))

(defun lf--parse-type-spec (spec)
  "SPEC -> list of input predicates (nil for type variables).

SPEC is a list of the form (:: T1 T2 ... -> R) or (T1 T2 ... -> R).
Returns one predicate (or nil) per input type, in order."
  (let* ((spec (if (eq ':: (car spec)) (cdr spec) spec))
         (arrow-pos (cl-position '-> spec)))
    (unless arrow-pos
      (error "lf-declare-type: missing `->` in type spec %s" spec))
    (mapcar #'lf--type->predicate (cl-subseq spec 0 arrow-pos))))

(defun lf--make-type-checking-expander (name predicates)
  "NAME × PREDICATES -> expander function.

The expander expects call arguments in repeating chunks of
(length PREDICATES).  Each chunk position with a non-nil predicate
is checked; type variables (nil) are unconstrained.  On success the
original (NAME . ARGS) form is returned unchanged; on failure a
friendly error is signalled."
  (let ((arity (length predicates)))
    (lambda (&rest args)
      (unless (zerop (mod (length args) arity))
        (error "(%s %s) ---expected arg count divisible by %d; got %d."
               name (mapconcat #'prin1-to-string args " ") arity (length args)))
      (cl-loop for chunk-args on args by (lambda (l) (nthcdr arity l))
               for chunk = (cl-subseq chunk-args 0 arity)
               do (cl-loop for arg  in chunk
                            for pred in predicates
                            when (and pred (not (funcall pred arg)))
                            do (error "(%s ... %s ...) ---expected `%s', got %s (%s)."
                                      name (prin1-to-string arg)
                                      pred (prin1-to-string arg) (type-of arg))))
      ;; Return as (apply 'NAME (list ...)) so macroexpand-all doesn't
      ;; re-enter this expander and loop forever.
      `(apply (quote ,name) (list ,@args)))))

(cl-defmacro lf-declare-type (name &rest type-spec)
  "Declare a call-site type-check for NAME from a type signature.

Usage:

    (lf-declare-type setq :: symbol alpha -> alpha)

The signature (:: T1 T2 ... -> R) is parsed mechanically:

  - Concrete type names (`symbol', `integer', `string', …) resolve
    to their standard NAMEp predicates and are enforced at every
    call site.
  - Single-character names (a, b, …) and Greek-letter names
    (`alpha', `beta', …) are type variables ---unconstrained.
  - The return type R is recorded for documentation only.
  - Arguments are expected in repeating chunks of the input arity,
    allowing variadic forms like `setq' (which takes N pairs).

Concretely, this installs an expander into `lf--declared-type-env'
so that every `eval' call rewrites NAME forms through
`macroexpand-all' before the built-in handler runs.

Examples:

    (lf-declare-type setq :: symbol alpha -> alpha)
    ;; Catches both wrong arity and non-symbol LHS:
    ;;   (setq bad 22 33)  ⇒ error: odd arg count
    ;;   (setq 99 \"x\")    ⇒ error: expected symbolp

    (lf-declare-type aset :: vector integer alpha -> alpha)
    ;; (aset \"str\" 0 ?x) ⇒ error: expected vectorp, got str (string)"
  (declare (indent defun))
  (let ((predicates (lf--parse-type-spec type-spec)))
    `(progn
       (unless (advice-member-p #'lf--type-check-eval-advice 'eval)
         (advice-add 'eval :around #'lf--type-check-eval-advice))
       (setf (alist-get (quote ,name) lf--declared-type-env)
             (lf--make-type-checking-expander (quote ,name) (quote ,predicates)))
       (quote ,name))))

(defmacro lf-define (place newvalue &optional constraints docstring &rest more)
"Essentially: `lf-define'  ≈  `setq' + `defvar' + `defun' + `setf'.

This is a unified variable/function definition interface, that
allows optional type CONSTRAINTS (in the shape of a vector)
followed by optional DOCSTRING.

It defines PLACE to be NEWVALUE, which satisfies CONSTRAINTS, as follows:

1. It can be used to define both variables and functions.

    (lf-define age 29 \"How old am I?\")

    (lf-define greet (name)
      \"Say hello to NAME\"
      (message-box \"Hello, %s!\" name))

    The documentation string for variables is optional, as with functions.
    The presence of MORE indicates that we are defining a function, with
    MORE serving as the function body.

2. It can be used to define *constrained* variables.  The
   following uses are equivalent; the first uses *type
   specifiers*, whereas the last uses an arbitrary predicate with
   the name ‘it’ referring to the name being defined.  By default,
   constraints are collected conjunctively.

      (lf-define age 0 [:type (integer 0 100)])
      ≈
      (lf-define age 0 [:type integer
                       (satisfies (lambda (value) (<= 0 value 100)))])
      ≈
      (lf-define age 0 [:type (and integer
                       (satisfies (lambda (value) (<= 0 value 100))))])
      ≈
      (lf-define age 0 [(integerp it) (<= 0 it 100)])

   Initial/new values not satifying the requested constraints
   result in an error.

      ;; Continuing with the above setup
      (lf-define age 123) ;; ⇒ Error: Existing constraints for “age” violated!

   If a constraint is declared and the initial value does not satisfy it, then
   the name being defined is made unbound, not defined at all.

      (lf-define age 0 [:type nil]) ;; Error; ‘age’ now unbound.

   If no constraint is declared, then the ‘lf-define’ is
   considered to be an update and so the most recent constraint
   is used to check the validity of the new value.
   Constraints are also checked whenever the variable is set with ‘setq’.

3. “Zap”: For non-function definitions, the expressions NEWVALUE may use the
   symbol IT, ‘it’, to refer to the variable name PLACE.

   As such:    (lf-define very-long-name (f it))
             ≈
               (setf very-long-name (f very-long-name))

   This also works when VERY-LONG-NAME is an arbitrary setffable place.

4. It can be used to define *constrained* functions.

    (lf-define speak (name age)
      [ :requires (stringp name) (integerp age)
        :ensures (stringp result)
      ]
      \"Produce an Arabic-English greeting.\"
      (format \"Marhaba! Hello %s-year old %s\" age name))

     (speak \"Yusuf\" 2) ;; ⇒ Marhaba! Hello 2-year old Yusuf
     (speak 'Yusuf  2)   ;; ⇒ Error: Constraints for “speak” have been violated!

   The use of ‘:requires’ is to explicitly provide machine-checked documentation
   of the expected inputs. Conversely, ‘:ensures’ communicates quickly to users
   the expected kind of output and it is machine-checked: Any future alterations
   to the function's implementation are checked to ensure the constraints are
   true.

5. If PLACE is a non-atomic form, then we default to using ‘setf’.

    (lf-define foods '(apple banana))
    (lf-define (car foods) 'pineapple)
    (cl-assert (equal foods '(pineapple banana)))"
  (declare (indent defun))
  (cl-destructuring-bind (constraints docstring more)
      (lf-extract-optionals-from-rest constraints #'vectorp docstring #'stringp more)

    (setq constraints (seq--into-list constraints))
    (cl-assert (or (listp constraints) (null constraints)))
    (cl-assert (or (stringp docstring) (null docstring)))
    (cl-assert (listp more))

    (cond
     ;; (lf-define variable value [constraints] [documentation])
     ((and (atom place) (or (not more) (equal '(nil) more)))
      (lf-define-variable place newvalue constraints docstring))

     ;; (lf-define f (x) body)
     ((and (listp newvalue) more)
      (lf-define-function place newvalue constraints docstring more))

     ;; All else
     (t `(setf ,place  (let ((it ,place)) ,newvalue))))))

(defun lf-define-variable (name value constraints docstring)
  "Set variable NAME with VALUE satisfying CONSTRAINTS, with DOCSTRING.

The return value is (a piece of code that returns) the new value.
If there is no new constraints, then we perform an update; else
register the new constraints then perform the update.

NAME is an atomic symbol, VALUE is an arbitrary expression,
CONSTRAINTS is either a vector that begins with the keyword ‘:type’
followed by a sequence of type specifiers OR it is just a vector
of expressions ---namely, a Boolean valued expression that mentions
the name ‘it’, which refers to any new / initial values for NAME.

We assign the constraints under the symbols plist; this way it's
namespaced, and, more importantly, future lf-define's to the same
variable do not introduce new anyonmous watchers, but instead
redefine the one, and only, constraint LF maintains.
See: (get name :lf-constraints-func)"

  (when constraints
    (message "“%s” has new constraints registered: %s" name `[,@constraints])

    (let (contract) ;; ≈ Lisp checkable version of user's “constraints”, conjunctive.
      ;; Type specifier or not?
      (if (equal :type (car constraints))
          (setq contract `(cl-typep it (quote (and ,@(cdr constraints)))))
        (setq contract (cons 'and constraints)))

    ;; Only continue if the value satisfies the constraints.  if initial
    ;; value does not satify constrains, leave name unbound.
    (unless (eval (cl-subst value 'it `,contract))
      (eval `(lf-undefine ,name))
      (error (concat "Error: Initial value “%s” violates declared constraint:"
                     "\n\t%s\n\nAs such, symbol “%s” is now unbound and unconstrained.")
                 value `[,@constraints] name))

    ;; ADD-VARIABLE-WATCHER is idempotent; so no need to use REMOVE-VARIABLE-WATCHER.
    (setf (get name :lf-constraints-func)
          `(lambda (it.symbol it let-or-set _)
             "If we are doing a SET and the CONSTRAINTS fail, then error; else do nothing."
             (let ((it.old (and (boundp it.symbol) (symbol-value it.symbol))))
               (and (equal let-or-set 'set)
                    (not ,contract)
                    (error (concat "Error: Constraints for “%s” "
                                   "have been violated.\nNewvalue "
                                   "“%s” (%s) does not satisfy: %s\n\nAs such, “%s” retains its old value “%s”.")
                           it.symbol it (type-of it) [,@constraints] it.symbol it.old)))))
    (add-variable-watcher name (get name :lf-constraints-func))))

  `(progn
     (setf (documentation-property (quote ,name) 'variable-documentation)
           (or ,docstring (documentation-property (quote ,name) 'variable-documentation)))
     (setf ,name ,(cl-subst name 'it value))))

(defun lf-define-function (name args constraints docstring body)
  "Define function NAME with type specification CONSTRAINTS.

The return value is (a piece of code that returns) the name of
the newly defiend function, as is the case with `defun'.

NAME is a symbol, ARGS is a list of symbols, DOCSTRING is a
string, BODY is an arbitrary form, and CONSTRAINTS is a vector
consisting of key-value pairs: The key ‘:requires’ and the key
‘:ensures’ are both Boolean valued expressions that may refer to
the symbols mentioned in ARGS.

Since the CONSTRAINTS can refer to arbitrary expressions
involving all of the variables, we have more than just simple
typing.  We have a form of specification."
  (let ((requires     (cl-getf constraints :requires t))
        (ensures      (cl-getf constraints :ensures  t))
        (advice-name  (intern (format "lf--typing-advice/%s" name))))
    `(progn
       (cl-defun ,name ,args ,docstring ,@body)

       (lf-specify ,name ,args :requires ,requires :ensures ,ensures)

       ;; Return value is the name of the newly defined function, as is the case with DEFUN
       (quote ,name))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'lf)

;;; lf.el ends here
