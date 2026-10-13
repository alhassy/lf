;;; lf-where-tests.el --- Tests for lf-where, lf-where*, lf-where-explain: shape checks on maps and records  -*- lexical-binding: t; -*-

(unless (fboundp 'define-lf-call-test)
  (load (expand-file-name "lf-test-utils.el" (file-name-directory (or load-file-name buffer-file-name))) nil t))

;; "Is OBJ a map with (at least) these keys, of these types?" ---inspired by Ramda's `R.where':
;; {String: (* -> Boolean)} -> {String: *} -> Boolean.  OBJ may be an alist, plist, hash-table, or record.

(cl-defstruct (lf-test-person) name age)

;;; Every map.el shape, plus records

(define-lf-call-test "lf-where is satisfied by a symbol-keyed alist with matching types"
  :call (lf-where '((number . 1) (subject . "hello")) :number #'integerp :subject #'stringp)
  :returns t)

(define-lf-call-test "lf-where is satisfied by a keyword-keyed alist with matching types"
  :call (lf-where '((:number . 1) (:subject . "hello")) :number #'integerp :subject #'stringp)
  :returns t)

(define-lf-call-test "lf-where is satisfied by a plist with matching types"
  :call (lf-where '(:number 1 :subject "hello") :number #'integerp :subject #'stringp)
  :returns t)

(define-lf-call-test "lf-where is satisfied by a hash-table with matching types"
  :call (let ((h (make-hash-table)))
          (puthash 'number 1 h)
          (puthash 'subject "hello" h)
          (lf-where h 'number #'integerp 'subject #'stringp))
  :returns t)

(define-lf-call-test "lf-where is satisfied by a cl-defstruct record with matching types"
  :call (lf-where (make-lf-test-person :name "musa" :age 29) :name #'stringp :age #'integerp)
  :returns t)

;;; Failure is a plain nil, not an error; so it composes inside and/or

(define-lf-call-test "lf-where returns nil, not an error, when a predicate fails"
  :call (lf-where '((number . "not-a-number") (subject . "hello")) :number #'integerp :subject #'stringp)
  :returns nil)

(deftest "lf-where composes as a plain boolean inside and/or"
  (let ((good '((number . 1) (subject . "hello")))
        (bad  '((number . "nope") (subject . "hello"))))
    (should (and (lf-where good :number #'integerp) (lf-where good :subject #'stringp)))
    (should-not (and (lf-where bad :number #'integerp) (lf-where bad :subject #'stringp)))
    (should (or (lf-where bad :number #'integerp) (lf-where bad :subject #'stringp)))))

;;; Spec keys may be keywords or symbols, whatever OBJ's own convention
;;
;; Real alists often use bare-symbol keys; a keyword spec key against them would otherwise silently look up
;; nothing.  `lf-where' inspects OBJ's convention ---or a struct's slot names, never keywords--- and coerces.

(define-lf-call-test "lf-where accepts a keyword spec key against a symbol-keyed alist"
  :call (lf-where '((number . 1)) :number #'integerp)
  :returns t)

(define-lf-call-test "lf-where accepts a symbol spec key against a keyword-keyed alist"
  :call (lf-where '((:number . 1)) 'number #'integerp)
  :returns t)

(define-lf-call-test "lf-where accepts a symbol spec key against a plist"
  :call (lf-where '(:number 1) 'number #'integerp)
  :returns t)

(define-lf-call-test "lf-where accepts a keyword spec key against a cl-defstruct record"
  :call (lf-where (make-lf-test-person :name "musa") :name #'stringp)
  :returns t)

(define-lf-call-test "lf-where accepts a symbol spec key against a cl-defstruct record"
  :call (lf-where (make-lf-test-person :name "musa") 'name #'stringp)
  :returns t)

;;; At least these keys: extra keys are ignored, and absent keys read as nil

(define-lf-call-test "lf-where ignores keys on OBJ that are not named in the spec"
  :call (lf-where '((number . 1) (subject . "hello") (extra . "ignored-entirely")) :number #'integerp)
  :returns t)

(define-lf-call-test "lf-where gives an absent key's predicate nil, so a permissive predicate holds"
  :call (lf-where '((subject . "hello")) :missing (lambda (v) (or (null v) (stringp v))))
  :returns t)

(define-lf-call-test "lf-where gives an absent key's predicate nil, so a strict predicate fails"
  :call (lf-where '((subject . "hello")) :missing #'stringp)
  :returns nil)

(define-lf-call-test "lf-where treats a nil-valued key as an absent one, under a permissive predicate"
  :call (lf-where '((subject . nil)) :subject (lambda (v) (or (null v) (stringp v))))
  :returns t)

(define-lf-call-test "lf-where treats a nil-valued key as an absent one, under a strict predicate"
  :call (lf-where '((subject . nil)) :subject #'stringp)
  :returns nil)

;;; lf-where*: boolean expressions with `it' bound, rather than predicate functions

(define-lf-call-test "lf-where* binds it to each key's looked-up value"
  :call (lf-where* '((number . 5) (subject . "hello"))
          :number  (and (integerp it) (> it 0))
          :subject (or (null it) (stringp it)))
  :returns t)

(define-lf-call-test "lf-where* fails when an expression does"
  :call (lf-where* '((number . -5)) :number (and (integerp it) (> it 0)))
  :returns nil)

(deftest "lf-where* is equivalent to lf-where with an explicit lambda"
  (let ((obj '((lastUpdated . nil))))
    (should (equal (lf-where* obj :lastUpdated (or (null it) (numberp it)))
                   (lf-where  obj :lastUpdated (lambda (it) (or (null it) (numberp it))))))))

;;; lf-where-explain: a descriptive error naming the first failing key

(define-lf-call-test "lf-where-explain returns t when every key is satisfied"
  :call (lf-where-explain '((number . 1) (subject . "hello")) :number #'integerp :subject #'stringp)
  :returns t)

(define-lf-error-test "lf-where-explain names the failing key, predicate, value, and type"
  :call (lf-where-explain '((number . "not-a-number") (subject . "hello")) :number #'integerp :subject #'stringp)
  :signals "Error: shape requirement violated at key “:number”.

EXPECTED: integerp
GIVEN: :number = \"not-a-number\" (string)")

(define-lf-error-test "lf-where-explain reports only the first failing key"
  ;; Both :number and :subject are wrong here; only :number ---the first in the spec--- is reported.
  :call (lf-where-explain '((number . "nope") (subject . 42)) :number #'integerp :subject #'stringp)
  :signals "Error: shape requirement violated at key “:number”.

EXPECTED: integerp
GIVEN: :number = \"nope\" (string)")

;;; End to end: a named shape predicate within lf-define's :requires

(deftest "a named shape predicate built from lf-where composes into lf-define's :requires"
  (defun gerrit-change-p (c)
    (lf-where c :number #'integerp :subject #'stringp :commitMessage (lambda (v) (or (null v) (stringp v)))))
  (should (gerrit-change-p '((number . 1) (subject . "Fix the thing") (commitMessage . nil))))
  (should-not (gerrit-change-p '((number . "not-a-number") (subject . "Fix the thing"))))
  (lf-define change-subject (change)
             [:requires (gerrit-change-p change)
              :ensures  (stringp result)]
             "Return CHANGE's subject line."
             (or (cdr (assq 'subject change)) ""))
  (should (change-subject '((number . 1) (subject . "Fix the thing"))))
  (should-error (change-subject '((subject . "no number key")))))

(provide 'lf-where-tests)
;;; lf-where-tests.el ends here
