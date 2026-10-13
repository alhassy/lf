;;; lf-extract-optionals-tests.el --- Tests for lf-extract-optionals-from-rest  -*- lexical-binding: t; -*-

(unless (fboundp 'define-lf-call-test)
  (load (expand-file-name "lf-test-utils.el" (file-name-directory (or load-file-name buffer-file-name))) nil t))

;; (lf-extract-optionals-from-rest X THING1-P Y THING2-P REST) ↦ (X′ Y′ REST′): optional X and Y are kept
;; only when they satisfy their predicates, in order; anything else is shifted onto the rest.

;;; An optional vector, then an optional string

(define-lf-call-test "lf-extract-optionals-from-rest, vector-then-string: with nothing at all, everything is empty"
  :call (lf-extract-optionals-from-rest nil #'vectorp nil #'stringp nil)
  :returns (nil nil nil))

(define-lf-call-test "lf-extract-optionals-from-rest, vector-then-string: without a vector or string, x starts the rest"
  :call (lf-extract-optionals-from-rest 'x #'vectorp nil #'stringp nil)
  :returns (nil nil (x)))

(define-lf-call-test "lf-extract-optionals-from-rest, vector-then-string: without a vector or string, x and y start the rest"
  :call (lf-extract-optionals-from-rest 'x #'vectorp 'y #'stringp nil)
  :returns (nil nil (x y)))

(define-lf-call-test "lf-extract-optionals-from-rest, vector-then-string: without a vector or string, x, y and z form the rest"
  :call (lf-extract-optionals-from-rest 'x #'vectorp 'y #'stringp '(z))
  :returns (nil nil (x y z)))

(define-lf-call-test "lf-extract-optionals-from-rest, vector-then-string: a string without a vector is the second optional"
  :call (lf-extract-optionals-from-rest "x" #'vectorp 'y #'stringp '(z))
  :returns (nil "x" (y z)))

(define-lf-call-test "lf-extract-optionals-from-rest, vector-then-string: a string without a vector, then y as the rest"
  :call (lf-extract-optionals-from-rest "x" #'vectorp 'y #'stringp nil)
  :returns (nil "x" (y)))

(define-lf-call-test "lf-extract-optionals-from-rest, vector-then-string: a string without a vector, then z as the rest"
  :call (lf-extract-optionals-from-rest "x" #'vectorp nil #'stringp '(z))
  :returns (nil "x" (z)))

(define-lf-call-test "lf-extract-optionals-from-rest, vector-then-string: a string without a vector, and no rest"
  :call (lf-extract-optionals-from-rest "x" #'vectorp nil #'stringp nil)
  :returns (nil "x" nil))

(define-lf-call-test "lf-extract-optionals-from-rest, vector-then-string: a vector then a string are both optionals"
  :call (lf-extract-optionals-from-rest [] #'vectorp "" #'stringp '(z))
  :returns ([] "" (z)))

(define-lf-call-test "lf-extract-optionals-from-rest, vector-then-string: a vector then a string, and no rest"
  :call (lf-extract-optionals-from-rest [] #'vectorp "" #'stringp nil)
  :returns ([] "" nil))

(define-lf-call-test "lf-extract-optionals-from-rest, vector-then-string: a vector then a non-string, which starts the rest"
  :call (lf-extract-optionals-from-rest [] #'vectorp 'y #'stringp '(z))
  :returns ([] nil (y z)))

;;; An optional string, then an optional number

(define-lf-call-test "lf-extract-optionals-from-rest, string-then-number: with nothing at all, everything is empty"
  :call (lf-extract-optionals-from-rest nil #'stringp nil #'numberp nil)
  :returns (nil nil nil))

(define-lf-call-test "lf-extract-optionals-from-rest, string-then-number: without a string or number, x starts the rest"
  :call (lf-extract-optionals-from-rest 'x #'stringp nil #'numberp nil)
  :returns (nil nil (x)))

(define-lf-call-test "lf-extract-optionals-from-rest, string-then-number: without a string or number, x and y start the rest"
  :call (lf-extract-optionals-from-rest 'x #'stringp 'y #'numberp nil)
  :returns (nil nil (x y)))

(define-lf-call-test "lf-extract-optionals-from-rest, string-then-number: without a string or number, x, y and z form the rest"
  :call (lf-extract-optionals-from-rest 'x #'stringp 'y #'numberp '(z))
  :returns (nil nil (x y z)))

(define-lf-call-test "lf-extract-optionals-from-rest, string-then-number: a number without a string is the second optional"
  :call (lf-extract-optionals-from-rest 34 #'stringp 'y #'numberp '(z))
  :returns (nil 34 (y z)))

(define-lf-call-test "lf-extract-optionals-from-rest, string-then-number: a number without a string, then y as the rest"
  :call (lf-extract-optionals-from-rest 34 #'stringp 'y #'numberp nil)
  :returns (nil 34 (y)))

(define-lf-call-test "lf-extract-optionals-from-rest, string-then-number: a number without a string, then z as the rest"
  :call (lf-extract-optionals-from-rest 34 #'stringp nil #'numberp '(z))
  :returns (nil 34 (z)))

(define-lf-call-test "lf-extract-optionals-from-rest, string-then-number: a number without a string, and no rest"
  :call (lf-extract-optionals-from-rest 34 #'stringp nil #'numberp nil)
  :returns (nil 34 nil))

(define-lf-call-test "lf-extract-optionals-from-rest, string-then-number: a string then a number are both optionals"
  :call (lf-extract-optionals-from-rest "" #'stringp 0 #'numberp '(z))
  :returns ("" 0 (z)))

(define-lf-call-test "lf-extract-optionals-from-rest, string-then-number: a string then a number, and no rest"
  :call (lf-extract-optionals-from-rest "" #'stringp 0 #'numberp nil)
  :returns ("" 0 nil))

(define-lf-call-test "lf-extract-optionals-from-rest, string-then-number: a string then a non-number, which starts the rest"
  :call (lf-extract-optionals-from-rest "" #'stringp 'y #'numberp '(z))
  :returns ("" nil (y z)))

(provide 'lf-extract-optionals-tests)
;;; lf-extract-optionals-tests.el ends here
