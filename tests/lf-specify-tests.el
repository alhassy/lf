;;; lf-specify-tests.el --- Tests for lf-specify: contracts on existing functions, and their call chains  -*- lexical-binding: t; -*-

(unless (fboundp 'define-lf-call-test)
  (load (expand-file-name "lf-test-utils.el" (file-name-directory (or load-file-name buffer-file-name))) nil t))

;;; Bounds checking on a wrapper
;;
;; `lf-specify' on built-in C functions like `nth' recurses forever ---the advice re-enters the same C
;; function--- so we demonstrate bounds checking on a thin user-defined wrapper instead.

(defmacro lf-test--with-safe-nth (&rest body)
  "Run BODY with `safe-nth' defined as `nth', its index constrained to lie within the list."
  `(progn (defun safe-nth (n list) (nth n list))
          (lf-specify safe-nth (n list)
            :requires (and (integerp n) (listp list) (<= 0 n) (< n (length list))))
          ,@body))

(deftest "lf-specify allows valid in-bounds access on a safe-nth wrapper"
  (lf-test--with-safe-nth
   (should (equal (safe-nth 1 '(a b c)) 'b))))

(deftest "lf-specify catches negative indices on a safe-nth wrapper"
  (lf-test--with-safe-nth
   (should-error (safe-nth -1 '(a b c)))))

(deftest "lf-specify catches out-of-bounds indices on a safe-nth wrapper"
  (lf-test--with-safe-nth
   (should-error (safe-nth 5 '(a b c)))))

;;; Call chains in error messages
;;
;; An `lf-specify' error once named only the failing function; these check that its "CALLED FROM: …"
;; trailer names every intermediate caller of a genuine multi-level chain.  The trailer is the subject
;; here, so ---unlike elsewhere--- we keep it, checking for its parts.

(deftest "lf-specify's error names every function in a multi-level call chain, innermost first"
  (lf-define bt-inner (x) [:requires (integerp x)] "innermost" (* x 2))
  (lf-define bt-middle (x) "middle calls inner" (bt-inner x))
  (lf-define bt-outer (x) "outer calls middle" (bt-middle x))
  (let ((msg (cl-second (should-error (bt-outer "not-an-int")))))
    (should (string-match-p "CALLED FROM:" msg))
    (should (string-match-p "bt-middle" msg))
    (should (string-match-p "bt-outer" msg))
    (should (< (string-match "bt-middle" msg) (string-match "bt-outer" msg)))))

(deftest "lf-specify's :ensures panic also carries the CALLED FROM trailer"
  (lf-define bt-inner2 (x) [:ensures (stringp result)] "innermost, deliberately wrong return type" (ignore x) 42)
  (lf-define bt-outer2 (x) "calls inner2" (bt-inner2 x))
  (let ((msg (cl-second (should-error (bt-outer2 1)))))
    (should (string-match-p "Panic!" msg))
    (should (string-match-p "CALLED FROM:" msg))
    (should (string-match-p "bt-outer2" msg))))

(define-lf-call-test "lf--backtrace-string is nil, not empty, when nothing calls the marker function"
  :call (lf--backtrace-string 'a-marker-function-nothing-calls)
  :returns nil)

(provide 'lf-specify-tests)
;;; lf-specify-tests.el ends here
