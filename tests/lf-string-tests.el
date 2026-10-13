;;; lf-string-tests.el --- Tests for lf-unindent and lf-string  -*- lexical-binding: t; -*-

(unless (fboundp 'define-lf-call-test)
  (load (expand-file-name "lf-test-utils.el" (file-name-directory (or load-file-name buffer-file-name))) nil t))

;;; lf-unindent
;;
;; `lf-string' calls `lf-unindent' as its very first step, so the multi-line `lf-string' tests below
;; re-derive most of this behaviour ---but only implicitly.  These test `lf-unindent' directly, so a
;; regression here points straight back at itself.

(define-lf-call-test "lf-unindent leaves a single-line string unchanged"
  :call (lf-unindent "Hello, world.")
  :returns "Hello, world.")

(define-lf-call-test "lf-unindent strips the common leading whitespace of a multi-line string"
  :call (lf-unindent
         "1. This is the first line, it has zero indentation.
          2. This line indicates the indentation offset for the remaining lines.
          3. As such, this line has no indentation.
             4. But this one is clearly indented (relative to line 2)")
  :returns "1. This is the first line, it has zero indentation.
2. This line indicates the indentation offset for the remaining lines.
3. As such, this line has no indentation.
   4. But this one is clearly indented (relative to line 2)")

(define-lf-call-test "lf-unindent ignores blank lines when computing the common indent"
  :call (lf-unindent
         "1. First line, zero indentation.
          2. Second line sets the offset.

          3. Third line, same offset as the second.")
  :returns "1. First line, zero indentation.
2. Second line sets the offset.

3. Third line, same offset as the second.")

(define-lf-call-test "lf-unindent keeps a line's extra indentation relative to the common minimum"
  :call (lf-unindent
         "1. Zero indentation.
     2. Five-space indentation, sets the common minimum.
     3. Also five-space indentation.
        4. Eight-space indentation ---three deeper than the minimum.")
  :returns "1. Zero indentation.
2. Five-space indentation, sets the common minimum.
3. Also five-space indentation.
   4. Eight-space indentation ---three deeper than the minimum.")

;;; lf-string

(define-lf-call-test "lf-string unindents multi-line strings"
  :call (lf-string "0. This is the first line, it has zero indentation.
                    1. This line indicates the indentation offset for the remaining lines.
                    2. As such, this line has no indentation.
                       3. But this one is clearly indentated (relative to line 1)")
  :returns "0. This is the first line, it has zero indentation.
1. This line indicates the indentation offset for the remaining lines.
2. As such, this line has no indentation.
   3. But this one is clearly indentated (relative to line 1)")

(define-lf-call-test "lf-string leaves single-line strings unchanged"
  :call (lf-string "Hello my friends")
  :returns "Hello my friends")

(define-lf-call-test "lf-string interpolates variables and expressions on a single line"
  :call (let ((you 'Jasim)) (lf-string "me and ${you} like the number ${(+ 2 3)}"))
  :returns "me and Jasim like the number 5")

(define-lf-call-test "lf-string interpolates expressions involving string literals"
  :call (let ((you 'Jasim)) (lf-string "${(concat \"m\" \"e\")} and ${you}"))
  :returns "me and Jasim")

(deftest "lf-string interpolates an expression spread over multiple lines"
  ;; The interpolated docstring of `apply' varies by Emacs version, so we check only its shape.
  (should (string-match-p
           (rx "Did you know that" (* anychar) "Call FUNCTION with our remaining args" (* anychar) "is super cool!")
           (lf-string "Did you know that
                       ${(documentation

                            'apply)}
                       is super cool!"))))

(provide 'lf-string-tests)
;;; lf-string-tests.el ends here
