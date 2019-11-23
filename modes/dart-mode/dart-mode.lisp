(defpackage :lem-dart-mode
  (:use :cl :lem :lem.language-mode)
  (:export :*dart-mode-hook*))
(in-package :lem-dart-mode)

(defun make-tmlanguage-dart ()
  (let ((patterns (make-tm-patterns
                   (make-tm-region "//" "$" :name 'syntax-comment-attribute))))
    (make-tmlanguage :patterns patterns)))

(defvar *dart-syntax-table*
  (let ((table (make-syntax-table
                :space-chars '(#\space #\tab #\newline)
                :symbol-chars '(#\_)
                :paren-pairs '((#\( . #\))
                               (#\{ . #\})
                               (#\[ . #\]))
                :string-quote-chars '(#\" #\' #\`)
                :line-comment-string "//"))
        (tmlanguage (make-tmlanguage-dart)))
    (set-syntax-parser table tmlanguage)
    table))

(define-major-mode dart-mode language-mode
    (:name "Dart"
     :keymap *dart-mode-keymap*
     :syntax-table *dart-syntax-table*
     :mode-hook *dart-mode-hook*)
  (setf (variable-value 'enable-syntax-highlight) t
        (variable-value 'tab-width) 2
        (variable-value 'line-comment) "//"
        (variable-value 'insertion-line-comment) "// "))

(pushnew (cons "\\.dart$" 'dart-mode) *auto-mode-alist* :test #'equal)
