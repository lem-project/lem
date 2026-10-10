(defpackage :lem-prolog
  (:use :cl :lem :lem/language-mode :lem/language-mode-tools)
  (:export :prolog-mode
           :*prolog-mode-hook*
           :*prolog-mode-keymap*
           :*prolog-syntax-table*)
  (:documentation "Prolog major mode.
The interaction is a port of Markus Triska's ediprolog 2.5-alpha1
(https://www.metalevel.at/ediprolog/)."))

(in-package :lem-prolog)


(defun make-tmlanguage-prolog ()
  (make-tmlanguage
   :patterns (make-tm-patterns
              (make-tm-line-comment-region "%")
              (make-tm-block-comment-region "/\\*" "\\*/")
              (make-tm-string-region "\"")
              (make-tm-string-region "'")
              (make-tm-match "\\b[0-9]+(\\.[0-9]+)?\\b"
                             :name 'syntax-constant-attribute)
              (make-tm-match "\\b(?:true|fail|false|halt)\\b"
                             :name 'syntax-constant-attribute))))

(defvar *prolog-syntax-table*
  (let ((table (make-syntax-table
                :space-chars '(#\space #\tab #\newline)
                :symbol-chars '(#\_)
                :paren-pairs '((#\( . #\)) (#\[ . #\]) (#\{ . #\}))
                :string-quote-chars '(#\" #\')
                :line-comment-string "%"
                :block-comment-pairs '(("/*" . "*/")))))
    (set-syntax-parser table (make-tmlanguage-prolog))
    table))

(define-major-mode prolog-mode language-mode
    (:name "Prolog"
     :keymap *prolog-mode-keymap*
     :syntax-table *prolog-syntax-table*
     :mode-hook *prolog-mode-hook*)
  (setf (variable-value 'enable-syntax-highlight) t
        (variable-value 'indent-tabs-mode) nil
        (variable-value 'tab-width) 4
        (variable-value 'line-comment) "%"
        (variable-value 'insertion-line-comment) "% "))


(define-file-type ("pl" "prolog") prolog-mode)
