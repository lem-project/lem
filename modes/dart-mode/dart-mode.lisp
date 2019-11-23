(defpackage :lem-dart-mode
  (:use :cl :lem :lem.language-mode)
  (:export :*dart-mode-hook*))
(in-package :lem-dart-mode)

#|
see : https://dart.dev/guides/language/language-tour
      https://dart.dev/guides/language/specifications/DartLangSpec-v2.2.pdf
|#

(defvar *dart-keywords*
  '("abstract" "dynamic" "implements" "show"
    "as" "else" "import" "static"
    "assert" "enum" "in" "super"
    "async" "export" "interface" "switch"
    "await" "extends" "is" "sync"
    "break" "external" "library" "this"
    "case" "factory" "mixin" "throw"
    "catch" #+(or) "false" "new" #+(or) "true"
    "class" "final" #+(or) "null" "try"
    "const" "finally" "on" "typedef"
    "continue" "for" "operator" "var"
    "covariant" "Function" "part" "void"
    "default" "get" "rethrow" "while"
    "deferred" "hide" "return" "with"
    "do" "if" "set" "yield"))

(defvar *dart-constants*
  '("false" "true" "null"))

(defun tokens (boundary strings)
  (let ((alternation
         `(:alternation ,@(sort (copy-list strings) #'> :key #'length))))
    (if boundary
        `(:sequence ,boundary ,alternation ,boundary)
        alternation)))

(defun make-tmlanguage-dart ()
  (let ((patterns
          (make-tm-patterns
           (make-tm-region "//" "$" :name 'syntax-comment-attribute)
           (make-tm-match (tokens :word-boundary *dart-keywords*)
                          :name 'syntax-keyword-attribute)
           (make-tm-match (tokens :word-boundary *dart-constants*)
                          :name 'syntax-constant-attribute))))
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
