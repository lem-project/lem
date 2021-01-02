(defpackage :lem-dart-mode
  (:use :cl :lem :lem.language-mode :lem.language-mode-tools)
  (:export :*dart-mode-hook*))
(in-package :lem-dart-mode)

#|
see : https://dart.dev/guides/language/language-tour
      https://dart.dev/guides/language/specifications/DartLangSpec-v2.2.pdf
|#

(defparameter *dart-constants*
  '("false" "true" "null"))

(defparameter *dart-builtin-types*
  '("BidirectionalIterator" "BigInt" "bool" "Comparable"
    "DateTime" "Deprecated" "double" "Duration"
    "Expando" "Function" "Future" "int"
    "Invocation" "Iterable" "Iterator" "List"
    "Map" "MapEntry" "Match" "Null"
    "num" "Object" "Pattern" "pragma"
    #+(or) "Provisional" "RegExp" "RegExpMatch" "RuneIterator"
    "Runes" "Set" "Sink" "StackTrace"
    "Stopwatch" "Stream" "String" "StringBuffer"
    "StringSink" "Symbol" "Type" "Uri"
    "UriData"))

(defparameter *dart-keywords*
  (sort (set-difference
         '("abstract" "as" "assert" "async" "await" "break" "case" "catch" "class" "const" "continue"
           "covariant" "default" "deferred" "do" "dynamic" "else" "enum" "export" "extends"
           "extension" "external" "factory""false" "final" "finally" "for" "function" "get" "hide"
           "if" "implements" "import" "in" "interface" "is" "library" "mixin" "new""null" "on"
           "operator" "part" "rethrow" "return" "set" "show" "static" "super" "switch" "sync" "this"
           "throw""true" "try" "typedef" "var" "void" "while" "with" "yield")
         *dart-constants*
         :test #'string=)
        #'string<))

(defvar *dart-arithmetic-operators* '("+" "-" "*" "/" "~/" "%" "++" "--"))
(defvar *dart-equality-and-relational-operators* '("==" "!=" ">" "<" ">=" "<="))
(defvar *dart-type-test-operators* '("as" "is" "is!"))
(defvar *dart-assignment-operators* '("=" "-=" "/=" "%=" ">>=" "^=" "+=" "*=" "~/=" "<<=" "&=" "|="))
(defvar *dart-logical-operators* '("!" "||" "&&"))
(defvar *dart-bitwise-and-shift-operators* '("&" "|" "^" "~" "<<" ">>"))
(defvar *dart-conditional-expressions* '("?" ":" "??" "?."))
(defvar *dart-cascade-notation* '(".."))

;; numeric literal
(defvar *dart-exponent-literal* "(e|E)(\\+|\\-)?[0-9]+")
(defvar *dart-number-literal* (format nil
                                      "([0-9]+(\\.[0-9]+)?(~A)?)|(\\.[0-9]+(~A)?)"
                                      *dart-exponent-literal*
                                      *dart-exponent-literal*))
(defvar *dart-hex-number-literal* "(0x[a-fA-F0-9]+)|(0X[a-fA-F0-9]+)")
(defvar *dart-numeric-literal* (format nil "(~A)|(~A)" *dart-hex-number-literal* *dart-number-literal*))

(defun tokens (boundary strings)
  (let ((alternation
         `(:alternation ,@(sort (copy-list strings) #'> :key #'length))))
    (if boundary
        `(:sequence ,boundary ,alternation ,boundary)
        alternation)))

(defun make-tmlanguage-dart ()
  (let ((patterns
          (make-tm-patterns
           (make-tm-line-comment-region "//")
           (make-tm-string-region "\'")
           (make-tm-string-region "\"")
           (make-tm-match (tokens :word-boundary *dart-keywords*)
                          :name 'syntax-keyword-attribute)
           (make-tm-match (tokens :word-boundary *dart-constants*)
                          :name 'syntax-constant-attribute)
           (make-tm-match *dart-numeric-literal*
                          :name 'syntax-constant-attribute)
           (make-tm-match (tokens :word-boundary *dart-builtin-types*)
                          :name 'syntax-type-attribute)
           (make-tm-match (tokens nil (append *dart-arithmetic-operators*
                                              *dart-equality-and-relational-operators*
                                              *dart-assignment-operators*
                                              *dart-logical-operators*
                                              *dart-bitwise-and-shift-operators*
                                              *dart-conditional-expressions*
                                              *dart-cascade-notation*))
                          :name 'syntax-builtin-attribute)
           (make-tm-match (tokens :word-boundary *dart-type-test-operators*)
                          :name 'syntax-builtin-attribute))))
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
