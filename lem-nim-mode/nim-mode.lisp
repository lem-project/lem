(defpackage :lem-nim-mode
  (:use :cl :lem :lem.language-mode)
  (:export :*nim-mode-hook*)
  #+sbcl
  (:lock t))
(in-package :lem-nim-mode)

(defvar *nim-mode-hook* '())

(defun tokens (boundary strings)
  (let ((alternation
         `(:alternation ,@(sort (copy-list strings) #'> :key #'length))))
    (if boundary
        `(:sequence ,boundary ,alternation ,boundary)
        alternation)))

(defun make-tm-string-region (sepalator)
  (make-tm-region `(:sequence ,sepalator)
                  `(:sequence ,sepalator)
                  :name 'syntax-string-attribute
                  :patterns (make-tm-patterns (make-tm-match "\\\\."))))

;; numerical literals
;; cf. https://nim-lang.org/docs/manual.html#lexical-analysis-numerical-constants
(let* ((digit "[0-9]")
       (octdigit "[0-7]")
       (hexdigit "[0-9A-Fa-f]")
       (bindigit "[0-1]")
       (hexlit (format nil "0(x|X)~a(_?~a)*" hexdigit hexdigit))
       (declit (format nil "~a(_?~a)*" digit digit))
       (octlit (format nil "0(o|c|C)~a(_?~a)*" octdigit octdigit))
       (binlit (format nil "0(b|b)~a(_?~a)*" bindigit bindigit)))

  (defun integer-literals ()
    (let* ((intlit (format nil "(~@{~a~^|~})" hexlit declit octlit binlit))
           (intsuffix "'?(i|I)(8|16|32|64)"))
      (format nil "\\b~a(~a)?\\b" intlit intsuffix)))

  (defun float-literals ()
    (let* ((exponent (format nil "(e|E)[+-]?~a(_?~a)*" digit digit))
           ;; it seems `10_20.e1_0` should be OK but compiler fails.
           ;; issue: https://github.com/nim-lang/Nim/issues/8766
           (floatlit (format nil "~a(_?~a)*((\\.(_?~a)*(~a)?)|~a)" digit digit digit exponent exponent))
           (floatsuffix "((f|F)(32)?|((f|F)64)|d|D)")
           (floatsuffixlit (format nil "(~a'~a)|((~a|~a|~a|~a)'?~a)"
                                   hexlit floatsuffix floatlit declit octlit binlit floatsuffix)))
      (format nil "\\b(~a|~a)\\b" floatlit floatsuffixlit))))

;; cf. https://nim-lang.org/docs/manual.html#syntax-grammar
(defun make-tmlanguage-nim ()
  (let* ((patterns (make-tm-patterns
                    (make-tm-region "#" "$" :name 'syntax-comment-attribute)
                    ;; keywords: https://nim-lang.org/docs/manual.html#lexical-analysis-identifiers-keywords
                    (make-tm-match (tokens :word-boundary
                                           '("addr" "and" "as" "asm" "bind" "block" "break"
                                             "case" "cast" "concept" "const" "continue" "converter"
                                             "defer" "discard" "distinct" "div" "do" "elif" "else"
                                             "end" "enum" "except" "export" "finally" "for" "from"
                                             "func" "if" "import" "in" "include" "interface" "is"
                                             "isnot" "iterator" "let" "macro" "method" "mixin" "mod"
                                             "nil" "not" "notin" "object" "of" "or" "out" "proc"
                                             "ptr" "raise" "ref" "return" "shl" "shr" "static"
                                             "template" "try" "tuple" "type" "using" "var" "when"
                                             "while" "xor" "yield"))
                                   :name 'syntax-keyword-attribute)
                    (make-tm-match (tokens :word-boundary
                                           '("false" "true" "nil"))
                                   :name 'syntax-constant-attribute)
                    (make-tm-string-region "\"")
                    (make-tm-string-region "\"\"\"")
                    (make-tm-match (integer-literals)
                                   :name 'syntax-constant-attribute)
                    (make-tm-match (float-literals)
                                   :name 'syntax-constant-attribute)
                    (make-tm-region
                     `(:sequence "#|")
                     `(:sequence "|#")
                     :name 'syntax-comment-attribute))))
    (make-tmlanguage :patterns patterns)))

(defvar *nim-syntax-table*
  (let ((table (make-syntax-table
                :space-chars '(#\space #\newline)
                :paren-pairs '((#\( . #\))
                               (#\{ . #\})
                               (#\[ . #\]))
                :string-quote-chars '(#\" #\')
                :block-string-pairs '(("\"\"\"" . "\"\"\""))
                :line-comment-string "#"))
        (tmlanguage (make-tmlanguage-nim)))
    (set-syntax-parser table tmlanguage)
    table))

(defun nim-calc-indent (point))
(defun beginnng-of-defun (point n))
(defun end-of-defun (point n))

(define-major-mode nim-mode language-mode
    (:name "nim"
     :keymap *nim-mode-keymap*
     :syntax-table *nim-syntax-table*)
  (setf (variable-value 'enable-syntax-highlight) t
        (variable-value 'indent-tabs-mode) nil
        (variable-value 'tab-width) 2
        (variable-value 'calc-indent-function) 'nim-calc-indent
        (variable-value 'line-comment) "#"
        (variable-value 'beginning-of-defun-function) 'beginning-of-defun
        (variable-value 'end-of-defun-function) 'end-of-defun)
  (run-hooks *nim-mode-hook*))

(pushnew (cons "\\.nim$" 'nim-mode) *auto-mode-alist* :test #'equal)
(pushnew (cons "\\.nimble$" 'nim-mode) *auto-mode-alist* :test #'equal)
