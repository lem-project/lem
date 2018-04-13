(defpackage :lem-python-mode
  (:use :cl :lem :lem.language-mode)
  (:export :python-mode
           :*python-mode-hook*))
(in-package :lem-python-mode)

(defvar *python-mode-hook* '())

(defun tokens (boundary strings)
  (let ((alternation
         `(:alternation ,@(sort (copy-list strings) #'> :key #'length))))
    (if boundary
        `(:sequence ,boundary ,alternation ,boundary)
        alternation)))

#| link : https://docs.python.org/3/reference/lexical_analysis.html |#
(defun make-tmlanguage-python ()
  (let* ((patterns (make-tm-patterns
                    (make-tm-region "#" "$" :name 'syntax-comment-attribute)
                    (make-tm-match (tokens :word-boundary
                                           '("False" "None" "True" "and" "as"
                                             "assert" "break" "class" "continue" "def"
                                             "del" "elif" "else" "except" "finally"
                                             "for" "from" "global" "if" "import"
                                             "in" "is" "lambda" "nonlocal" "not"
                                             "or" "pass" "raise" "return" "try"
                                             "while" "with" "yield"))
                                   :name 'syntax-keyword-attribute)
                    (make-tm-region '(:sequence "\"")
                                    '(:sequence "\"")
                                    :name 'syntax-string-attribute
                                    :patterns (make-tm-patterns (make-tm-match "\\\\.")))
                    (make-tm-region '(:sequence "'")
                                    '(:sequence "'")
                                    :name 'syntax-string-attribute
                                    :patterns (make-tm-patterns (make-tm-match "\\\\.")))
                    (make-tm-match "[0-9]+" :name 'syntax-constant-attribute)
                    (make-tm-match (tokens nil '("+" "-" "*" "**" "/" "//" "%" "@"
                                                 "<<" ">>" "&" "|" "^" "~"
                                                 "<" ">" "<=" ">=" "==" "!="))
                                   :name 'syntax-keyword-attribute))))
    (make-tmlanguage :patterns patterns)))

(defvar *python-syntax-table*
  (let ((table (make-syntax-table
                :space-chars '(#\space #\tab #\newline)
                :paren-alist '((#\( . #\))
                               (#\{ . #\})
                               (#\[ . #\]))
                :string-quote-chars '(#\" #\')
                :line-comment-string "#"))
        (tmlanguage (make-tmlanguage-python)))
    (set-syntax-parser table tmlanguage)
    table))

(define-major-mode python-mode language-mode
    (:name "python"
     :keymap *python-mode-keymap*
     :syntax-table *python-syntax-table*)
  (setf (variable-value 'enable-syntax-highlight) t)
  (run-hooks *python-mode-hook*))

(pushnew (cons "\\.py$" 'python-mode) *auto-mode-alist* :test #'equal)
