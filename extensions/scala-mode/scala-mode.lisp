(defpackage :lem-scala-mode
  (:use :cl :lem :lem/language-mode :lem/language-mode-tools)
  (:export :scala-mode))
(in-package :lem-scala-mode)

(defun tokens (boundary strings)
  (let ((alternation
          `(:alternation ,@(sort (copy-list strings) #'> :key #'length))))
    (if boundary
        `(:sequence ,boundary ,alternation ,boundary)
        alternation)))

(defun make-tmlanguage-scala ()
  (let ((patterns (make-tm-patterns
                   (make-tm-region '(:sequence "//") "$" :name 'syntax-comment-attribute)
                   (make-tm-match (tokens :word-boundary
                                          '("abstract" "case" "catch" "class" "def"
                                            "do" "else" "extends" "false" "final"
                                            "finally" "for" "forSome" "if" "implicit"
                                            "import" "lazy" "match" "new" "null"
                                            "object" "override" "package" "private" "protected"
                                            "requires" "return" "sealed" "super" "this"
                                            "throw" "trait" "try" "true" "type"
                                            "val" "var" "while" "with" "yield"
                                            "_" ":" "=" "=>" "<<:" "<%" ">:" "#" "@"))
                                  :name 'syntax-keyword-attribute)
                   (make-tm-string-region "\"")
                   (make-tm-string-region "'")
                   (make-tm-string-region "\"\"\"")
                   (make-tm-string-region "'''"))))
    (make-tmlanguage :patterns patterns)))

(defvar *syntax-table*
  (let ((table (make-syntax-table
                :space-chars '(#\space #\tab #\newline)
                :paren-pairs '((#\( . #\))
                               (#\{ . #\})
                               (#\[ . #\]))
                :string-quote-chars '(#\" #\')
                :block-string-pairs '(("\"\"\"" . "\"\"\""))
                :line-comment-string "//")))
    (set-syntax-parser table (make-tmlanguage-scala))
    table))

(define-major-mode scala-mode language-mode
    (:name "Scala"
     :keymap *scala-mode-keymap*
     :syntax-table *syntax-table*
     :mode-hook *scala-mode-hook*)
  (setf (variable-value 'enable-syntax-highlight) t
        (variable-value 'indent-tabs-mode) nil
        (variable-value 'tab-width) 2
        (variable-value 'calc-indent-function) 'lem-js-mode::js-calc-indent
        (variable-value 'line-comment) "//"
        (variable-value 'beginning-of-defun-function) 'beginning-of-defun
        (variable-value 'end-of-defun-function) 'end-of-defun))

(defun beginning-of-defun (point n)
  (loop :repeat n :do (search-backward-regexp point "^\\w")))

(defun end-of-defun (point n)
  (with-point ((p point))
    (loop :repeat n
          :do (line-offset p 1)
              (if (search-forward-regexp p "^[\\w}]")
                  (character-offset p 1)
                  (return)))
    (line-start p)
    (move-point point p)))

(define-file-type ("scala") scala-mode)
