(defpackage :lem-xml-mode
  (:use :cl :lem :lem.language-mode)
  (:export :xml-mode
           :*xml-mode-hook*))
(in-package :lem-xml-mode)

(defvar *xml-mode-hook* '())

(defun tokens (boundary strings)
  (let ((alternation
         `(:alternation ,@(sort (copy-list strings) #'> :key #'length))))
    (if boundary
        `(:sequence ,boundary ,alternation ,boundary)
        alternation)))

(defun make-tmlanguage-xml ()
  (let* ((patterns (make-tm-patterns
                    (make-tm-region '(:sequence "<!--")
                                    '(:sequence "-->")
                                    :name 'syntax-comment-attribute)
                    (make-tm-region '(:sequence "\"")
                                    '(:sequence "\"")
                                    :name 'syntax-string-attribute
                                    :patterns (make-tm-patterns (make-tm-match "\\\\.")))
                    (make-tm-region '(:sequence "'")
                                    '(:sequence "'")
                                    :name 'syntax-string-attribute
                                    :patterns (make-tm-patterns (make-tm-match "\\\\.")))
                    (make-tm-match (tokens nil '("<" "</" "<?" ">" "/>" "?>" "="))
                                   :name 'syntax-keyword-attribute))))
    (make-tmlanguage :patterns patterns)))

(defvar *xml-syntax-table*
  (let ((table (make-syntax-table
                :space-chars '(#\space #\tab #\newline)
                :paren-pairs '((#\< . #\>))
                :string-quote-chars '(#\" #\' #\`)
                :block-comment-pairs '(("<!--" . "-->"))))
        (tmlanguage (make-tmlanguage-xml)))
    (set-syntax-parser table tmlanguage)
    table))

(define-major-mode xml-mode language-mode
    (:name "xml"
     :keymap *xml-mode-keymap*
     :syntax-table *xml-syntax-table*)
  (setf (variable-value 'enable-syntax-highlight) t)
  (run-hooks *xml-mode-hook*))

(pushnew (cons "\\.xml$" 'xml-mode) *auto-mode-alist* :test #'equal)
