(defpackage :lem-yaml-mode
  (:use :cl :lem :lem.language-mode)
  (:export :*yaml-mode-hook*))
(in-package :lem-yaml-mode)

#| link: https://yaml.org/spec/1.2/spec.html |#

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

(defun make-tmlanguage-yaml ()
  (let* ((patterns (make-tm-patterns
                    (make-tm-match "^#.*$" :name 'syntax-comment-attribute)
                    (make-tm-string-region "\"")
                    (make-tm-string-region "'")
                    (make-tm-match (tokens nil '("-" "?" ":" "," "[" "]" "{" "}"))
                                   :name 'syntax-builtin-atribute)
                    (make-tm-match (tokens :word-boundary '("true" "false"))
                                   :name 'syntax-keyword-attribute)
                    (make-tm-match "^\\s*[^:]+"
                                   :name 'syntax-constant-attribute))))
    (make-tmlanguage :patterns patterns)))

(defvar *yaml-syntax-table*
  (let ((table (make-syntax-table
                :string-quote-chars '(#\" #\')))
        (tmlanguage (make-tmlanguage-yaml)))
    (set-syntax-parser table tmlanguage)
    table))

(define-major-mode yaml-mode language-mode
    (:name "yaml"
     :keymap *yaml-mode-keymap*
     :syntax-table *yaml-syntax-table*
     :mode-hook *yaml-mode-hook*)
  (setf (variable-value 'enable-syntax-highlight) t
        (variable-value 'indent-tabs-mode) nil
        (variable-value 'tab-width) 2
        (variable-value 'calc-indent-function) 'yaml-calc-indent
        (variable-value 'line-comment) "#"))

(defun yaml-calc-indent (point)
  (with-point ((point point))
    (let ((tab-width (variable-value 'tab-width :default point))
          (column (point-column point)))
      (+ column (- tab-width (rem column tab-width))))))

(pushnew (cons "\\.yaml$" 'yaml-mode) *auto-mode-alist* :test #'equal)
