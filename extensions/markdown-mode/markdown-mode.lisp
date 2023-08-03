#| link : https://daringfireball.net/projects/markdown/syntax |#

(defpackage :lem-markdown-mode
  (:use :cl :lem :lem/language-mode)
  (:export :*markdown-mode-hook*
           :markdown-mode))
(in-package :lem-markdown-mode)

(defun make-tmlanguage-markdown ()
  (let* ((patterns (make-tm-patterns
                    (make-tm-match "^#.*$" :name 'syntax-constant-attribute)
                    (make-tm-match "^>.*$" :name 'syntax-string-attribute)
                    (make-tm-region '(:sequence "```")
                                    '(:sequence "```")
                                    :name 'syntax-string-attribute
                                    :patterns (make-tm-patterns (make-tm-match "\\\\.")))
                    (make-tm-match "([-*_] ?)([-*_] ?)([-*_] ?)+"
                                   :name 'syntax-comment-attribute)
                    (make-tm-match "^ *([*+\\-]|([0-9]+\\.)) +"
                                   :name 'syntax-keyword-attribute))))
    (make-tmlanguage :patterns patterns)))

(defvar *markdown-syntax-table*
  (let ((table (make-syntax-table
                :space-chars '(#\space #\tab #\newline)
                :string-quote-chars '(#\`)))
        (tmlanguage (make-tmlanguage-markdown)))
    (set-syntax-parser table tmlanguage)
    table))

(define-major-mode markdown-mode language-mode
    (:name "Markdown"
     :keymap *markdown-mode-keymap*
     :syntax-table *markdown-syntax-table*
     :mode-hook *markdown-mode-hook*)
  (setf (variable-value 'enable-syntax-highlight) t
        (variable-value 'indent-tabs-mode) nil
        (variable-value 'tab-width) 4
        (variable-value 'calc-indent-function) 'markdown-calc-indent))

(defun markdown-calc-indent (point)
  (with-point ((point point))
    (let ((tab-width (variable-value 'tab-width :default point))
          (column (point-column point)))
      (+ column (- tab-width (rem column tab-width))))))

(define-file-type ("md" "markdown") markdown-mode)
