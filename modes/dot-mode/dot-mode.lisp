#| see: https://www.graphviz.org/doc/info/lang.html |#
(defpackage :lem-dot-mode
  (:use :cl :lem :lem.language-mode :lem.language-mode-tools)
  (:import-from :cl-ppcre
                :scan
                :all-matches-as-strings)
  (:export :*dot-mode-hook*))
(in-package :lem-dot-mode)

(defvar *dot-types*
 '("strict" "graph" "digraph" "graph" "node" "edge" "subgraph"))

(defvar *dot-operators*
  '(";" "," "=" ":" "->" "--" "+"))

(defun tokens (boundary strings)
 (let ((alternation
        `(:alternation ,@(sort (copy-list strings) #'> :key #'length))))
   (if boundary
       `(:sequence ,boundary ,alternation ,boundary)
       alternation)))

(defun line-comment-region (start)
  (make-tm-region start "$"
                  :name 'syntax-comment-attribute))

(defun block-comment-region (start end)
  (make-tm-region `(:sequence ,start)
                  `(:sequence ,end)
                  :name 'syntax-comment-attribute))

(defun make-tmlanguage-dot ()
  (let* ((patterns (make-tm-patterns
                    (line-comment-region "//")
                    (block-comment-region "/*" "*/")
                    (make-tm-region '(:sequence "\"")
                                    '(:sequence "\"")
                                    :name 'syntax-string-attribute
                                    :patterns (make-tm-patterns
                                               (make-tm-match "\\\\.")))
                    (make-tm-match "-?(\\.[0-9]+)|([0-9]+(\\.[0-9]*)?)"
                                   :name 'syntax-constant-attribute)
                    (make-tm-match (tokens :word-boundary *dot-types*)
                                   :name 'syntax-type-attribute)
                    (make-tm-match (tokens nil *dot-operators*)
                                   :name 'syntax-keyword-attribute))))
    
    (make-tmlanguage :patterns patterns)))

(defvar *dot-syntax-table*
  (let ((table (make-syntax-table
                :space-chars '(#\space #\tab #\newline)
                :string-quote-chars '(#\")
                :line-comment-string "//"
                :block-comment-pairs '(("/*" . "*/"))))
        (tmlanguage (make-tmlanguage-dot)))
    (set-syntax-parser table tmlanguage)
    table))

(define-major-mode dot-mode language-mode
    (:name "dot"
     :keymap *dot-mode-keymap*
     :syntax-table *dot-syntax-table*
     :mode-hook *dot-mode-hook*)
  (setf (variable-value 'enable-syntax-highlight) t
        (variable-value 'tab-width) 2))

(dolist (pattern '("\\.dot$"))
  (pushnew (cons pattern 'dot-mode)
           *auto-mode-alist*
           :test #'equal))
