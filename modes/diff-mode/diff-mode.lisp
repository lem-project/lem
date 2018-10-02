(defpackage :lem-diff-mode
  (:use :cl :lem :lem.language-mode)
  (:export :*diff-mode-hook*))
(in-package :lem-diff-mode)

(define-attribute add-line-attribute
  (:light :foreground "blue")
  (:dark :foreground "cyan"))

(define-attribute delete-line-attribute
  (t :foreground "red"))

(defun make-tmlanguage-diff ()
  (let* ((patterns (make-tm-patterns
                    (make-tm-match "^\\+.*$" :name 'add-line-attribute)
                    (make-tm-match "^-.*$" :name 'delete-line-attribute))))
    (make-tmlanguage :patterns patterns)))

(defvar *diff-syntax-table*
  (let ((table (make-syntax-table)))
    (set-syntax-parser table (make-tmlanguage-diff))
    table))

(define-major-mode diff-mode language-mode
    (:name "diff"
     :syntax-table *diff-syntax-table*)
  (setf (variable-value 'enable-syntax-highlight) t))

(pushnew (cons "\\.patch$" 'diff-mode) *auto-mode-alist* :test #'equal)
