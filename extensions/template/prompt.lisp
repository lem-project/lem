(defpackage #:lem-template/prompt
  (:use :cl :lem)
  (:import-from #:alexandria-2 #:curry #:hash-table-keys)
  (:export #:prompt-hash-table))
(in-package :lem-template/prompt)

(defun table-completion (table string)
  "Completion function for #'prompt-hash-table."
  (cons "none"
        (remove-if-not
         (lambda (it) (str:prefix? (list it) string))
         (hash-table-keys table))))

(defun prompt-hash-table (prompt table)
  "Prompt the keys of a hash-table, return the corresponding value."
  (gethash
   (prompt-for-string
    prompt
    :completion-function (curry #'table-completion table))
   table))
