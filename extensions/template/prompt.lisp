(defpackage #:lem-template/prompt
  (:use :cl :lem)
  (:import-from #:alexandria-2
                #:curry
                #:hash-table-keys)
  (:export #:prompt-hash-table))
(in-package :lem-template/prompt)

(defun table-completion (table string)
  (remove-if-not
   (lambda (it) (str:prefix? (list it) string))
   (hash-table-keys table)))

(defun prompt-hash-table (prompt table)
  (gethash
   (prompt-for-string
    prompt
    :completion-function (curry #'table-completion table))
   table))
