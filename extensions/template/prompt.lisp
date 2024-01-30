(defpackage #:lem-template/prompt
  (:use :cl :lem)
  (:import-from #:alexandria-2 #:curry #:hash-table-keys)
  (:export #:prompt-hash-table))
(in-package :lem-template/prompt)

(defun table-completion (table &key with-none-option)
  "Completion function for #'prompt-hash-table."
  (lambda (string)
    (remove-if-not
     (lambda (it) (str:prefix? (list it) string))
     (if with-none-option
         (cons "none" (hash-table-keys table))
         (hash-table-keys table)))))

(defun prompt-hash-table (prompt table &key with-none-option)
  "Prompt the keys of a hash-table, return the corresponding value."
  (gethash
   (prompt-for-string
    prompt
    :completion-function (table-completion table :with-none-option with-none-option))
   table))
