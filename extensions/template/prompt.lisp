(defpackage #:lem-template/prompt
  (:use :cl :lem)
  (:import-from #:alexandria-2 #:hash-table-keys #:rcurry)
  (:export #:prompt-hash-table))
(in-package :lem-template/prompt)

(defun prompt-hash-table (prompt table &key with-none-option)
  "Prompt the keys of a hash-table, return the corresponding value."
  (gethash
   (prompt-for-string
    prompt
    :completion-function
    (rcurry #'completion
            (if with-none-option
                (cons "none" (hash-table-keys table))
                (hash-table-keys table))
            :test #'lem-core::fuzzy-match-p))
   table))
