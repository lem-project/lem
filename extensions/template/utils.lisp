(defpackage #:lem-template/utils
  (:use :cl :lem)
  (:import-from #:alexandria-2 #:hash-table-keys)
  (:export #:new-file-p #:hash-table-first))
(in-package :lem-template/utils)

(defun new-file-p (buffer)
  "Buffer is a new file, and does not already exist on disk."
  (not (uiop:file-exists-p (buffer-filename buffer))))

(defun hash-table-first (table)
  "Get one item out of a hash-table."
  (gethash (car (hash-table-keys table)) table))
