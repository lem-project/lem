(defpackage #:lem-template/utils
  (:use :cl :lem)
  (:import-from #:alexandria-2 #:hash-table-keys)
  (:export #:buffer-empty-p #:new-file-p #:hash-table-first))
(in-package :lem-template/utils)

(defun buffer-empty-p (buffer)
  "If start and end are equal, buffer is empty."
  (point= (buffer-start-point buffer)
          (buffer-end-point buffer)))

(defun new-file-p (buffer)
  "Buffer is a new file, and does not already exist on disk."
  (not (uiop:file-exists-p (buffer-filename buffer))))

(defun hash-table-first (table)
  "Get one item out of a hash-table."
  (gethash (car (hash-table-keys table)) table))
