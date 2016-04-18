;; -*- mode:lisp; package:lem -*-

(in-package :lem)

(export '(set-attr
          get-attr
          make-attr))

(defvar *attribute-name-table* (make-hash-table))

(defun get-attr (name)
  (check-type name symbol)
  (gethash name *attribute-name-table*))

(defun set-attr (name attr)
  (unless (integerp attr)
    (setq attr (get-attr name)))
  (check-type attr integer)
  (setf (gethash name *attribute-name-table*) attr))

(defun make-attr (&key color reverse-p bold-p underline-p)
  (logior (or (get-attr color) 0)
          (if reverse-p
              charms/ll:a_reverse
              0)
          (if bold-p
              charms/ll:a_bold
              0)
          (if underline-p
              charms/ll:a_underline
              0)))
