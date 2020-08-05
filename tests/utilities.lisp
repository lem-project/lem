(defpackage :lem-tests/utilities
  (:use :cl)
  (:export :sample-file))
(in-package :lem-tests/utilities)

(defun sample-file (filename)
  (asdf:system-relative-pathname :lem (merge-pathnames filename "tests/sample-code/")))
