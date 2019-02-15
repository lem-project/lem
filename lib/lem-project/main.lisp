(defpackage #:lem-project/main
  (:nicknames #:lem-project)
  (:import-from #:cl-project)
  (:use #:cl)
  (:export #:make-project))
(in-package #:lem-project/main)

(defvar *skeleton-directory*
  (asdf:system-relative-pathname :lem-project #p"skeleton/"))

(defun make-project (path &rest args)
  (let ((cl-project:*skeleton-directory* *skeleton-directory*))
    (apply #'cl-project:make-project path args)))
