(defpackage :lem-winsize
  (:use :cl :cffi)
  (:export :win-row :win-col :win-size))

(in-package :lem-winsize)

(define-foreign-library libwinsize
  (:unix "winsize.so"))

(defun load-directory ()
  (let ((here #.(or *compile-file-truename* *load-truename*)))
    (make-pathname :directory (pathname-directory here))))

(let ((*foreign-library-directories* (list (load-directory))))
  (load-foreign-library 'libwinsize))

(defcfun "win_row" :int (fd :int))
(defcfun "win_col" :int (fd :int))

(defun win-size (fd)
  (list (win-row fd)
        (win-col fd)))
