(defpackage :lem-lsp-mode/utils
  (:use :cl :alexandria)
  (:import-from :quri)
  (:import-from :alexandria)
  (:import-from :trivia)
  (:export :find-root-pathname
           :elt-clamp))
(in-package :lem-lsp-mode/utils)

(defun find-root-pathname (directory root-test-function)
  (cond ((dolist (file (uiop:directory-files directory))
           (when (funcall root-test-function file)
             (return directory))))
        ((uiop:pathname-equal directory (user-homedir-pathname)) nil)
        ((find-root-pathname (uiop:pathname-parent-directory-pathname directory) root-test-function))))

(defun elt-clamp (elements index)
  (elt elements (clamp index 0 (1- (length elements)))))
