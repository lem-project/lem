
(defpackage :porcelain
  (:use :cl)
  (:export))
(in-package :porcelain)

(defun status ()
  (let ((porcelain (uiop:run-program (list "git" "status" "--porcelain=v1")
                                     :output :string)))
    (str:trim porcelain)))

(defun components()
  (let ((porcelain (status)))
    (loop for line in (str:lines porcelain)
          if (str:starts-with-p "M" (str:trim line))
            collect (str:trim (subseq line 2)) into modified-files
          if (str:starts-with-p "??" (str:trim line))
            collect (str:trim (subseq line 3)) into untracked-files
          finally (return (values modified-files
                                  untracked-files)))))

(defun file-diff (file)
  (let ((out (uiop:run-program (list "git" "diff"
                                     "--no-color"
                                     file)
                               :output :string)))
    out))
