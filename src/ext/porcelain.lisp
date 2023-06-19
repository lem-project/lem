
(defpackage :porcelain
  (:use :cl)
  (:export))
(in-package :porcelain)

(defun porcelain ()
  (uiop:run-program (list "git" "status" "--porcelain=v1")
                    :output :string))

(defun components()
  "Return 3 values:
  - untracked files
  - modified and unstaged files
  - modified and stages files."
  (loop for line in (str:lines (porcelain))
        for file = (subseq line 3)
        unless (str:blankp line)
        if (equal (elt line 0) #\M)
          collect file into modified-staged-files
        if (equal (elt line 1) #\M)
          collect file into modified-unstaged-files
        if (str:starts-with-p "??" line)
          collect file into untracked-files
        finally (return (values untracked-files
                                modified-unstaged-files
                                modified-staged-files))))

(defun file-diff (file)
  (let ((out (uiop:run-program (list "git" "diff"
                                     "--no-color"
                                     file)
                               :output :string)))
    out))
