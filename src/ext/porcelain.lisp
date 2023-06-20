
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

(defparameter *file-diff-args* (list "diff"
                                   "--no-color")
  "Must be surrounded by the git binary and the file path.

  For staged files, add --cached.")

(defun file-diff (file &key cached)
  (let ((out (uiop:run-program
              (concatenate 'list '("git")
                           *file-diff-args*
                           (if cached '("--cached"))
                           (list file))
              :output :string)))
    out))

(defun commit (message)
  (log:info "commiting: " message)
  (uiop:run-program (list "git"
                          "commit"
                          "-m"
                          message)))

(defun list-branches ()
  (str:lines
   (uiop:run-program (list "git"
                           "branch"
                           "--list"
                           "--no-color")
                     :output :string)))

(defun current-branch ()
  (let ((branches (list-branches)))
    (loop for branch in branches
          if (str:starts-with-p "*" branch)
            return (subseq branch 2))))
