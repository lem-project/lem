
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

(defun list-latest-commits (&optional (n 10))
  (str:lines
   (uiop:run-program (list "git"
                           "log"
                           "--pretty=oneline"
                           "-n"
                           (princ-to-string n))
                     :output :string)))

(defun latest-commits (&key (hash-length 8))
  (let ((lines (list-latest-commits)))
    (loop for line in lines
          for space-position = (position #\space line)
          for small-hash = (subseq line 0 hash-length)
          for message = (subseq line space-position)
          collect (concatenate 'string small-hash message))))

(defun stage (file)
  (uiop:run-program (list "git" "add" file)))

(defun unstage (file)
  "Unstage changes to a file."
  ;; test with me: this hunk was added with Lem!
  (uiop:run-program (list "git"
                          "reset"
                          "HEAD"
                          "--"
                          file)
                    :output :string))
#|
Interestingly, this returns the list of unstaged changes:
"Unstaged changes after reset:
M	src/ext/legit.lisp
M	src/ext/peek-legit.lisp
M	src/ext/porcelain.lisp
""
|#

(defun apply-patch (patch)
  "Apply a patch file.
  This is used to stage hunks of files."
  (uiop:run-program (list "git"
                          "apply"
                          "--ignore-space-change"  ;; in context only.
                          "-C0"  ;; easier to apply patch without context.
                          "--index"
                          "--cached"
                          patch)
                    :output :string
                    :error-output t))
