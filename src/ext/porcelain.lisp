
(defpackage :porcelain
  (:use :cl)
  (:shadow :push)
  (:export))
(in-package :porcelain)

(declaim (type (list) *git-base-arglist*))
(defvar *git-base-arglist* (list "git")
  "The git program, to be appended command-line options.")

(declaim (type (list) *fossil-base-args*))
(defvar *fossil-base-args* (list "fossil")
  "fossil, to be appended command-line options.")

(defvar *nb-latest-commits* 10
  "Number of commits to show in the status.")

(defvar *branch-sort-by* "-creatordate"
  "When listing branches, sort by this field name.
  Prefix with \"-\" to sort in descending order.

  Defaults to \"-creatordate\", to list the latest used branches first.")

(defvar *file-diff-args* (list "diff"
                               "--no-color")
  "Arguments to display the file diffs.
  Will be surrounded by the git binary and the file path.

  For staged files, --cached is added by the command.")

(defun git-project-p ()
  (uiop:directory-exists-p ".git"))

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

(defun git-list-branches (&key (sort-by *branch-sort-by*))
  "Return: list of branches, raw output.
  Each element starts with two meaningful characters, such as \"* \" for the current branch."
  (str:lines
   (uiop:run-program (list "git"
                           "branch"
                           "--list"
                           "--no-color"
                           (format nil "--sort=~a" sort-by))
                     :output :string
                     :error-output :string)))

(defun branches (&key (sort-by *branch-sort-by*))
  (loop for branch in (git-list-branches :sort-by sort-by)
        collect (subseq branch 2 (length branch))))

(defun current-branch ()
  (let ((branches (git-list-branches :sort-by "-creatordate")))
    (loop for branch in branches
          if (str:starts-with-p "*" branch)
            return (subseq branch 2))))

(defun list-latest-commits (&optional (n *nb-latest-commits*))
  (when (plusp n)
    (str:lines
     (uiop:run-program (list "git"
                             "log"
                             "--pretty=oneline"
                             "-n"
                             (princ-to-string n))
                       :output :string))))

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
  (run-git (list "reset" "HEAD" "--" file)))

#|
Interestingly, this returns the list of unstaged changes:
"Unstaged changes after reset:
M	src/ext/legit.lisp
M	src/ext/peek-legit.lisp
M	src/ext/porcelain.lisp
""
|#

(defun run-git (arglist)
  "Run the git command with these options (list).
  Return standard and error output as strings.

  Don't signal an error if the process doesn't exit successfully
  (but return the error message, so it can be displayed by the caller).
  However, if uiop:run-program fails to run the command, the interactive debugger
  is invoked (and shown correctly in Lem)."
  (uiop:run-program (concatenate 'list *git-base-arglist* arglist)
                    :output :string
                    :error-output :string
                    :ignore-error-status t))

(defun apply-patch (patch &key reverse)
  "Apply a patch file.
  This is used to stage hunks of files."
  (let ((base (list "apply"
                    "--ignore-space-change" ;; in context only.
                    "-C0" ;; easier to apply patch without context.
                    "--index"
                    "--cached"))
        (maybe (if reverse
                   (list "--reverse")
                   (list)))
        (arglist (list patch)))
    (run-git (concatenate 'list base maybe arglist))))

(defun checkout (branch)
  (run-git (list "checkout" branch)))

(defun checkout-create (new start)
  (run-git (list "checkout" "-b" new start)))

(defun pull ()
  ;; xxx: recurse submodules, etc.
  (run-git (list "pull" "HEAD")))

(defun push (&rest args)
  (when args
    (error "Our git push command doesn't accept args. Did you mean cl:push ?!!"))
  (run-git (list "push")))
