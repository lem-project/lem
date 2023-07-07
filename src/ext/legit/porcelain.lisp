
(defpackage :lem/porcelain
  (:use :cl)
  (:shadow :push)
  (:export
   :*vcs*
   :apply-patch
   :branches
   :checkout
   :checkout-create
   :commit
   :components
   :current-branch
   :file-diff
   :latest-commits
   :pull
   :push
   :show-commit-diff
   :stage
   :unstage
   :vcs-project-p
   )
  (:documentation "Functions to run Git operations: get the list of changes, of untracked files, commit, push… A simple dispatch mechanism is used to use other VCS systems. In the caller, bind the `*VCS*` variable. See `legit:with-current-project`."))

(in-package :lem/porcelain)

(declaim (type (cons) *git-base-arglist*))
(defvar *git-base-arglist* (list "git")
  "The git program, to be appended command-line options.")

(declaim (type (cons) *fossil-base-args*))
(defvar *fossil-base-args* (list "fossil")
  "fossil, to be appended command-line options.")

(defvar *nb-latest-commits* 10
  "Number of commits to show in the status.")

(defvar *branch-sort-by* "-creatordate"
  "When listing branches, sort by this field name.
  Prefix with \"-\" to sort in descending order.

  Defaults to \"-creatordate\", to list the latest used branches first.")

(defvar *file-diff-args* (list "diff" "--no-color")
  "Arguments to display the file diffs.
  Will be surrounded by the git binary and the file path.

  For staged files, --cached is added by the command.")

(defvar *vcs* nil
  "git, fossil? For current project. Bind this in the caller.

  For instance, see the legit::with-current-project macro that lexically binds *vcs* for an operation.")

(defun git-project-p ()
  (values (uiop:directory-exists-p ".git")
          :git))

(defun fossil-project-p ()
  (cond
    ((uiop:file-exists-p ".fslckout")
     (values ".fslckout" :fossil))
    (t
     ;; Maybe do we need "fossil open" here.
     (loop for file in (uiop:directory-files "./")
           if (equal "fossil" (pathname-type file))
             return (values file :fossil)))))

(defvar *vcs-existence-order*
  '(
    git-project-p
    fossil-project-p
    ))

(defun vcs-project-p ()
  "If this project under a known version control system?
  Return two values: the project root (pathname), the VCS (:git or :fossil)."
  ;; This doesn't return the 2 values :(
  ;; (or (fossil-project-p)
  ;;     (git-project-p))
  (loop for fn in *vcs-existence-order*
        do (multiple-value-bind (root vcs)
               (funcall fn)
             (if root
                 (return (values root vcs))))))

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

(defun run-fossil (arglist)
  "Run the fossil command with these options.

   arglist: a list of CLI options and arguments to append to the base fossil program.
   See `*fossil-base-args*`."
  (uiop:run-program (concatenate 'list
                                 *fossil-base-args*
                                 (uiop:ensure-list arglist))
                    :output :string
                    :error-output :string
                    :ignore-error-status t))


(defun git-porcelain ()
  (run-git (list "status" "--porcelain=v1")))

(defun fossil-porcelain ()
  (multiple-value-bind (out error code)
      (run-fossil "changes")
    (cond
      ((not (zerop code))
       (error (str:join #\newline (list out error))))
      (t
       (values out error)))))

;; Dispatching on the right VCS:
;; *vcs* is bound in the caller (in legit::with-current-project).
(defun porcelain ()
  (case *vcs*
    (:fossil (fossil-porcelain))
    (t (git-porcelain))))

(defun git-components()
  "Return 3 values:
  - untracked files
  - modified and unstaged files
  - modified and stages files."
  (loop for line in (str:lines (porcelain))
        for file = (subseq line 3)
        unless (str:blankp line)
          if (equal (elt line 0) #\M)
            collect file into modified-staged-files
        if (equal (elt line 0) #\A)
          ;; Here we don't differentiate between modified and newly added.
          ;; We could do better.
          collect file into modified-staged-files
        if (equal (elt line 1) #\M)
          collect file into modified-unstaged-files
        if (str:starts-with-p "??" line)
          collect file into untracked-files
        finally (return (values untracked-files
                                modified-unstaged-files
                                modified-staged-files))))

(defun fossil-components ()
  "Return values:
  - untracked files (todo)
  - list of ADDED files
  - modified files"
  (loop for line in (str:lines (fossil-porcelain))
        for parts = (str:words line)
        for status = (first parts)
        for file = (second parts)
        if (equal "ADDED" status)
          collect file into added-files
        if (equal "EDITED" status)
          collect file into edited-files
        finally (return (values nil
                                added-files
                                edited-files))))

(defun components ()
  (case *vcs*
    (:fossil (fossil-components))
    (t (git-components))))

;; diff
(defun git-file-diff (file &key cached)
  (run-git
   (concatenate 'list
                *file-diff-args*
                (if cached '("--cached"))
                (list file))))

(defun fossil-file-diff (file &key cached)
  (declare (ignorable cached))
  (run-fossil (list "diff" file)))

(defun file-diff (file &key cached)
  (case *vcs*
    (:fossil
     (fossil-file-diff file))
    (t
     (git-file-diff file :cached cached))))

;; show commits.
(defun git-show-commit-diff (ref)
  (run-git (list "show" ref)))

(defun show-commit-diff (ref)
  (case *vcs*
    (:fossil nil)
    (t (git-show-commit-diff ref))))

;; commit
(defun git-commit (message)
  (run-git (list "commit" "-m" message)))

(defun fossil-commit (message)
  (run-fossil (list "commit" "-m" message)))

(defun commit (message)
  (case *vcs*
    (:fossil (fossil-commit message))
    (t (git-commit message))))

;; branches
(defun git-list-branches (&key (sort-by *branch-sort-by*))
  "Return: list of branches, raw output.
  Each element starts with two meaningful characters, such as \"* \" for the current branch."
  (str:lines
   (run-git (list "branch"
                  "--list"
                  "--no-color"
                  (format nil "--sort=~a" sort-by)))))

(defun branches (&key (sort-by *branch-sort-by*))
  (loop for branch in (git-list-branches :sort-by sort-by)
        collect (subseq branch 2 (length branch))))

(defun fossil-branches (&key &allow-other-keys)
  (error "not implemented"))

(defun git-current-branch ()
  (let ((branches (git-list-branches :sort-by "-creatordate")))
    (loop for branch in branches
          if (str:starts-with-p "*" branch)
            return (subseq branch 2))))

(defun fossil-current-branch ()
  ;; strip out "* " from "* trunk"
  (str:trim
   (subseq (run-fossil "branch") 2)))

(defun current-branch ()
  (case *vcs*
    (:fossil
     (fossil-current-branch))
    (t
     (git-current-branch))))

;; latest commits.
(defun %git-list-latest-commits (&optional (n *nb-latest-commits*))
  (when (plusp n)
    (str:lines
     (run-git (list "log" "--pretty=oneline" "-n" (princ-to-string n))))))

(defun git-latest-commits (&key (hash-length 8))
  (let ((lines (%git-list-latest-commits)))
    (loop for line in lines
          for space-position = (position #\space line)
          for small-hash = (subseq line 0 hash-length)
          for message = (subseq line space-position)
          collect (list :line (concatenate 'string small-hash message)
                        :message message
                        :hash small-hash))))

(defun fossil-latest-commits (&key &allow-other-keys)
  ;; return bare result.
  (str:lines (run-fossil "timeline")))

(defun latest-commits (&key (hash-length 8))
  "Return a list of strings or plists.
  The plist has a :hash and a :message, or simply a :line."
  (case *vcs*
    (:fossil
     (fossil-latest-commits))
    (t
     (git-latest-commits :hash-length hash-length))))

;; stage, add files.
(defun git-stage (file)
  (run-git (list "add" file)))

(defun fossil-stage (file)
  (run-fossil (list "add" file)))

(defun stage (file)
  (case *vcs*
    (:fossil (fossil-stage file))
    (t (git-stage file))))

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

(defvar *verbose* nil)

(defun git-apply-patch (diff &key reverse)
  "Apply a patch file.
  This is used to stage hunks of files."
  (when *verbose*
    (log:info diff)
    (with-open-file (f (merge-pathnames "lem-hunk-latest.patch" (lem-home))
                       :direction :output
                       :if-exists :supersede
                       :if-does-not-exist :create)
      (write-sequence diff f)))

  ;; Write diff on file.
  ;; It should also be possible to give it on stdin ("git apply - ")
  ;; (uiop:with-temporary-file (:stream f) ;; issues with this.
  (with-open-file (f ".lem-hunk.patch"
                     :direction :output
                     :if-exists :supersede
                     :if-does-not-exist :create)
    (write-sequence diff f)
    (finish-output f))

  (let ((base (list "apply"
                    "--ignore-space-change" ;; in context only.
                    "-C0" ;; easier to apply patch without context.
                    "--index"
                    "--cached"))
        (maybe (if reverse
                   (list "--reverse")
                   (list)))
        (arglist (list ".lem-hunk.patch")))
    (run-git (concatenate 'list base maybe arglist))))

(defun fossil-apply-patch (diff &key reverse)
  "Needs fossil at least > 2.10. Version 2.22 works."
  (declare (ignorable diff reverse))
  ;; mmh… it wants a binary patch.
  (values nil "fossil patch is not supported." 1))

(defun apply-patch (diff &key reverse)
  "Apply a patch from this diff.
  diff: string."
  (case *vcs*
    (:fossil (fossil-apply-patch diff :reverse reverse))
    (t (git-apply-patch diff :reverse reverse))))

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