
(uiop:define-package :lem/porcelain
  (:use :cl)
  (:shadow :push)
  (:import-from :trivial-types
                :proper-list)
  (:export
   :porcelain-error
   :apply-patch
   :branches
   :checkout
   :checkout-create
   :commit
   :components
   :current-branch
   :discard-file
   :file-diff
   :latest-commits
   :pull
   :push
   :rebase-abort
   :rebase-continue
   :rebase-skip
   :show-commit-diff
   :stage
   :unstage
   :*diff-context-lines*
   :commits-log
   :*commits-log-page-size*
   :commit-count
   :*nb-latest-commits*)
  (:documentation "Functions to run VCS operations: get the list of changes, of untracked files, commit, push… Git support is the main goal, a simple layer is used with other VCS systems (Fossil, Mercurial).

On interactive commands, Legit will check what VCS is in use in the current project.

When multiple VCS are used at the same time in a project, Git takes
precedence by default. See `lem/porcelain:*vcs-existence-order*`."))

(in-package :lem/porcelain)

#|
Supported version control systems:
- Git: main support
- Fossil: preliminary support
- Mercurial: preliminary support

TODOs:

Mercurial:

- add (stage) diff hunks

|#

;;; Global variables, for all VCS.
(defvar *nb-latest-commits* 10
  "Number of commits to show in the status.")

(defvar *commits-log-page-size* 200
  "Number of commits to show in the commits log.")

(defvar *diff-context-lines* 4
  "How many lines of context before/after the first committed line")

(define-condition porcelain-condition (simple-error)
  ())

(define-condition porcelain-error (porcelain-condition)
  ((message
    :initform ""
    :initarg :message
    :type :string))
  (:report
   (lambda (condition stream)
     (with-slots (message) condition
       (princ message stream)))))

(defun porcelain-error (message &rest args)
  (error 'porcelain-error :message (apply #'format nil message args)))

;;;
;;; Getting changes.
;;;
(defgeneric components (vcs)
  (:documentation "Returns three values: TODO document what these are"))

;;;
;;; diff
;;;
(defgeneric file-diff (vcs file &key cached)
  (:documentation "TODO Document: presumably, returns the string form of the diff"))

;;;
;;; Show commits.
;;;
(defgeneric show-commit-diff (vcs ref &key ignore-all-space))

;; commit
(defgeneric commit (vcs message)
  (:documentation "Performs a commit operation: `message` must be a string."))

;; branches
(defun branches (&key (sort-by *branch-sort-by*))
  (loop for branch in (git-list-branches :sort-by sort-by)
        collect (subseq branch 2 (length branch))))

(defgeneric current-branch (vcs)
  (:documentation "Return the current branch name (string)."))

(defgeneric rebase-in-progress (vcs)
  (:documentation 
   "Return a plist if a rebase is in progress. Used for legit-status.

  plist keys:

  :status (boolean) -> T if a rebase is in progress
  :head-name -> content from .git/rebase-merge/head-name, such as \"refs/heads/master\"
  :head-short-name -> \"master\"
  :onto -> content from .git/rebase-merge/onto, a commit id."))
(defmethod rebase-in-progress (vcs)
  (log:info "rebase not available for ~a" vcs)
  (values))

;;;
;;; Latest commits.
;;;
(defgeneric latest-commits (vcs &key n hash-length offset)
  (:documentation "Return a list of strings or plists.
  The plist has a :hash and a :message, or simply a :line."))

(defgeneric commits-log (vcs &key offset limit hash-length)
  (:documentation
   "Return a list of commits starting from the given offset.
   If a limit is not provided, it returns all commits after the offset."))

(defgeneric commit-count (vcs)
  (:documentation 
   "Returns: integer representing number of commits in the current branch."))

;; stage, add files.
(defgeneric stage (vcs file)
  (:documentation "Stage changes to a file(TODO document type)"))

(defgeneric unstage (vcs file)
  (:documentation 
   "Unstage changes to this file. The reverse of \"add\"."))
(defmethod unstage (vcs file)
  (porcelain-error "VCS does not support or legit does not implement unstage: ~a" vcs))
#|
Interestingly, this returns the list of unstaged changes:
"Unstaged changes after reset:
M	src/ext/legit.lisp
M	src/ext/peek-legit.lisp
M	src/ext/porcelain.lisp
""
|#

;; discard changes.
(defgeneric discard-file (vcs file)
  (:documentation "Discard all changes to this file"))
(defmethod discard-file (vcs file)
  (porcelain-error "discard-file is not implemented for this VCS: ~a" vcs))

(defgeneric apply-patch (vcs diff &key reverse)
  (:documentation 
   "Apply a patch from this diff.
  diff: string."))

(defun checkout (branch)
  (run-git (list "checkout" branch)))

(defun checkout-create (new start)
  (run-git (list "checkout" "-b" new start)))

(defun pull ()
  ;; xxx: recurse submodules, etc.
  (run-git (list "pull" "HEAD")))

(defun push (&rest args)
  (when args
    (porcelain-error "Our git push command doesn't accept args. Did you mean cl:push ?!!"))
  (run-git (list "push")))

;; Interactive rebase
(defgeneric rebase-interactively (vcs &key from))

(defgeneric rebase-continue (vcs)
  (:documentation
   "Either send a continuation signal to the underlying git rebase process, for it to pick up our changes to the interactive rebase file,
  either call git rebase --continue."))
(defmethod rebase-continue (vcs)
  (porcelain-error "Rebasing is not supported for ~a" vcs))

(defgeneric rebase-abort (vcs))
(defmethod rebase-abort (vcs)
  (porcelain-error "Rebasing is not supported for ~a" vcs))

(defgeneric rebase-skip (vcs))
(defmethod rebase-skip (vcs)
  (porcelain-error "Rebasing is not supported for ~a" vcs))