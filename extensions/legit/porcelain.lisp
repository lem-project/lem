
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
   :vcs-project-p
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

(declaim (type (proper-list string) *fossil-base-args*))
(defvar *fossil-base-args* (list "fossil")
  "The fossil program, to be appended command-line options.")

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

;; VCS implementation for fossil
(defclass vcs-fossil () ())

(defun fossil-project-p ()
  "Return t if we either find a .fslckout file in the current directory (should be the project root) or a file with a .fossil extension."
  (cond
    ((uiop:file-exists-p ".fslckout")
     (make-instance 'vcs-fossil))
    (t
     ;; Maybe do we need "fossil open" here.
     (loop for file in (uiop:directory-files "./")
           if (equal "fossil" (pathname-type file))
           return (make-instance 'vcs-fossil)))))

(defvar *vcs-existence-order*
  '(
    git-project-p
    fossil-project-p
    hg-project-p
    ))

(defun vcs-project-p ()
  "When this project is under a known version control system, returns a VCS object for the project.
   Otherwise, returns nil."
  ;; This doesn't return the 2 values :(
  ;; (or (fossil-project-p)
  ;;     (git-project-p))
  (loop for fn in *vcs-existence-order*
        do (alexandria:if-let (vcs (funcall fn))
             (return vcs))))

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

;;;
;;; Getting changes.
;;;
(defgeneric components (vcs)
  (:documentation "Returns three values: TODO document what these are"))

(defun fossil-porcelain ()
  "Get changes."
  (multiple-value-bind (out error code)
      (run-fossil "changes")
    (cond
      ((not (zerop code))
       (porcelain-error (str:join #\newline (list out error))))
      (t
       (values out error)))))

(defmethod components ((vcs vcs-fossil))
  "Return values:
  - untracked files (todo)
  - list of ADDED files
  - modified files"
  (loop for line in (str:lines (fossil-porcelain))
        for parts = (str:words line)
        for status = (first parts)
        for file = (second parts)
        if (equal "ADDED" status)
          collect (list :file file :type :added) into modified-staged-files
        if (equal "EDITED" status)
          collect (list :file file :type :modified) into modified-staged-files
        if (equal "DELETED" status)
          collect (list :file file :type :deleted) into modified-staged-files
        if (equal "UNKNOWN" status)
          collect file into untracked-files
        finally (return (values untracked-files
                                nil
                                modified-staged-files))))

;;;
;;; diff
;;;
(defgeneric file-diff (vcs file &key cached)
  (:documentation "TODO Document: presumably, returns the string form of the diff"))

(defmethod file-diff ((vcs vcs-fossil) file &key cached)
  (declare (ignorable cached))
  (run-fossil (list "diff" file)))

;;;
;;; Show commits.
;;;
(defgeneric show-commit-diff (vcs ref &key ignore-all-space))

(defmethod show-commit-diff ((vcs vcs-fossil) ref &key ignore-all-space)
  (declare (ignore vcs)
           (ignore ref)
           (ignore ignore-all-space))
  nil)

;; commit
(defgeneric commit (vcs message)
  (:documentation "Performs a commit operation: `message` must be a string."))

(defmethod commit ((vcs vcs-fossil) message)
  (run-fossil (list "commit" "-m" message)))

;; branches
(defun branches (&key (sort-by *branch-sort-by*))
  (loop for branch in (git-list-branches :sort-by sort-by)
        collect (subseq branch 2 (length branch))))

(defun fossil-branches (&key &allow-other-keys)
  (porcelain-error "not implemented"))


(defgeneric current-branch (vcs)
  (:documentation "Return the current branch name (string)."))

(defmethod current-branch ((vcs vcs-fossil))
  ;; strip out "* " from "* trunk"
  (str:trim
   (subseq (run-fossil "branch") 2)))

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

(defmethod latest-commits ((vcs vcs-fossil) &key (n *nb-latest-commits*) (hash-length 8) (offset 0))
  (declare (ignorable n hash-length offset))
  ;; return bare result.
  (str:lines (run-fossil "timeline")))

(defgeneric commits-log (vcs &key offset limit hash-length)
  (:documentation
   "Return a list of commits starting from the given offset.
   If a limit is not provided, it returns all commits after the offset."))

(defmethod commits-log ((vcs vcs-fossil) &key (offset 0) limit (hash-length 8))
  (declare (ignorable hash-length))
  (let* ((commits (latest-commits vcs))
         (total-commits (length commits))
         (end (when limit (min total-commits (+ offset limit)))))
    (if (>= offset total-commits)
        nil
        (subseq commits offset end))))

(defgeneric commit-count (vcs)
  (:documentation 
   "Returns: integer representing number of commits in the current branch."))

(defun %not-fossil-commit-line (line)
  (str:starts-with-p "+++ no more data" line))

(defun fossil-commit-count ()
  ;; Not really tested in Lem.
  (length
   ;; Does the timeline command always end with "+++ no more data (1) +++" ?
   (remove-if #'%not-fossil-commit-line
              (str:lines
               (run-fossil (list "timeline" "--oneline"))))))

(defmethod commit-count ((vcs vcs-fossil))
  (fossil-commit-count))

;; stage, add files.
(defgeneric stage (vcs file)
  (:documentation "Stage changes to a file(TODO document type)"))

(defmethod stage ((vcs vcs-fossil) file)
  (run-fossil (list "add" file)))

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

(defmethod apply-patch ((vcs vcs-fossil) diff &key reverse)
  "Needs fossil at least > 2.10. Version 2.22 works."
  (declare (ignorable diff reverse))
  ;; mmh… it wants a binary patch.
  (values nil "fossil patch is not supported." 1))

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

(defmethod rebase-interactively ((vcs vcs-fossil) &key from)
  (declare (ignore from))
  (porcelain-error "No interactive rebase for Fossil."))

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