
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

(declaim (type (proper-list string) *hg-base-arglist*))
(defvar *hg-base-arglist* (list "hg")
  "The mercurial program (hg), to be appended command-line options.")

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

;; VCS implementation for hg
(defclass vcs-hg () ())

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

(defun hg-project-p ()
  "Return t if we find a .hg/ directory in the current directory (which should be the project root. Use `lem/porcelain:with-current-project`)."
  (when (uiop:directory-exists-p ".hg")
    (make-instance 'vcs-hg)))

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

(defun run-hg (arglist)
  "Run the mercurial command with these options (list).
  Return standard and error output as strings.

  For error handling strategy, see `run-git`."
  (uiop:run-program (concatenate 'list
                                 *hg-base-arglist*
                                 (uiop:ensure-list arglist)
                                 '("--pager" "never")   ;; no interactive pager.
                                 )
                    :output :string
                    :error-output :string
                    :ignore-error-status t))


;;;
;;; Getting changes.
;;;
(defgeneric components (vcs)
  (:documentation "Returns three values: TODO document what these are"))

(defun hg-porcelain ()
  "Return changes."
  (run-hg "status"))

(defmethod components ((vcs vcs-hg))
  "Return 3 values:
  - untracked files
  - modified and unstaged files
  - modified and staged files."
  ;; Mercurial manual:
  ;; The codes used to show the status of files are:
  ;;    M = modified
  ;;    A = added
  ;;    R = removed => difference with Git
  ;;    C = clean   => difference
  ;;    ! = missing (deleted by non-hg command, but still tracked)
  ;;    ? = not tracked
  ;;    I = ignored (=> not shown without -A)
  ;;      = origin of the previous file (with --copies)
  (loop for line in (str:lines (hg-porcelain))
        for file = (subseq line 2)  ;; a slight difference than with git.
        unless (str:blankp line)
          ;; Modified
          if (equal (elt line 0) #\M)
            collect (list :file file :type :modified) into modified-staged-files
        ;; Added
        if (equal (elt line 0) #\A)
          collect (list :file file :type :added) into modified-staged-files
        ;; Removed
        if (equal (elt line 0) #\R)
          collect (list :file file :type :deleted) into modified-staged-files
        ;; Modified (unstaged)
        if (or (equal (elt line 1) #\M) (equal (elt line 1) #\X))
          collect (list :file file :type :modified) into modified-unstaged-files
        ;; Untracked
        if (str:starts-with-p "?" line)
          collect file into untracked-files
        ;; Missing (deleted)
        if (str:starts-with-p "!" line)
          collect (list :file file :type :deleted) into modified-unstaged-files
        finally (return (values untracked-files
                                modified-unstaged-files
                                modified-staged-files))))

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

(defmethod file-diff ((vcs vcs-hg) file &key cached)
  "Show the diff of staged files (and only them)."
  (when cached
    (run-hg (list "diff"
                  (format nil "-U~D" *diff-context-lines*)
                  file))
    ;; files not staged can't be diffed.
    ;; We could read and display their full content anyways?
    ))

(defmethod file-diff ((vcs vcs-fossil) file &key cached)
  (declare (ignorable cached))
  (run-fossil (list "diff" file)))

;;;
;;; Show commits.
;;;
(defgeneric show-commit-diff (vcs ref &key ignore-all-space))

(defmethod show-commit-diff ((vcs vcs-hg) ref &key ignore-all-space)
  (declare (ignore ignore-all-space))
  (run-hg (list "log" "-r" ref "-p")))

(defmethod show-commit-diff ((vcs vcs-fossil) ref &key ignore-all-space)
  (declare (ignore vcs)
           (ignore ref)
           (ignore ignore-all-space))
  nil)

;; commit
(defgeneric commit (vcs message)
  (:documentation "Performs a commit operation: `message` must be a string."))

(defmethod commit ((vcs vcs-hg) message)
  (run-hg (list "commit" "-m" message)))

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

(defmethod current-branch ((vcs vcs-hg))
  "Return the current branch name."
  (str:trim
   (run-hg "branch")))

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

(defmethod latest-commits ((vcs vcs-hg) &key (n *nb-latest-commits*) (hash-length 8) (offset 0))
  (declare (ignorable n hash-length offset))
  (let ((out (run-hg "log")))
    ;; Split by paragraph.
        #| $ hg log
changeset:   1:c20c766359d3
user:        user@machine
date:        Mon Oct 02 23:01:32 2023 +0200
summary:     second line

changeset:   0:b27dda897ba8
user:        user@machine
date:        Mon Oct 02 22:51:57 2023 +0200
summary:     test

|#
    (loop for paragraph in (ppcre:split "\\n\\n" out)
          collect
          (loop for line in (str:lines (str:trim paragraph))
                with entry = (list :line ""
                                   :message ""
                                   :hash "")
                for (key %val) = (str:split ":" line :limit 2)
                for val = (str:trim %val)
                for changeset = ""
                for tag = ""
                for user = ""
                for date = ""
                for summary = ""
                do
                   (cond
                     ;; we can use str:string-case with a recent enough quicklisp dist > July 2023 (PR 103)
                     ((equal key "changeset")
                      (setf changeset val)
                      (setf (getf entry :changeset) val)
                      (setf (getf entry :hash) val))
                     ((equal key "tag")
                      (setf tag val)
                      (setf (getf entry :tag) val))
                     ((equal key "user")
                      (setf user val)
                      (setf (getf entry :user) val))
                     ((equal key "date")
                      (setf date val)
                      (setf (getf entry :date) val))
                     ((equal key "summary")
                      (setf summary (str:trim val))
                      (setf (getf entry :summary) val)
                      (setf (getf entry :message)
                            (str:concat " " val))
                      (setf (getf entry :line)
                            (str:concat changeset " " summary))))
                finally (return entry)))))


(defmethod latest-commits ((vcs vcs-fossil) &key (n *nb-latest-commits*) (hash-length 8) (offset 0))
  (declare (ignorable n hash-length offset))
  ;; return bare result.
  (str:lines (run-fossil "timeline")))

(defgeneric commits-log (vcs &key offset limit hash-length)
  (:documentation
   "Return a list of commits starting from the given offset.
   If a limit is not provided, it returns all commits after the offset."))

(defmethod commits-log ((vcs vcs-hg) &key (offset 0) limit (hash-length 8))
  (declare (ignorable hash-length))
  (let* ((commits (latest-commits vcs))
         (total-commits (length commits))
         (end (when limit
                (min total-commits (+ offset limit)))))
    (if (>= offset total-commits)
        nil
        (subseq commits offset end))))

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

(defmethod commit-count ((vcs vcs-hg))
  (parse-integer
   (str:trim (run-hg '("id" "--num" "--rev" "tip")))))

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

(defmethod stage ((vcs vcs-hg) file)
  (run-hg (list "add" file)))

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

(defmethod apply-patch ((vcs vcs-hg) diff &key reverse)
  (declare (ignorable diff reverse))
  (porcelain-error "applying patch not yet implemented for Mercurial"))

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

(defmethod rebase-interactively ((vcs vcs-hg) &key from)
  (declare (ignore from))
  (porcelain-error "Interactive rebase not implemented for Mercurial"))

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