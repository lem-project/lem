
(defpackage :lem/porcelain
  (:use :cl)
  (:shadow :push)
  (:import-from :trivial-types
                :proper-list)
  (:export
   :*vcs*
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
   )
  (:documentation "Functions to run VCS operations: get the list of changes, of untracked files, commit, push… Git support is the main goal, a simple layer is used with other VCS systems (Fossil, Mercurial).

On interactive commands, Legit will check what VCS is in use in the current project.
When used programmatically, bind the `lem/porcelain:*vcs*` variable in your caller. See `lem/legit:with-current-project`.

When multiple VCS are used at the same time in a project, Git takes
precedence by default. See `lem/porcelain:*vcs-existence-order*`."))

(in-package :lem/porcelain)

#|
Supported version control systems:
- Git: main support
- Fossil: preliminary support
- Mercurial: preliminary support

TODOs:

- show missing files.

Mercurial:

- add (stage) diff hunks

|#

(declaim (type (proper-list string) *git-base-arglist*))
(defvar *git-base-arglist* (list "git")
  "The git program, to be appended command-line options.")

(declaim (type (proper-list string) *fossil-base-args*))
(defvar *fossil-base-args* (list "fossil")
  "The fossil program, to be appended command-line options.")

(declaim (type (proper-list string) *hg-base-arglist*))
(defvar *hg-base-arglist* (list "hg")
  "The mercurial program (hg), to be appended command-line options.")

;;; Global variables, for all VCS.
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


(defun git-project-p ()
  "Return t if we find a .git/ directory in the current directory (which should be the project root. Use `lem/legit::with-current-project`)."
  (values (uiop:directory-exists-p ".git")
          :git))

(defun fossil-project-p ()
  "Return t if we either find a .fslckout file in the current directory (should be the project root) or a file with a .fossil extension."
  (cond
    ((uiop:file-exists-p ".fslckout")
     (values ".fslckout" :fossil))
    (t
     ;; Maybe do we need "fossil open" here.
     (loop for file in (uiop:directory-files "./")
           if (equal "fossil" (pathname-type file))
             return (values file :fossil)))))

(defun hg-project-p ()
  "Return t if we find a .hg/ directory in the current directory (which should be the project root. Use `lem/legit::with-current-project`)."
  (values (uiop:directory-exists-p ".hg")
          :hg))

(defvar *vcs-existence-order*
  '(
    git-project-p
    fossil-project-p
    hg-project-p
    ))

(defun vcs-project-p ()
  "Is this project under a known version control system?
  Return two values: the current directory (pathname), which is then considered the project root, and the VCS found (:git, :fossil or :hg)."
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
  (uiop:run-program (concatenate 'list
                                 *git-base-arglist*
                                 (uiop:ensure-list arglist))
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

(defun git-porcelain ()
  "Return the git status: for each file in the project, the `git status --porcelain` command
allows to learn about the file state: modified, deleted, ignored… "
  (run-git (list "status" "--porcelain=v1")))

(defun hg-porcelain ()
  "Return changes."
  (run-hg "status"))

(defun fossil-porcelain ()
  "Get changes."
  (multiple-value-bind (out error code)
      (run-fossil "changes")
    (cond
      ((not (zerop code))
       (porcelain-error (str:join #\newline (list out error))))
      (t
       (values out error)))))

;; Dispatching on the right VCS:
;; *vcs* is bound in the caller (in legit::with-current-project).
(defun porcelain ()
  "Dispatch on the current `*vcs*` and get current changes (added, modified files…)."
  (case *vcs*
    (:git (git-porcelain))
    (:fossil (fossil-porcelain))
    (:hg (hg-porcelain))
    (t (porcelain-error "VCS not supported: ~a" *vcs*))))

(defun git-components()
  "Return 3 values:
  - untracked files
  - modified and unstaged files
  - modified and stages files.

   Git manual:

   In the short-format, the status of each path is shown as one of these forms
           XY PATH
           XY ORIG_PATH -> PATH
   For paths with merge conflicts, X and Y show the modification states of each side of the
       merge. For paths that do not have merge conflicts, X shows the status of the index, and Y
       shows the status of the work tree. For untracked paths, XY are ??. Other status codes can
       be interpreted as follows:
       •   ' ' = unmodified
       •   M = modified
       •   A = added
       •   D = deleted
       •   R = renamed
       •   C = copied
       •   U = updated but unmerged"
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

(defun hg-components ()
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
            collect file into modified-staged-files
        ;; Added
        if (equal (elt line 0) #\A)
          ;; is that enough?
          collect file into modified-staged-files
        ;; Modified
        if (equal (elt line 1) #\X)  ;?
          collect file into modified-unstaged-files
        ;; Untracked
        if (str:starts-with-p "?" line)
          collect file into untracked-files
        ;; Missing (deleted)
        if (str:starts-with-p "!" line)
          collect file into missing-files
        finally (return (values untracked-files
                                modified-unstaged-files
                                modified-staged-files
                                missing-files))))
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
    (:git (git-components))
    (:fossil (fossil-components))
    (:hg (hg-components))
    (t (porcelain-error "VCS not supported: ~a" *vcs*))))


;;;
;;; diff
;;;
(defun git-file-diff (file &key cached)
  ;; --cached is a synonym for --staged.
  ;; So it is set only for staged files. From git-components: the 3rd value, modified and staged files.
  (run-git
   (concatenate 'list
                *file-diff-args*
                (if cached '("--cached"))
                (list file))))

(defun hg-file-diff (file &key cached)
  "Show the diff of staged files (and only them)."
  (when cached
      (run-hg (list "diff" file))
      ;; files not staged can't be diffed.
      ;; We could read and display their full content anyways?
      ))

(defun fossil-file-diff (file &key cached)
  (declare (ignorable cached))
  (run-fossil (list "diff" file)))

(defun file-diff (file &key cached)
  (case *vcs*
    (:fossil
     (fossil-file-diff file))
    (:hg
     (hg-file-diff file :cached cached))
    (t
     (git-file-diff file :cached cached))))


;;;
;;; Show commits.
;;;
(defun git-show-commit-diff (ref &key ignore-all-space)
  (let ((options '()))
    (when ignore-all-space
      (cl:push "-w" options))
    (run-git `("show" ,@options ,ref))))

(defun hg-show-commit-diff (ref)
  (run-hg (list "log" "-r" ref "-p")))

(defun show-commit-diff (ref &key ignore-all-space)
  (case *vcs*
    (:fossil nil)
    (:hg (hg-show-commit-diff ref))
    (t (git-show-commit-diff ref :ignore-all-space ignore-all-space))))

;; commit
(defun git-commit (message)
  (run-git (list "commit" "-m" message)))

(defun hg-commit (message)
   (run-hg (list "commit" "-m" message)))

(defun fossil-commit (message)
  (run-fossil (list "commit" "-m" message)))

(defun commit (message)
  (case *vcs*
    (:fossil (fossil-commit message))
    (:hg (hg-commit message))
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
  (porcelain-error "not implemented"))

(defun git-current-branch ()
  (let ((branches (git-list-branches :sort-by "-creatordate")))
    (loop for branch in branches
          if (str:starts-with-p "*" branch)
            return (subseq branch 2))))

(defun hg-current-branch ()
  "Return the current branch name."
  (str:trim
   (run-hg "branch")))

(defun fossil-current-branch ()
  ;; strip out "* " from "* trunk"
  (str:trim
   (subseq (run-fossil "branch") 2)))

(defun current-branch ()
  "Return the current branch name."
  (case *vcs*
    (:fossil
     (fossil-current-branch))
    (:hg
     (hg-current-branch))
    (t
     (git-current-branch))))

(defun rebase-in-progress ()
  "Return a plist if a rebase is in progress. Used for legit-status.

  plist keys:

  :status (boolean) -> T if a rebase is in progress
  :head-name -> content from .git/rebase-merge/head-name, such as \"refs/heads/master\"
  :head-short-name -> \"master\"
  :onto -> content from .git/rebase-merge/onto, a commit id."
  (case *vcs*
    (:git
     (when (uiop:directory-exists-p ".git/rebase-merge/")
       (let ((head (str:trim (str:from-file ".git/rebase-merge/head-name")))
             (onto (str:trim (str:from-file ".git/rebase-merge/onto"))))
         (list :status t
               :head-name head
               :head-short-name (or (third (str:split "/" head))
                                    head)
               :onto onto
               :onto-short-commit (str:shorten 8 onto :ellipsis "")))))
    (t
     (log:info "rebase not available for ~a" *vcs*)
     (values))))

;;;
;;; Latest commits.
;;;
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

(defun hg-latest-commits (&key (hash-length 8))
  (declare (ignorable hash-length))
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


(defun fossil-latest-commits (&key &allow-other-keys)
  ;; return bare result.
  (str:lines (run-fossil "timeline")))

(defun latest-commits (&key (hash-length 8))
  "Return a list of strings or plists.
  The plist has a :hash and a :message, or simply a :line."
  (case *vcs*
    (:fossil
     (fossil-latest-commits))
    (:hg
     (hg-latest-commits))
    (t
     (git-latest-commits :hash-length hash-length))))

;; stage, add files.
(defun git-stage (file)
  (run-git (list "add" file)))

(defun hg-stage (file)
  (run-hg (list "add" file)))

(defun fossil-stage (file)
  (run-fossil (list "add" file)))

(defun stage (file)
  (case *vcs*
    (:fossil (fossil-stage file))
    (:hg (hg-stage file))
    (t (git-stage file))))

(defun unstage (file)
  "Unstage changes to this file.
  The reverse of \"add\"."
  (case *vcs*
    (:git (git-unstage file))
    (:hg (hg-unstage file))
    (:fossil (porcelain-error "unstage not implemented for Fossil."))
    (t (porcelain-error "VCS not supported: ~a" *vcs*))))

(defun git-unstage (file)
  "Unstage changes to a file."
  (run-git (list "reset" "HEAD" "--" file)))
#|
Interestingly, this returns the list of unstaged changes:
"Unstaged changes after reset:
M	src/ext/legit.lisp
M	src/ext/peek-legit.lisp
M	src/ext/porcelain.lisp
""
|#

(defun hg-unstage (file)
  (declare (ignorable file))
  ;; no index like git, we'd need to exclude files from the commit with -X ?
  (porcelain-error "no unstage support for Mercurial"))

;; discard changes.
(defun git-discard-file (file)
  "Discard all the changes to this file.

  This currently means: checkout this file."
  (run-git (list "checkout" file)))

(defun discard-file (file)
  (case *vcs*
    (:git (git-discard-file file))
    (t
     (porcelain-error "discard-file is not implemented for this VCS: ~a" *vcs*))))

(defvar *verbose* nil)

(defun git-apply-patch (diff &key reverse)
  "Apply a patch file.
  This is used to stage hunks of files."
  (when *verbose*
    (log:info diff)
    (with-open-file (f (merge-pathnames "lem-hunk-latest.patch" (%maybe-lem-home))
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
    (:hg (porcelain-error "applying patch not yet implemented for Mercurial"))
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
    (porcelain-error "Our git push command doesn't accept args. Did you mean cl:push ?!!"))
  (run-git (list "push")))

;;
;; Git interactive rebase.
;;
;; -i --autostash
;; If rebase from first commit: use --root
;; otherwise use <commit-hash>^
;; exple: git rebase --autostash -i 317315966^
;; This creates a file in .git/rebase-merge/git-rebase-merge-todo
;; which we edit with Lem, and we validate the rebase process.
;;

(defvar *rebase-script-path*)

;; Save our script as a string at compile time.
(defparameter *rebase-script-content*
  #+(or lem-ncurses lem-sdl2)
  (str:from-file
   (asdf:system-relative-pathname (asdf:find-system "lem")
                                  "scripts/dumbrebaseeditor.sh"))
  #-(or lem-ncurses lem-sdl2)
  ""
  "Our dumb editor shell script, saved as a string at compile time.
  We then save it to the user's ~/.lem/legit/rebaseetidor.sh at first use.")

(defun %maybe-lem-home ()
  "Return Lem's home directory by calling lem:lem-home only if the :lem package exists,
  otherwise return ~/.lem/.
  We don't want a hard-coded dependency on Lem in the porcelain package, to ease testing."
  (if (find-package :lem)
      (uiop:symbol-call :lem :lem-home)
      (merge-pathnames ".lem/" (user-homedir-pathname))))

(defun rebase-script-path ()
  (if (boundp '*rebase-script-path*)
    *rebase-script-path*
    (let* ((legit-path (merge-pathnames "legit/" (%maybe-lem-home)))
           (script-path (namestring (uiop:merge-pathnames* "dumbrebaseeditor.sh" legit-path))))
      (ensure-directories-exist legit-path)
      (unless (uiop:file-exists-p script-path)
        (str:to-file script-path *rebase-script-content*))
      ;; Ensure the file is executable.
      #+unix
      (uiop:run-program (list "chmod" "+x" script-path)
                        :output :string
                        :error-output :string
                        :ignore-error-status t)
      #-unix
      (porcelain-error "lem/legit: our rebase script is only for Unix platforms currently. We need to run a shell script and trap a signal.")
      (setf *rebase-script-path* script-path))))

(defvar *rebase-pid* nil
  "PID file for the git rebase in process.")
;; With this approach, only 1 rebase per Lem process.

(defun root-commit-p (hash)
  "Find this repository's very first commit on the current branch,
  return T if this commit hash is the root.

  hash: (string) can be a short commit hash or an entire one.

  This check is required when doing a git interactive rebase."
  ;; the git command
  ;; git rebase --interactive a1b2c3^
  ;; fails if a1b2c3 is the root commit.
  ;; We must use --root instead.
  (let ((root (run-git (list "rev-list"
                             "--max-parents=0"
                             "HEAD"))))
    ;; We use small hashes, so don't use equal.
    (str:starts-with-p hash root)))

(defun rebase-interactively (&key from)
  (case *vcs*
    (:git (git-rebase-interactively :from from))
    (:hg (porcelain-error "Interactive rebase not implemented for Mercurial"))
    (:fossil (porcelain-error "No interactive rebase for Fossil."))
    (t (porcelain-error "Interactive rebase not available for this VCS: ~a" *vcs*))))

(defun git-rebase-interactively (&key from)
  "Start a rebase session.

  Then edit the git rebase file and validate the rebase with `rebase-continue`
  or stop it with `rebase-abort`.

  from: commit hash (string) to start the rebase from.

  Return three values suitable for `legit:run-function`: output string, error output string, exit code (integer)."
  ;; For testing, go to a test project (,cd on Slime), and edit this project's
  ;; .git/rebase-merge/git-rebase-merge-todo
  ;; Beware of C-c ~ lol^^
  (when (uiop:directory-exists-p ".git/rebase-merge/")
    (porcelain-error "It seems that there is already a rebase-merge directory,
and I wonder if you are in the middle of another rebase.
If that is the case, please try
   git rebase (--continue | --abort | --skip)
If that is not the case, please
   rm -fr \".git/rebase-merge\"
and run me again.
I am stopping in case you still have something valuable there."))

  (unless from
    (return-from git-rebase-interactively
      (values "Git rebase is missing the commit to rebase from. We are too shy to rebase everything from the root commit yet. Aborting"
              nil
              1)))

  (let ((editor (uiop:getenv "EDITOR")))
    (setf (uiop:getenv "EDITOR") (rebase-script-path))
    (unwind-protect
         ;; xxx: get the error output, if any, to get explanations of failure.
         (let ((process (uiop:launch-program (list
                                              "git"
                                              "rebase"
                                              "--autostash"
                                              "-i"
                                              ;; Give the right commit to rebase from.
                                              ;; When rebasing from the root commit,
                                              ;; something special?
                                              (if (root-commit-p from)
                                                  "--root"
                                                  (format nil "~a^" from)))
                                             :output :stream
                                             :error-output :stream
                                             :ignore-error-status t)))
           (if (uiop:process-alive-p process)
               (let* ((output (read-line (uiop:process-info-output process)))
                      (pidtxt (str:trim (second (str:split ":" output)))))
                 (setf *rebase-pid* pidtxt)
                 (format t "The git interactive rebase is started on pid ~a. Edit the rebase file and validate." pidtxt)
                 (values (format nil "rebase started")
                         nil
                         0))
               (porcelain-error "git rebase process didn't start properly. Aborting.")))
      (setf (uiop:getenv "EDITOR") editor))))

(defun rebase-continue ()
  "Either send a continuation signal to the underlying git rebase process, for it to pick up our changes to the interactive rebase file,
  either call git rebase --continue."
  (case *vcs*
    (:git (git-rebase-continue))
    (t
     (porcelain-error "Rebasing is not supported for ~a" *vcs*))))

(defun %rebase-signal (&key (sig "-SIGTERM"))
  "Send a kill signal to our rebase script: with -SIGTERM, git picks up our changes and continues the rebase process. This is called by a rebase continue command.
  With -SIGKILL the process is stopped. This is called by rebase abort."
  (multiple-value-bind (output error-output exit-code)
      (uiop:run-program (list "kill" sig *rebase-pid*)
                        :output :string
                        :error-output :string
                        :ignore-error-status t)
    (declare (ignorable output))
    (setf *rebase-pid* nil)
    (values (format nil "rebase finished.")
            error-output
            exit-code)))

(defun git-rebase-continue ()
  (cond
    ;; First, if we are running our rebase script, send a "continue" signal
    ;; so that git continues the rebase.
    ;; This is used by C-c C-c in the interactive rebase buffer.
    (*rebase-pid*
     (%rebase-signal))
    ;; Check if a rebase was started by someone else and continue it.
    ;; This is called from legit interace: "r" "c".
    ((uiop:directory-exists-p ".git/rebase-merge/")
     (run-git (list "rebase" "--continue")))
    (t
     (porcelain-error  "No git rebase in process?"))))

(defun rebase-abort ()
  (case *vcs*
    (:git (git-rebase-abort))
    (t
     (porcelain-error "Rebasing is not supported for ~a" *vcs*))))

(defun git-rebase-abort ()
  (cond
    ;; First, if we are running our rebase script, kill it.
    ;; This makes git abort the rebase too.
    ;; This is used by C-c C-k in the interactive rebase buffer.
    (*rebase-pid*
     (%rebase-signal :sig "-SIGKILL"))
    ;; Check if a rebase was started by someone else and abort it.
    ;; This is called from legit interface: "r" "a".
    ((uiop:directory-exists-p ".git/rebase-merge/")
     (run-git (list "rebase" "--abort")))
    (t
      (porcelain-error  "No git rebase in process? PID not found."))))

(defun rebase-skip ()
  (case *vcs*
    (:git (git-rebase-skip))
    (t
     (porcelain-error "Rebasing is not supported for ~a" *vcs*))))

(defun git-rebase-skip ()
  (cond
    (*rebase-pid*
     (porcelain-error "The rebase process you started from Lem is still running. Please continue or abort it (or kill job ~a)" *rebase-pid*))
    ;; Check if a rebase was started by someone else and abort it.
    ;; This is called from legit interface: "r" "s".
    ((uiop:directory-exists-p ".git/rebase-merge/")
     (run-git (list "rebase" "--skip")))
    (t
      (porcelain-error  "No git rebase in process? PID not found."))))
