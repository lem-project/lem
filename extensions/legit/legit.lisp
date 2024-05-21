(defpackage :lem/legit
  (:use :cl
   :lem)
  (:export :legit-status
           :*prompt-for-commit-abort-p*
           :*ignore-all-space*)
  (:documentation "Display version control data of the current project in an interactive two-panes window.

  This package in particular defines the right window of the legit interface and the user-level commands.

  Gets VCS data by calling lem/porcelain and asking lem/peek-legit to display data on the left window."))

(in-package :lem/legit)

#|
An interactive interface to Git, with preliminary support for other version-control systems (Fossil, Mercurial).

Done:

- status window: current branch, latest commits, unstaged changes, staged changes, untracked files.
- navigation commands
- view changes diff
- stage, unstage files
- inside the diff, stage, unstage hunks
- discard an unstaged file
- commit, redact a commit text in its dedicated buffer
- push, pull the remote branch
- branch checkout, branch create&checkout
- view commit at point
- rebase interactively (see legit-rebase)
- basic Fossil support (current branch, add change, commit)
- basic Mercurial support

Ongoing:

- interactive rebase (POC working for Unix). See more in legit-rebase.lisp

### See also

- https://github.com/fiddlerwoaroof/cl-git native CL, no wrapper around libgit.
- https://github.com/russell/cl-git/ wrapper around libgit2.
- http://shinmera.github.io/legit/ rename that lib and see how useful its caching can be.

|#

(defvar *legit-verbose* nil
  "If non nil, print some logs on standard output (terminal) and create the hunk patch file on disk at (lem home)/lem-hunk-latest.patch.")

(defvar *ignore-all-space* nil "If non t, show all spaces in a diff. Spaces are ignored by default.

Currently Git-only. Concretely, this calls Git with the -w option.")

;; Supercharge patch-mode with our keys.
(define-major-mode legit-diff-mode lem-patch-mode:patch-mode
    (:name "legit-diff"
     :syntax-table lem-patch-mode::*patch-syntax-table*
     :keymap *legit-diff-mode-keymap*)
  (setf (variable-value 'enable-syntax-highlight) t))

;; git commands.
;; Some are defined on peek-legit too.
(define-key *global-keymap* "C-x g" 'legit-status)
(define-key *legit-diff-mode-keymap* "s" 'legit-stage-hunk)
(define-key *legit-diff-mode-keymap* "u" 'legit-unstage-hunk)
(define-key *legit-diff-mode-keymap* "n" 'legit-goto-next-hunk)
(define-key *legit-diff-mode-keymap* "p" 'legit-goto-previous-hunk)

(define-key *legit-diff-mode-keymap* "c" 'legit-commit)
(define-key lem/peek-legit:*peek-legit-keymap* "c" 'legit-commit)

(define-key lem/peek-legit:*peek-legit-keymap* "b b" 'legit-branch-checkout)
(define-key *legit-diff-mode-keymap* "b b" 'legit-branch-checkout)
(define-key lem/peek-legit:*peek-legit-keymap* "b c" 'legit-branch-create)
(define-key *legit-diff-mode-keymap* "b c" 'legit-branch-create)
;; push
(define-key *legit-diff-mode-keymap* "P p" 'legit-push)
(define-key lem/peek-legit:*peek-legit-keymap* "P p" 'legit-push)
;; pull
(define-key lem/peek-legit:*peek-legit-keymap* "F p" 'legit-pull)
(define-key *legit-diff-mode-keymap* "F p" 'legit-pull)

;; rebase
;;; interactive
(define-key lem/peek-legit:*peek-legit-keymap* "r i" 'legit-rebase-interactive)
(define-key lem/peek-legit:*peek-legit-keymap* "r a" 'rebase-abort)
(define-key lem/peek-legit:*peek-legit-keymap* "r c" 'rebase-continue)
(define-key lem/peek-legit:*peek-legit-keymap* "r s" 'rebase-skip)

;; redraw everything:
(define-key lem/peek-legit:*peek-legit-keymap* "g" 'legit-status)

;; navigation
(define-key *legit-diff-mode-keymap* "C-n" 'next-line)
(define-key *legit-diff-mode-keymap* "C-p" 'previous-line)
(define-key lem/peek-legit:*peek-legit-keymap* "M-n" 'legit-next-header)
(define-key lem/peek-legit:*peek-legit-keymap* "M-p" 'legit-previous-header)
(define-key *legit-diff-mode-keymap* "Tab" 'next-window)

;; help
(define-key lem/peek-legit:*peek-legit-keymap* "?" 'legit-help)
(define-key lem/peek-legit:*peek-legit-keymap* "C-x ?" 'legit-help)
;; quit
(define-key *legit-diff-mode-keymap* "q" 'legit-quit)
(define-key lem/peek-legit:*peek-legit-keymap* "q" 'legit-quit)
(define-key *legit-diff-mode-keymap* "M-q" 'legit-quit)
(define-key lem/peek-legit:*peek-legit-keymap* "M-q" 'legit-quit)
(define-key *legit-diff-mode-keymap* "Escape" 'legit-quit)
(define-key lem/peek-legit:*peek-legit-keymap* "Escape" 'legit-quit)
(define-key *legit-diff-mode-keymap* "C-c C-k" 'legit-quit)
(define-key lem/peek-legit:*peek-legit-keymap* "C-c C-k" 'legit-quit)

(defun pop-up-message (message)
  (with-pop-up-typeout-window (s (make-buffer "*legit status*") :erase t)
    (format s "~a" message)))

(defun last-character (s)
  (subseq s (- (length s) 2) (- (length s) 1)))

(defun call-with-porcelain-error (function)
  (handler-bind ((lem/porcelain:porcelain-error
                   (lambda (c)
                     (lem:editor-error (slot-value c 'lem/porcelain::message)))))
      (funcall function)))

(defmacro with-porcelain-error (&body body)
  "Handle porcelain errors and turn them into a lem:editor-error."
  ;; This helps avoiding tight coupling.
  `(call-with-porcelain-error (lambda () ,@body)))

(defun call-with-current-project (function)
  (with-porcelain-error ()
    (let ((root (lem-core/commands/project:find-root (buffer-directory))))
      (uiop:with-current-directory (root)
        (multiple-value-bind (root vcs)
            (lem/porcelain:vcs-project-p)
          (if root
              (let ((lem/porcelain:*vcs* vcs))
                (progn
                  (funcall function)))
              (message "Not inside a version-controlled project?")))))))

(defmacro with-current-project (&body body)
  "Execute body with the current working directory changed to the project's root,
  find and set the VCS system for this operation.

  If no Git directory (or other supported VCS system) are found, message the user."
  `(call-with-current-project (lambda () ,@body)))


;;; Git commands
;;; that operate on files.
;;;
;;; Global commands like commit need not be defined here.

;; diff
(defun show-diff (diff)
  ;; Show a diff in the *legit-diff* buffer.
  ;; Share usage between showing file diff ("move" function)
  ;; and showing a commit.
  (let ((buffer (make-buffer "*legit-diff*")))
    (setf (buffer-directory buffer)
          (uiop:getcwd))
    (setf (buffer-read-only-p buffer) nil)
    (erase-buffer buffer)
    (move-to-line (buffer-point buffer) 1)
    (insert-string (buffer-point buffer) diff)
    (change-buffer-mode buffer 'legit-diff-mode)
    (setf (buffer-read-only-p buffer) t)
    (move-to-line (buffer-point buffer) 1)))

(defun make-diff-function (file  &key cached)
  (lambda ()
    (with-current-project ()
      (show-diff (lem/porcelain:file-diff file :cached cached)))))

(defun make-visit-file-function (file)
  ;; note: the lambda inside the loop is not enough, it captures the last loop value.
  (lambda ()
    file))

;; show commit.
(defun make-show-commit-function (ref)
  (lambda ()
    (with-current-project ()
      (show-diff (lem/porcelain:show-commit-diff ref :ignore-all-space *ignore-all-space*)))))

;; stage
(defun make-stage-function (file)
  (lambda ()
    (with-current-project ()
      (lem/porcelain:stage file)
      t)))

;; unstage
(defun make-unstage-function (file &key already-unstaged)
  (lambda ()
    (with-current-project ()
      (if already-unstaged
          (message "Already unstaged")
          (lem/porcelain:unstage file)))))

;; discard an unstaged change.
(defun make-discard-file-function (file &key is-staged)
  "Discard changes to an unstaged file.

  If is-staged is not nil, then message the user that this file must be unstaged."
  (lambda ()
    (cond
      (is-staged
       (message "Unstage the file first"))
      (t
       (with-current-project ()
         (when (prompt-for-y-or-n-p  (format nil "Discard unstaged changes in ~a?" file))
           (lem/porcelain:discard-file file)))))))


;;;
;;; Git commands
;;; that operate on diff hunks.
;;;

(defun %hunk-start-point (start?)
  "Find the start of the current hunk.
  It is the beginning of the previous \"@@ \" line.
  Return a point, or nil."
  (line-start start?)
  (if (str:starts-with-p "@@ " (line-string start?))
      start?
      (save-excursion
       (let ((point (search-backward-regexp start? "^\@\@")))
         (when point
           point)))))

(defun %hunk-end-point (end?)
  "Find the end of the current hunk.
  It is the last point of the line preceding the following \"@@ \" line,
  or the end of buffer."
  (line-start end?)
  (if (str:starts-with-p "@@ " (line-string end?))
      ;; start searching from next line.
      (setf end?
            (move-to-next-virtual-line end?)))
  (move-point
   end?
   (or
    (and
     (search-forward-regexp end? "^\@\@")
     (line-offset end? -1)
     (line-end end?)
     end?)
    (move-to-end-of-buffer))))

(defun %current-hunk ()
  "Get the current diff hunk at point.
  Return: string.

  To find a hunk, the point has to be inside one,
  i.e, after a line that starts with \"@@\"."
  ;; We are inside a diff (patch) buffer.
  ;; Get the headers and hunk at point to create a patch,
  ;; and we apply the patch.
  ;;
  ;; Steps to stage hunks are:
  ;; - create a patch file
  ;;   - ensure it respects git's format (ends with a space, the first line character is meaningful)
  ;; - apply it to the index
  ;; and that's it.
  ;;
  ;; Idea:
  ;; To get the list of hunk lines, simply check what lines start with "@@ "
  ;; save the lines index, and move the point to the closest line.
  ;; We would NOT need to tediously move points to find lines.

  (save-excursion
   (with-point ((keypresspoint (copy-point (current-point))))
     ;; The first 4 lines are the patch header.
     (let* ((diff-text (buffer-text (point-buffer keypresspoint)))
            (diff-lines (str:lines diff-text))
            (header (str:unlines (subseq diff-lines 0 4)))
            hunk
            patch)
       ;; Get hunk at point.
       (with-point ((start (copy-point keypresspoint) ) ;; @@
                    (start? (copy-point keypresspoint))
                    (end (copy-point keypresspoint))
                    (end? (copy-point keypresspoint)))
         (setf start (%hunk-start-point start?))
         (unless start
           (message "No hunk at point.")
           (return-from %current-hunk))
         (setf end (%hunk-end-point end?))

         (setf hunk (points-to-string start end))
         (setf patch (str:concat header
                                 (string #\newline)
                                 hunk))

         (when (not (equal " " (last-character patch)))
           ;; important for git patch.
           (setf patch (str:join "" (list patch (string #\newline) " "))))

         ;; Delete current hunk in diff buffer, place cursor on next one.
         (when (and start end)
           (setf (buffer-read-only-p (current-buffer)) nil)
           (delete-character start (count-characters start end))
           ;; delete a remaining newline character and we are on the next hunk line.
           (delete-character start 1)
           (setf (buffer-read-only-p (current-buffer)) t))

         patch)))))

(defun run-function (fn &key message)
  "Run this function and show `message` and standard output
  to the user on success as a tooltip message,
  or show the external command's error output on a popup window.

  The function FN returns up to three values:

   - standard output (string)
   - error output (string)
   - exit code (integer)

  Use with-current-project in the caller too.
  Typicaly used to run an external process in the context of a diff buffer command."
  (multiple-value-bind (output error-output exit-code)
      (funcall fn)
    (cond
      ((zerop exit-code)
       (let ((msg (str:join #\newline (remove-if #'null (list message output)))))
         (when (str:non-blank-string-p msg)
           (message msg))))
      (t
       (when error-output
         (pop-up-message error-output))))))

(define-command legit-stage-hunk () ()
  (with-current-project ()
    (run-function (lambda ()
                    (lem/porcelain:apply-patch (%current-hunk)))
                  :message "Staged hunk")))

(define-command legit-unstage-hunk () ()
  (with-current-project ()
    (run-function (lambda ()
                    (lem/porcelain:apply-patch (%current-hunk) :reverse t))
                  :message "Unstaged hunk")))

(define-command legit-goto-next-hunk () ()
  "Move point to the next hunk line, if any."
  (let* ((point (copy-point (current-point)))
         (end? (copy-point (current-point)))
         (end (move-to-next-virtual-line
               (move-point (current-point) (%hunk-end-point point)))))
    (if (equal end (buffer-end end?))
        point
        end)))

(define-command legit-goto-previous-hunk () ()
  "Move point to the previous hunk line, if any."
  (let* ((point (copy-point (current-point)))
         (start? (move-to-previous-virtual-line
                  (copy-point (current-point))))
         (start (when start?
                  (%hunk-start-point start?))))
    (when start
      (move-point (current-point) start)
      (if (equal start (buffer-start start?))
          point
          start))))

(defparameter *commit-buffer-message*
  "~%# Please enter the commit message for your changes.~%~
  # Lines starting with '#' will be discarded, and an empty message does nothing.~%~
  # Validate with C-c C-c, quit with M-q or C-c C-k")

(define-command legit-commit () ()
  "Write a commit message in its dedicated buffer.

  In this buffer, use C-c to validate, M-q or C-c C-k to quit."

  ;; The git command accepts a commit message as argument (-m),
  ;; but also a simple "git commit" starts an editing process, with a pre-formatted
  ;; help text. As with the interactive rebase process, we would need to:
  ;; - start the commit process with a dummy editor,
  ;; - on validation kill the dummy editor and let the git process continue (at this moment git itself decides to validate or to ignore the message).
  ;; This is used by Magit, and we do this for the interactive rebase, but:
  ;; - our dummy editor script doesn't support windows (still as of <2023-09-22 Fri>).
  ;;
  ;; So we go with a simpler, cross-platform and pure Lem/Lisp workflow:
  ;; - create a Lem buffer, add some help text
  ;; - on validation, check ourselves that the message isn't void, extract other information (signature…) and run the commit, with the -m argument.

  (let ((buffer (make-buffer "*legit-commit*")))
    (setf (buffer-directory buffer) (buffer-directory))
    (setf (buffer-read-only-p buffer) nil)
    (erase-buffer buffer)
    (move-to-line (buffer-point buffer) 1)
    (insert-string (buffer-point buffer)
                   (format nil *commit-buffer-message*))
    (change-buffer-mode buffer 'legit-commit-mode)
    (move-to-line (buffer-point buffer) 1)

    ;; The Legit command, like grep, creates its own window.
    ;; Where is it best to show the commit buffer?
    ;; 1) quit the legit view altogether, open the commit buffer in full height:
    ;; (lem/legit::legit-quit)
    ;; 2) open the commit buffer on the left instead of legit status (and nice to have: show the full changes on the right)
    ;; (setf (not-switchable-buffer-p (current-buffer)) nil)
    ;; 3) open the commit buffer on the right, don't touch the ongoing legit status.
    (next-window)
    (switch-to-buffer buffer)))


(define-command legit-status () ()
  "Show changes and untracked files."
  (with-current-project ()
    (multiple-value-bind (untracked-files unstaged-files staged-files)
        (lem/porcelain:components)

      ;; big try! It works \o/
      (lem/peek-legit:with-collecting-sources (collector :read-only nil)
        ;; Header: current branch.
        (lem/peek-legit:collector-insert
         (format nil "Branch: ~a" (lem/porcelain:current-branch))
         :header t)
        (lem/peek-legit:collector-insert "")

        ;; Is a git rebase in progress?
        (let ((rebase-status (lem/porcelain::rebase-in-progress)))
          (when (getf rebase-status :status)
            (lem/peek-legit:collector-insert
             (format nil "!rebase in progress: ~a onto ~a"
                     (getf rebase-status :head-short-name)
                     (getf rebase-status :onto-short-commit)))
            (lem/peek-legit:collector-insert "")))

        ;; Untracked files.
        (lem/peek-legit:collector-insert "Untracked files:" :header t)
        (if untracked-files
            (loop :for file :in untracked-files
                  :do (lem/peek-legit:with-appending-source
                          (point :move-function (make-diff-function file)
                                 :visit-file-function (make-visit-file-function file)
                                 :stage-function (make-stage-function file)
                                 :unstage-function (lambda () (message "File is not tracked, can't be unstaged.")))
                        (insert-string point file :attribute 'lem/peek-legit:filename-attribute :read-only t)))
            (lem/peek-legit:collector-insert "<none>"))

        (lem/peek-legit:collector-insert "")
        ;; Unstaged changes.
        (lem/peek-legit:collector-insert "Unstaged changes:" :header t)
        (if unstaged-files
            (loop :for file :in unstaged-files
                  :do (lem/peek-legit:with-appending-source
                          (point :move-function (make-diff-function file)
                                 :visit-file-function (make-visit-file-function file)
                                 :stage-function (make-stage-function file)
                                 :unstage-function (make-unstage-function file :already-unstaged t)
                                 :discard-file-function (make-discard-file-function file))

                        (insert-string point file :attribute 'lem/peek-legit:filename-attribute :read-only t)
                        ))
            (lem/peek-legit:collector-insert "<none>"))

        (lem/peek-legit:collector-insert "")
        (lem/peek-legit:collector-insert "Staged changes:" :header t)

        ;; Stages files.
        (if staged-files
            (loop :for file :in staged-files
                  :for i := 0 :then (incf i)
                  :do (lem/peek-legit:with-appending-source
                          (point :move-function (make-diff-function file :cached t)
                                 :visit-file-function (make-visit-file-function file)
                                 :stage-function (make-stage-function file)
                                 :unstage-function (make-unstage-function file)
                                 :discard-file-function (make-discard-file-function file :is-staged t))

                        (insert-string point file :attribute 'lem/peek-legit:filename-attribute :read-only t)))
            (lem/peek-legit:collector-insert "<none>"))

        ;; Latest commits.
        (lem/peek-legit:collector-insert "")
        (lem/peek-legit:collector-insert "Latest commits:" :header t)
        (let ((latest-commits (lem/porcelain:latest-commits)))
          (if latest-commits
              (loop for commit in latest-commits
                    for line = nil
                    for hash = nil
                    for message = nil
                    if (consp commit)
                      do (setf line (getf commit :line))
                         (setf hash (getf commit :hash))
                         (setf message (getf commit :message))
                    else
                      do (setf line commit)

                    do (lem/peek-legit:with-appending-source
                           (point :move-function (make-show-commit-function hash)
                                  :visit-file-function (lambda ())
                                  :stage-function (lambda () )
                                  :unstage-function (lambda () ))
                         (with-point ((start point))
                           (when hash
                             (insert-string point hash :attribute 'lem/peek-legit:filename-attribute :read-only t))
                           (if message
                               (insert-string point message)
                               (insert-string point line))

                           ;; Save the hash on this line for later use.
                           (when hash
                             (put-text-property start point :commit-hash hash)))))
              (lem/peek-legit:collector-insert "<none>")))

        (add-hook (variable-value 'after-change-functions :buffer (lem/peek-legit:collector-buffer collector))
                  'change-grep-buffer)))))

(defun prompt-for-branch (&key prompt initial-value)
  ;; only call from a command.
  (let* ((current-branch (or initial-value (lem/porcelain:current-branch)))
         (candidates (lem/porcelain:branches)))
    (if candidates
        (prompt-for-string (or prompt "Branch: ")
                           :initial-value current-branch
                           :history-symbol '*legit-branches-history*
                           :completion-function (lambda (x) (completion-strings x candidates))
                           :test-function (lambda (name) (member name candidates :test #'string=)))
        (message "No branches. Not inside a git project?"))))

(define-command legit-branch-checkout () ()
  "Choose a branch to checkout."
  (with-current-project ()
    (let ((branch (prompt-for-branch))
          (current-branch (lem/porcelain:current-branch)))
      (when (equal branch current-branch)
        (show-message (format nil "Already on ~a" branch) :timeout 3)
        (return-from legit-branch-checkout))
      (when branch
        (run-function (lambda ()
                        (lem/porcelain:checkout branch))
                      :message (format nil "Checked out ~a" branch))
        (legit-status)))))

(define-command legit-branch-create () ()
  "Create and checkout a new branch."
  (with-current-project ()
    (let ((new (prompt-for-string "New branch name: "
                                  :history-symbol '*new-branch-name-history*))
          (base (prompt-for-branch :prompt "Base branch: " :initial-value "")))
      (when (and new base)
        (run-function (lambda ()
                        (lem/porcelain:checkout-create new base))
                      :message (format nil "Created ~a" new))
        (legit-status)))))

(define-command legit-pull () ()
  "Pull changes, update HEAD."
  (with-current-project ()
    (run-function #'lem/porcelain:pull)))

(define-command legit-push () ()
  "Push changes to the current remote."
  (with-current-project ()
    (run-function #'lem/porcelain:push)))

(define-command legit-rebase-interactive () ()
  "Rebase interactively, from the commit the point is on.

  Austostash pending changes, to enable the rebase and find the changes back afterwards."
  (with-current-project ()

    ;; Find the commit hash the point is on: mandatory.
    (let ((commit-hash (text-property-at (current-point) :commit-hash)))

      (unless commit-hash
        (message "Not on a commit line?")
        (return-from legit-rebase-interactive))

      (run-function (lambda ()
                      (lem/porcelain::rebase-interactively :from commit-hash)))

      (let ((buffer (find-file-buffer ".git/rebase-merge/git-rebase-todo")))
        (when buffer
          (lem/peek-legit::quit)
          (switch-to-buffer buffer)
          (change-buffer-mode buffer 'legit-rebase-mode))))))

(define-command legit-next-header () ()
  "Move point to the next header of this VCS window."
  (lem/peek-legit:peek-legit-next-header))

(define-command legit-previous-header () ()
  "Move point to the previous header of this VCS window."
  (lem/peek-legit:peek-legit-previous-header))

(define-command legit-quit () ()
  "Quit"
  (lem/peek-legit:quit)
  (ignore-errors
   (delete-buffer (get-buffer "*legit-diff*"))
   (delete-buffer (get-buffer "*legit-help*"))))

(define-command legit-help () ()
  "Show the important keybindings."
  (with-pop-up-typeout-window (s (make-buffer "*legit-help*") :erase t)
    (format s "Lem's interface to git. M-x legit-status (C-x g)~&")
    (format s "~%")
    (format s "Commands:~&")
    (format s "(s)tage and (u)nstage a file. Inside a diff, (s)tage or (u)nstage a hunk.~&")
    (format s "(k) discard changes.~&")
    (format s "(c)ommit~&")
    (format s "(b)ranches-> checkout another (b)ranch.~&")
    (format s "          -> (c)reate.~&")
    (format s "(F)etch, pull-> (p) from remote branch~&")
    (format s "(P)push      -> (p) to remote branch~&")
    (format s "(r)ebase     -> (i)nteractively from commit at point, (a)bort~&")
    (format s "(g) -> refresh~&")
    (format s "~%")
    (format s "Navigate: n and p, C-n and C-p, M-n and M-p.~&")
    (format s "Change windows: Tab, C-x o, M-o~&")
    (format s "Quit: Escape, q, C-x 0.~&")
    (format s "~%")
    (format s "Show this help: C-x ? or ?, M-x legit-help")
    ))
