(defpackage :lem/legit
  (:use :cl
   :lem
   :lem/grep)
  (:export :legit-status))
(in-package :lem/legit)

(defvar *legit-verbose* nil
  "If non nil, print some logs on standard output (terminal) and create the hunk patch file on disk at (lem home)/lem-hunk-latest.patch.")

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
(define-key lem/peek-legit::*peek-legit-keymap* "b b" 'legit-branch-checkout)

;; redraw everything:
(define-key lem/peek-legit::*peek-legit-keymap* "g" 'legit-status)

;; navigation
(define-key *legit-diff-mode-keymap* "C-n" 'next-line)
(define-key *legit-diff-mode-keymap* "C-p" 'previous-line)
;; help
(define-key lem/peek-legit::*peek-legit-keymap* "?" 'legit-help)
(define-key lem/peek-legit::*peek-legit-keymap* "C-x ?" 'legit-help)
;; quit
(define-key *legit-diff-mode-keymap* "q" 'lem/peek-legit::peek-legit-quit)
(define-key *legit-diff-mode-keymap* "Escape" 'lem/Peek-legit::peek-legit-quit)
(define-key *legit-diff-mode-keymap* "M-q" 'lem/peek-legit::peek-legit-quit)
(define-key *legit-diff-mode-keymap* "c-c C-k" 'lem/peek-legit::peek-legit-quit)

(defun pop-up-message (message)
  (with-pop-up-typeout-window (s (make-buffer "*Legit status*") :erase t)
    (format s "~a" message)))

(defun last-character (s)
  (subseq s (- (length s) 2) (- (length s) 1)))


;;; Git commands
;;; that operate on files.
;;;
;;; Global commands like commit need not be defined here.

;; diff
(defun move (file &key cached)
  (let ((buffer (lem-base:get-or-create-buffer "*legit-diff*"))
        (diff (porcelain::file-diff file :cached cached)))
    (setf (buffer-read-only-p buffer) nil)
    (erase-buffer buffer)
    (move-to-line (buffer-point buffer) 1)
    (insert-string (buffer-point buffer) diff)
    (change-buffer-mode buffer 'legit-diff-mode)
    (setf (buffer-read-only-p buffer) t)
    (move-to-line (buffer-point buffer) 1)))

(defun make-move-function (file  &key cached)
  (lambda ()
    (move file :cached cached)))

;; stage
(defun make-stage-function (file)
  (lambda ()
    (porcelain::stage file)
    t))

;; unstage
(defun make-unstage-function (file &key already-unstaged)
  (if already-unstaged
      (lambda ()
        (message "Already unstaged"))
      (lambda ()
        (porcelain::unstage file)
        t)))


;;;
;;; Git commands
;;; that operate on diff hunks.
;;;

(defun %hunk-start-point (start?)
  "Find the start of the current hunk.
  It is the beginning of the previous \"@@ \" line.
  Return a point.
  If no hunk is found before the point, message to the user and exit."
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
    (move-to-end-of-buffer)))
  end?)

(defun %call-legit-hunk-function (fn)
  "Stage the diff hunk at point.
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
           (return-from %call-legit-hunk-function))
         (setf end (%hunk-end-point end?))

         (log:info start end)

         (setf hunk (points-to-string start end))
         (setf patch (str:concat header
                                 (string #\newline)
                                 hunk))
         (when (not (equal " " (last-character patch)))
           ;; important for git patch.
           (setf patch (str:join "" (list patch (string #\newline) " "))))

         (when *legit-verbose*
           (log:info patch)
           (with-open-file (f (merge-pathnames "lem-hunk-latest.patch" (lem-home))
                              :direction :output
                              :if-exists :supersede
                              :if-does-not-exist :create)
             (write-sequence patch f)))

         ;; (uiop:with-temporary-file (:stream f) ;; issues with this.
         (with-open-file (f ".lem-hunk.patch"
                            :direction :output
                            :if-exists :supersede
                            :if-does-not-exist :create)
           (write-sequence patch f)
           (finish-output f))

         (funcall fn))))))

(define-command legit-stage-hunk () ()
  (%call-legit-hunk-function (lambda ()
                               (multiple-value-bind (output error-output exit-code)
                                   (porcelain::apply-patch ".lem-hunk.patch")
                                 (declare (ignorable output))
                                 (cond
                                   ((zerop exit-code)
                                    (message "Hunk staged."))
                                   (t
                                    (when error-output
                                      (pop-up-message error-output))))))))

(define-command legit-unstage-hunk () ()
  (%call-legit-hunk-function (lambda ()
                               (porcelain::apply-patch ".lem-hunk.patch" :reverse t)
                               (message "OK (?)"))))


(define-command legit-status () ()
  "Show changes and untracked files."
  (multiple-value-bind (untracked unstaged-files staged-files)
      (porcelain::components)
    (declare (ignorable untracked))
    ;; (message "Modified files: ~S" modified)

    ;; big try! It works \o/
    (lem/peek-legit:with-collecting-sources (collector :read-only nil)
      ;; (lem/peek-legit::collector-insert "Keys: (n)ext, (p)revious lines,  (s)tage file.")

      (lem/peek-legit::collector-insert 
       (format nil "Branch: ~a" (porcelain::current-branch)))
      (lem/peek-legit::collector-insert "")
      
      ;; Unstaged changes.
      (lem/peek-legit::collector-insert "Unstaged changes:")
      (loop :for file :in unstaged-files
            :do (lem/peek-legit:with-appending-source
                    (point :move-function (make-move-function file)
                           :stage-function (make-stage-function file)
                           :unstage-function (make-unstage-function file :already-unstaged t))

                  (insert-string point file :attribute 'lem/peek-legit:filename-attribute :read-only t)
                  ))

      (lem/peek-legit::collector-insert "")
      (lem/peek-legit::collector-insert "Staged changes:")

      ;; Stages files.
      (if staged-files
          (loop :for file :in staged-files
            :for i := 0 :then (incf i)
            :do (lem/peek-legit::with-appending-staged-files
                    (point :move-function (make-move-function file :cached t)
                           :stage-function (make-stage-function file)
                           :unstage-function (make-unstage-function file))

                  (insert-string point file :attribute 'lem/peek-legit:filename-attribute :read-only t)))
          (lem/peek-legit::collector-insert "<none>"))

      ;; Latest commits.
      (lem/peek-legit::collector-insert "")
      (lem/peek-legit::collector-insert "Latest commits:")
      (let ((latest-commits (porcelain::latest-commits)))
        (if latest-commits
            (loop for line in latest-commits
                  do (lem/peek-legit::collector-insert line))
            (lem/peek-legit::collector-insert "<none>")))

      (add-hook (variable-value 'after-change-functions :buffer (lem/peek-legit:collector-buffer collector))
                'change-grep-buffer))))

(defun prompt-for-branch ()
  ;; only call from a command.
  (let* ((current-branch (porcelain::current-branch))
         (candidates (porcelain::branches)))
    (if candidates
        (prompt-for-string "Branch: "
                           :initial-value current-branch
                           :history-symbol '*legit-branches-history*
                           :completion-function (lambda (x) (completion-strings x candidates))
                           :test-function (lambda (name) (member name candidates :test #'string=)))
        (message "No branches. Not inside a git project?"))))

(define-command legit-branch-checkout () ()
  "Choose a branch to checkout."
  (let ((branch (prompt-for-branch))
        (current-branch (porcelain::current-branch)))
    (when (equal branch current-branch)
      (show-message (format nil "Already on ~a" branch) :timeout 3)
      (return-from legit-branch-checkout))
    (when branch
      (multiple-value-bind (output error-output exit-code)
          (porcelain::checkout branch)
        (declare (ignorable output))
        (cond
          ((zerop exit-code)
           (show-message (format nil "Checked out ~a" branch) :timeout 3))
          (t
           (pop-up-message error-output)))))))

(define-command legit-help () ()
  "Show the important keybindings."
  (with-pop-up-typeout-window (s (make-buffer "*Legit help*") :erase t)
    (format s "Lem's interface to git. M-x legit-status (C-x g)~&")
    (format s "~%")
    (format s "Commands:~&")
    (format s "(s)tage and (u)nstage a file. Inside a diff, (s)tage or (u)nstage a hunk.~&")
    (format s "(c)ommit~&")
    (format s "(b)ranches-> checkout another (b)ranch.~&")
    (format s "(g): refresh~&")
    (format s "~%")
    (format s "Navigate: n and p, C-n and C-p.~&")
    (format s "Change windows: C-x o or M-o~&")
    (format s "Quit: Escape, q, C-x 0.~&")
    (format s "~%")
    (format s "Show this help: C-x ? or ?")
    ))
