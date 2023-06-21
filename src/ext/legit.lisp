(defpackage :lem/legit
  (:use :cl
   :lem
   :lem/grep)
  (:export :legit-status))
(in-package :lem/legit)

(define-key lem/peek-legit::*peek-legit-keymap* "?" 'legit-help)
(define-key lem/peek-legit::*peek-legit-keymap* "C-x ?" 'legit-help)
(define-key *global-keymap* "C-x g" 'legit-status)
;; redraw everything:
(define-key lem/peek-legit::*peek-legit-keymap* "g" 'legit-status)

;; Supercharge patch-mode with our keys.
(define-major-mode legit-diff-mode lem-patch-mode:patch-mode
    (:name "legit-diff"
     :syntax-table lem-patch-mode::*patch-syntax-table*
     :keymap *legit-diff-mode-keymap*)
  (setf (variable-value 'enable-syntax-highlight) t))

(define-key *legit-diff-mode-keymap* "C-n" 'next-line)
(define-key *legit-diff-mode-keymap* "C-p" 'previous-line)

(define-key *legit-diff-mode-keymap* "q" 'lem/peek-legit::peek-legit-quit)
(define-key *legit-diff-mode-keymap* "Escape" 'lem/Peek-legit::peek-legit-quit)
(define-key *legit-diff-mode-keymap* "M-q" 'lem/peek-legit::peek-legit-quit)
(define-key *legit-diff-mode-keymap* "c-c C-k" 'lem/peek-legit::peek-legit-quit)
;;; Git commands
;;; that operate on files.
;;;
;;; Global commands like commit need not be defined here.

;; diff
(defun move (file &key cached)
  (let ((buffer (lem-base:get-or-create-buffer "*legit-diff*"))
        (diff (porcelain::file-diff file :cached cached)))
    (log:info "inserting diff to " buffer)
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

(define-command legit-help () ()
  "Show the important keybindings."
  (with-pop-up-typeout-window (s (make-buffer "*Legit help*") :erase t)
    (format s "Lem's interface to git.~&")
    (format s "You can view diffs of (un)staged changes, stage files and create a commit.~&")
    (format s "~%")
    (format s "Navigate: n and p, C-n and C-p.~&")
    (format s "Change windows: C-x o or M-o~&~%")
    (format s "Stage a file: press s~&")
    (format s "Commit: press c~&")
    (format s "Quit: Escape, q, C-x 0.~&")
    (format s "~%")
    (format s "Show this help: C-x ? or ?")
    ))
