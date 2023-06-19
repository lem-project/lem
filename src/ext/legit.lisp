(defpackage :lem/legit
  (:use :cl
   :lem
   :lem/grep)
  (:export :legit-status :legit))
(in-package :lem/legit)

(define-key lem/peek-legit::*peek-legit-keymap* "?" 'legit-help)

(defun move (file &key cached)
  (let ((buffer (lem-base:get-or-create-buffer "*legit-diff*"))
        (diff (porcelain::file-diff file :cached cached)))
    (log:info "inserting diff to " buffer)
    (setf (buffer-read-only-p buffer) nil)
    (erase-buffer buffer)
    (move-to-line (buffer-point buffer) 1)
    (insert-string (buffer-point buffer) diff)
    (change-buffer-mode buffer 'lem-patch-mode:patch-mode)
    (setf (buffer-read-only-p buffer) t)
    (move-to-line (buffer-point buffer) 1)))

(defun make-move-function (file  &key cached)
  (lambda ()
    (move file :cached cached)))

(defun stage (file)
  (log:info "Stage! " file)
  (uiop:run-program (list "git" "add" file)))

(defun make-stage-function (file)
  (lambda ()
    (stage file)))

(defun get-content-string (start)
  (with-point ((start start)
               (end start))
    (line-start start)
    (next-single-property-change start :content-start)
    (character-offset start 1)
    (line-end end)
    (points-to-string start end)))

(defun change-grep-buffer (start end old-len)
  (declare (ignore end old-len))
  (let ((string (get-content-string start))
        (move (lem/peek-legit:get-move-function start)))
    (with-point ((point (funcall move)))
      (with-point ((start point)
                   (end point))
        (line-start start)
        (line-end end)
        (buffer-undo-boundary (point-buffer start))
        (delete-between-points start end)
        (insert-string start string)
        (buffer-undo-boundary (point-buffer start)))))
  (lem/peek-legit:show-matched-line))

(defvar *last-query* "git grep -nH ")
(defvar *last-directory* nil)

;; (load "porcelain.lisp")
(load "src/ext/porcelain.lisp")

(define-command legit-status () ()
  "Show changes and untracked files."
  (multiple-value-bind (untracked unstaged-files staged-files)
      (porcelain::components)
    (declare (ignorable untracked))
    ;; (message "Modified files: ~S" modified)

    ;; big try!
    (lem/peek-legit:with-collecting-sources (collector :read-only nil)
      ;; (lem/peek-legit::collector-insert "Keys: (n)ext, (p)revious lines,  (s)tage file.")

      (lem/peek-legit::collector-insert "Unstaged changes:")
      (loop :for file :in unstaged-files
            :do (lem/peek-legit:with-appending-source
                    (point :move-function (make-move-function file)
                           :stage-function (make-stage-function file))

                  (insert-string point file :attribute 'lem/peek-legit:filename-attribute :read-only t)
                  ))

      (lem/peek-legit::collector-insert "")
      (lem/peek-legit::collector-insert "Staged changes:")

      (loop :for file :in staged-files
            :for i := 0 :then (incf i)
            :do (lem/peek-legit::with-appending-staged-files
                    (point :move-function (make-move-function file :cached t)
                           :stage-function (make-stage-function file))

                  (insert-string point file :attribute 'lem/peek-legit:filename-attribute :read-only t)))

      (add-hook (variable-value 'after-change-functions :buffer (lem/peek-legit:collector-buffer collector))
                'change-grep-buffer))))

(define-command legit-help () ()
  "Show the important keybindings."
  (with-pop-up-typeout-window (s (make-buffer "*Legit help*") :erase t)
    (format s "Lem's interface to git.")
    (format s "~%~%")
    (format s "Stage a file: press s")
    (format s "~%~%")
    (format s "Press q to quit.")
    ))
