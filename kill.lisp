(in-package :lem)

(defvar *kill-ring* nil)
(defvar *kill-ring-yank-ptr* nil)
(defvar *kill-ring-max* 10)

(defvar *kill-new-flag* t)

(defun kill-append (str)
  (setf (car *kill-ring-yank-ptr*)
    (concatenate 'string
      (car *kill-ring-yank-ptr*)
      str)))

(defun kill-push (str)
  (cond
   (*kill-new-flag*
    (push str *kill-ring*)
    (when (nthcdr *kill-ring-max* *kill-ring*)
      (setq *kill-ring*
        (subseq *kill-ring* 0 *kill-ring-max*)))
    (setq *kill-ring-yank-ptr* *kill-ring*)
    (setq *kill-new-flag* nil)
    str)
   (t
    (kill-append str))))

(define-key "C-y" 'yank)
(defcommand yank () ()
  (insert-string (car *kill-ring-yank-ptr*)))

(defmacro with-kill (() &body body)
  `(progn
    (when (not *last-kill-flag*)
      (setq *kill-new-flag* t))
    (setq *curr-kill-flag* t)
    (setq *last-kill-flag* t)
    ,@body))
