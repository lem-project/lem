(in-package :lem)

(export '(*kill-ring*
          *kill-ring-yank-ptr*
          *kill-ring-max*
          *kill-new-flag*
          kill-append
          kill-push
          yank))

(defvar *kill-ring* nil)
(defvar *kill-ring-yank-ptr* nil)
(defvar *kill-ring-max* 10)

(defvar *kill-new-flag* t)
(defvar *kill-before-p* nil)
(defvar *kill-disable-p* nil)

(defun kill-append (lines before-p)
  (setf (car *kill-ring*)
    (if before-p
      (append
       (butlast lines)
       (list
        (concatenate 'string
          (car (last lines))
          (first (car *kill-ring*))))
       (rest (car *kill-ring*)))
      (append
       (butlast (car *kill-ring*))
       (list
        (concatenate 'string
          (car (last (car *kill-ring*)))
          (first lines)))
       (rest lines)))))

(defun kill-push (lines)
  (cond
   (*kill-new-flag*
    (push lines *kill-ring*)
    (when (nthcdr *kill-ring-max* *kill-ring*)
      (setq *kill-ring*
        (subseq *kill-ring* 0 *kill-ring-max*)))
    (setq *kill-ring-yank-ptr* *kill-ring*)
    (setq *kill-new-flag* nil))
   (t
    (kill-append lines *kill-before-p*)))
  t)

(define-key *global-keymap* "C-y" 'yank)
(define-command yank (n) ("p")
  (do ((ptr *kill-ring-yank-ptr*
         (or (cdr ptr)
             *kill-ring*))
       (n n (1- n)))
      ((>= 1 n)
       (insert-lines (car ptr)))))

(defmacro with-kill (() &body body)
  `(unless *kill-disable-p*
     (when-interrupted-flag :kill
                            (setq *kill-new-flag* t))
     ,@body))
