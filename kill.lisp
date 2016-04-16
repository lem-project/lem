;; -*- Mode: LISP; Package: LEM -*-

(in-package :lem)

(export '(*kill-ring-max*
          *kill-before-p*
          kill-append
          kill-push
          yank
          yank-pop
          kill-ring-first-string
          kill-ring-nth-string))

(defvar *kill-ring* nil)
(defvar *kill-ring-yank-ptr* nil)
(defvar *kill-ring-max* 10)

(defvar *kill-new-flag* t)
(defvar *kill-before-p* nil)

(defun kill-append (string before-p)
  (setf (car *kill-ring*)
        (if before-p
            (concatenate 'string
                         string
                         (car *kill-ring*))
            (concatenate 'string
                         (car *kill-ring*)
                         string))))

(defun kill-push (string)
  (cond
   (*kill-new-flag*
    (push string *kill-ring*)
    (when (nthcdr *kill-ring-max* *kill-ring*)
      (setq *kill-ring*
            (subseq *kill-ring* 0 *kill-ring-max*)))
    (setq *kill-ring-yank-ptr* *kill-ring*)
    (setq *kill-new-flag* nil))
   (t
    (kill-append string *kill-before-p*)))
  t)

(defun kill-ring-nth (n)
  (do ((ptr *kill-ring-yank-ptr*
            (or (cdr ptr)
                *kill-ring*))
       (n n (1- n)))
      ((>= 1 n)
       (car ptr))))

(define-key *global-keymap* (kbd "C-y") 'yank)
(define-command yank (n) ("p")
  (let ((string (kill-ring-nth n)))
    (setf (get-bvar :yank-start) (current-point))
    (insert-string string)
    (setf (get-bvar :yank-end) (current-point))
    (when-interrupted-flag :yank)
    t))

(define-key *global-keymap* (kbd "M-y") 'yank-pop)
(define-command yank-pop (&optional n) ("p")
  (let ((start (get-bvar :yank-start))
        (end (get-bvar :yank-end))
        prev-yank-p)
    (when-continue-flag :yank (setq prev-yank-p t))
    (cond ((and start end prev-yank-p)
           (delete-region start end)
           (setq *kill-ring-yank-ptr*
                 (or (cdr *kill-ring-yank-ptr*)
                     *kill-ring*))
           (yank n))
          (t
           (minibuf-print "Previous command was not a yank")
           nil))))

(defun kill-ring-first-string ()
  (car *kill-ring-yank-ptr*))

(defun kill-ring-nth-string (n)
  (kill-ring-nth n))

(defun kill-ring-new ()
  (setf *kill-new-flag* t))
