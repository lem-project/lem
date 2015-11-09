;; -*- Mode: LISP; Package: LEM -*-

(in-package :lem)

(export '(*kill-ring*
          *kill-ring-yank-ptr*
          *kill-ring-max*
          *kill-new-flag*
          *kill-before-p*
          *kill-disable-p*
          kill-append
          kill-push
          yank
          yank-pop
          kill-ring-first-string
          kill-ring-nth-string
          with-kill))

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

(defun kill-ring-nth (n)
  (do ((ptr *kill-ring-yank-ptr*
            (or (cdr ptr)
                *kill-ring*))
       (n n (1- n)))
      ((>= 1 n)
       (car ptr))))

(define-key *global-keymap* (kbd "C-y") 'yank)
(define-command yank (n) ("p")
  (let ((lines (kill-ring-nth n)))
    (buffer-put (window-buffer) :yank-start (point))
    (insert-lines lines)
    (buffer-put (window-buffer) :yank-end (point))
    (when-interrupted-flag :yank)
    t))

(define-key *global-keymap* (kbd "M-y") 'yank-pop)
(define-command yank-pop (&optional n) ("p")
  (let ((start (buffer-get (window-buffer) :yank-start))
        (end (buffer-get (window-buffer) :yank-end))
        prev-yank-p)
    (when-continue-flag :yank (setq prev-yank-p t))
    (cond ((and start end prev-yank-p)
           (let ((*kill-disable-p* t))
             (kill-region start end))
           (setq *kill-ring-yank-ptr*
                 (or (cdr *kill-ring-yank-ptr*)
                     *kill-ring*))
           (yank n))
          (t
           (minibuf-print "Previous command was not a yank")
           nil))))

(defun kill-ring-first-string ()
  (join (string #\newline)
        (car *kill-ring-yank-ptr*)))

(defun kill-ring-nth-string (n)
  (join (string #\newline)
        (kill-ring-nth n)))

(defmacro with-kill (() &body body)
  `(unless *kill-disable-p*
     (when-interrupted-flag :kill
                            (setq *kill-new-flag* t))
     ,@body))
