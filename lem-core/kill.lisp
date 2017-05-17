(in-package :lem)

(export '(*kill-ring-max*
          *kill-before-p*
          kill-append
          kill-push
          kill-ring-rotate
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
     (setq *kill-new-flag* nil)
     (trivial-clipboard:text string))
    (t
     (trivial-clipboard:text (kill-append string *kill-before-p*))))
  t)

(defun current-kill-ring ()
  (let ((string (trivial-clipboard:text)))
    (if (and string (string/= string ""))
        string
        (kill-ring-nth 1))))

(defun kill-ring-nth (n)
  (do ((ptr *kill-ring-yank-ptr*
            (or (cdr ptr)
                *kill-ring*))
       (n n (1- n)))
      ((>= 1 n)
       (car ptr))))

(defun kill-ring-rotate ()
  (setf *kill-ring-yank-ptr*
        (or (cdr *kill-ring-yank-ptr*)
            *kill-ring*)))

(defun kill-ring-first-string ()
  (car *kill-ring-yank-ptr*))

(defun kill-ring-nth-string (n)
  (kill-ring-nth n))

(defun kill-ring-new ()
  (setf *kill-new-flag* t))
