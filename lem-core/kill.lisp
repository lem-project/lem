(in-package :lem)

(export '(*kill-ring-max*
          *kill-before-p*
          kill-append
          kill-push
          kill-ring-rotate
          kill-ring-first-string
          kill-ring-nth-string
          kill-ring-new))

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

(defvar *clipboard-newer-than-kill-ring-p* nil)
(defvar *kill-ring-newer-than-clipboard-p* nil)
(defvar *sync-kill-ring-with-clipboard* nil)

(defun copy-to-clipboard (string)
  (lem-if:clipboard-copy (implementation) string)
  (setq *clipboard-newer-than-kill-ring-p* t
        *kill-ring-newer-than-clipboard-p* nil))

(defun get-clipboard-data ()
  (lem-if:clipboard-paste (implementation)))

(defun sync-kill-ring-to-clipboard ()
  (when *kill-ring-newer-than-clipboard-p*
    (let ((x (current-kill-ring)))
      (when (and x (string/= x (get-clipboard-data)))
        (copy-to-clipboard x)
        (setq *clipboard-newer-than-kill-ring-p* nil
              *kill-ring-newer-than-clipboard-p* nil)))))

(defun sync-clipboard-to-kill-ring ()
  (when *clipboard-newer-than-kill-ring-p*
    (let ((x (get-clipboard-data)))
      (when (and x (string/= x (current-kill-ring)))
        (kill-push x)
        (setq *clipboard-newer-than-kill-ring-p* nil
              *kill-ring-newer-than-clipboard-p* nil)))))

(defun kill-push (string)
  (setq *clipboard-newer-than-kill-ring-p* nil
        *kill-ring-newer-than-clipboard-p* t)
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

(defun current-kill-ring ()
  (when *sync-kill-ring-with-clipboard*
    (sync-clipboard-to-kill-ring))
  (kill-ring-nth 1))

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
