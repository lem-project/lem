(in-package :lem)

(export '(*kill-ring-max*
          *kill-before-p*
          kill-append
          kill-push
          kill-ring-rotate
          kill-ring-rotate-undo
          kill-ring-first-string
          kill-ring-nth-string
          kill-ring-new))

(defvar *kill-ring* nil)
(defvar *kill-ring-yank-ptr* nil)
(defvar *kill-ring-yank-ptr-prev* nil)
(defvar *kill-ring-max* 10)

(defvar *kill-new-flag* t)
(defvar *kill-before-p* nil)

(defvar *enable-clipboard-p* nil)

(defun kill-append (string before-p options)
  (setf (car *kill-ring*)
        (cons (if before-p
                  (concatenate 'string
                               string
                               (first (car *kill-ring*)))
                  (concatenate 'string
                               (first (car *kill-ring*))
                               string))
              options)))

(defun copy-to-clipboard (string)
  (lem-if:clipboard-copy (implementation) string))

(defun get-clipboard-data ()
  (lem-if:clipboard-paste (implementation)))

(defun kill-push (string &rest options)
  (cond
    (*kill-new-flag*
     (push (cons string options) *kill-ring*)
     (when (nthcdr *kill-ring-max* *kill-ring*)
       (setq *kill-ring*
             (subseq *kill-ring* 0 *kill-ring-max*)))
     (setq *kill-ring-yank-ptr* *kill-ring*)
     (setq *kill-ring-yank-ptr-prev* nil)
     (setq *kill-new-flag* nil))
    (t
     (kill-append string *kill-before-p* options)))
  (when *enable-clipboard-p*
    (copy-to-clipboard (car (first *kill-ring*))))
  t)

(defun current-kill-ring ()
  (or (and *enable-clipboard-p*
           (get-clipboard-data))
      (kill-ring-nth 1)))

(defun kill-ring-nth (n)
  (do ((ptr *kill-ring-yank-ptr*
            (or (cdr ptr)
                *kill-ring*))
       (n n (1- n)))
      ((>= 1 n)
       (apply #'values (car ptr)))))

(defun kill-ring-rotate ()
  (destructuring-bind (head &rest tail)
      *kill-ring-yank-ptr*
    (setf *kill-ring-yank-ptr*
          (or tail *kill-ring*))
    (setf *kill-ring-yank-ptr-prev*
          (and tail (list head)))))

(defun kill-ring-rotate-undo ()
  (setf *kill-ring-yank-ptr*
        (if (car *kill-ring-yank-ptr-prev*)
            (cons (pop *kill-ring-yank-ptr-prev*)
                  *kill-ring-yank-ptr*)
            *kill-ring*)))

(defun kill-ring-first-string ()
  (apply #'values (car *kill-ring-yank-ptr*)))

(defun kill-ring-nth-string (n)
  (apply #'values (kill-ring-nth n)))

(defun kill-ring-new ()
  (setf *kill-new-flag* t))
