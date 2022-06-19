(in-package :lem)

(export '(*kill-ring-max*
          *kill-before-p*
          kill-append
          kill-push
          kill-ring-rotate
          kill-ring-rotate-undo
          kill-ring-new))

(defparameter *kill-ring-max* 10)

(defvar *kill-ring* nil)
(defvar *kill-ring-yank-ptr* nil)
(defvar *kill-ring-yank-ptr-prev* nil)

(defvar *kill-new-flag* t)
(defvar *kill-before-p* nil)

(defun %kill-append (string options before-p)
  (setf (car *kill-ring*)
        (cons (if before-p
                  (concatenate 'string
                               string
                               (first (car *kill-ring*)))
                  (concatenate 'string
                               (first (car *kill-ring*))
                               string))
              options)))

(defun kill-append (string &rest options)
  (%kill-append string options nil))

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
     (%kill-append string options *kill-before-p*)))
  (when (enable-clipboard-p)
    (copy-to-clipboard (car (first *kill-ring*))))
  t)

(defun current-kill-ring ()
  (or (and (enable-clipboard-p)
           (get-clipboard-data))
      (kill-ring-nth 1)))

(defun kill-ring-nth (n)
  (check-type n (integer 1 *))
  (loop :for ptr := *kill-ring-yank-ptr* :then (or (rest ptr) *kill-ring*)
        :repeat (1- n)
        :finally (return (apply #'values (first ptr)))))

(defun kill-ring-rotate ()
  (when *kill-ring-yank-ptr*
    (destructuring-bind (head &rest tail)
        *kill-ring-yank-ptr*
      (setf *kill-ring-yank-ptr*
            (or tail *kill-ring*))
      (setf *kill-ring-yank-ptr-prev*
            (and tail (list head))))))

(defun kill-ring-rotate-undo ()
  (setf *kill-ring-yank-ptr*
        (if (car *kill-ring-yank-ptr-prev*)
            (cons (pop *kill-ring-yank-ptr-prev*)
                  *kill-ring-yank-ptr*)
            *kill-ring*)))

(defun kill-ring-new ()
  (setf *kill-new-flag* t))
