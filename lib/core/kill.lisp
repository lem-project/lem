(in-package :lem)

(export '(*kill-before-p*
          kill-append
          kill-push
          kill-ring-rotate
          kill-ring-rotate-undo
          kill-ring-new))

(defvar *kill-before-p* nil)
(defvar *killring* (make-killring 10))

(defun kill-append (string &rest options)
  (killring-concat *killring* (make-killring-element string options) *kill-before-p*))

(defun kill-push (string &rest options)
  (let ((element (killring-add *killring*
                               (make-killring-element string options)
                               *kill-before-p*)))
    (when (enable-clipboard-p)
      (copy-to-clipboard (killring-element-string element)))
    element))

(defun current-kill-ring ()
  (or (and (enable-clipboard-p)
           (get-clipboard-data))
      (killring-nth *killring* 0)))

(defun kill-ring-nth (n)
  (check-type n (integer 1 *))
  (killring-nth *killring* (1- n)))

(defun kill-ring-rotate ()
  (killring-rotate *killring*))

(defun kill-ring-rotate-undo ()
  (killring-rotate-undo *killring*))

(defun kill-ring-new ()
  (killring-new *killring*))
