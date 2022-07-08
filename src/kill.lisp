(in-package :lem)

(defvar *kill-before-p* nil)
(defvar *kill-options* '())

(defvar *killring* (make-killring 10))

(defun call-with-killring (function &key options before repeat)
  (let ((*kill-options* options)
        (*kill-before-p* before))
    (unless repeat
      (killring-new *killring*))
    (funcall function)))

(defmacro with-killring ((&key options before (repeat t)) &body body)
  `(call-with-killring (lambda () ,@body)
                       :options ,options
                       :before ,before
                       :repeat ,repeat))

(defun kill-push (string)
  (killring-add *killring*
                (make-killring-element string *kill-options*)
                *kill-before-p*))

(defun current-kill-ring (&key (use-clipboard (enable-clipboard-p)))
  (or (and use-clipboard (get-clipboard-data))
      (killring-nth *killring* 0)))
