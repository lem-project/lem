(in-package :lem)

(export '(prompt-start-point
          prompt-active-p))

(defgeneric prompt-start-point (prompt))
(defgeneric caller-of-prompt-window (prompt))
(defgeneric prompt-for-character (prompt-string))
(defgeneric prompt-active-p (prompt))

(defgeneric active-minibuffer-window ()) ;TOOD

(defun prompt-for-y-or-n-p (prompt)
  (do () (nil)
    (let ((c (prompt-for-character (format nil "~a [y/n]? " prompt))))
      (case c
        (#\y (return t))
        (#\n (return nil))))))
