(in-package :lem)

(export '(prompt-start-point
          prompt-active-p
          active-minibuffer-window
          prompt-for-character
          prompt-for-y-or-n-p
          ))

(defgeneric prompt-start-point (prompt))
(defgeneric caller-of-prompt-window (prompt))
(defgeneric prompt-active-p (prompt))
(defgeneric active-minibuffer-window ()) ;TOOD
(defgeneric prompt-for-character (prompt-string))

(defun prompt-for-y-or-n-p (prompt-string)
  (do () (nil)
    (let ((c (prompt-for-character (format nil "~a [y/n]? " prompt-string))))
      (case c
        (#\y (return t))
        (#\n (return nil))))))
