(defpackage :yeditor-effects/e-buffer
  (:use :cl :lem :yeditor-tx :yeditor-context)
  (:export :install-buffer))
(in-package :yeditor-effects/e-buffer)

(defun install-buffer (comp name)
  "Register a Lem buffer creation as a transaction effect.
   The buffer is created when the transaction commits; the undo thunk kills it."
  (let ((buf nil))
    (let ((handle (install-effect comp :buffer
                                   (lambda () (setf buf (lem:make-buffer name :enable-undo-p nil)))
                                   :undo-thunk (lambda () (when buf (lem:kill-buffer buf))))))
      (push (cons :name name) handle)
      handle)))
