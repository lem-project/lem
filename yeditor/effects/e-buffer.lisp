(defpackage :yeditor-effects/e-buffer
  (:use :cl :lem :yeditor-tx :yeditor-context)
  (:export :install-buffer))
(in-package :yeditor-effects/e-buffer)

(defun install-buffer (comp name)
  "Create a Lem buffer named NAME. Returns an effect handle.
   The undo thunk kills the created buffer."
  (let ((buf (lem:make-buffer name :enable-undo-p nil)))
    (let ((handle (install-effect comp :buffer
                                   (lambda () (declare (ignore)))
                                   :undo-thunk (lambda () (lem:kill-buffer buf)))))
      ;; attach name for debugging
      (push (cons :name name) handle)
      handle)))
