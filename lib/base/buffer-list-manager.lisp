(in-package :lem-base)

(defvar *buffer-list-manager*)
(defvar |***allow-initialize-instance-buffer-list-manager-DO-NOT-TOUCH***| nil)

(defclass buffer-list-manager ()
  ((buffers
    :initarg :buffers
    :initform '()
    :accessor buffer-list-manager-buffers)))

(defmethod initialize-instance :before ((instance buffer-list-manager) &key)
  (assert |***allow-initialize-instance-buffer-list-manager-DO-NOT-TOUCH***|))

(defun buffer-list-manager ()
  (unless (boundp '*buffer-list-manager*)
    (setf *buffer-list-manager*
          (let ((|***allow-initialize-instance-buffer-list-manager-DO-NOT-TOUCH***| t))
            (make-instance 'buffer-list-manager))))
  *buffer-list-manager*)

(defun call-with-current-buffers (buffer-list function)
  (let ((buffers (buffer-list-manager-buffers (buffer-list-manager))))
    (setf (buffer-list-manager-buffers (buffer-list-manager)) buffer-list)
    (unwind-protect (funcall function)
      (setf (buffer-list-manager-buffers (buffer-list-manager)) buffers))))

(defmacro with-current-buffers (buffer-list &body body)
  `(call-with-current-buffers ,buffer-list (lambda () ,@body)))
