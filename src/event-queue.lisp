(in-package :lem-core)

(defvar *editor-event-queue* (make-concurrent-queue))

(defun event-queue-length (&optional (evq *editor-event-queue*))
  (len evq))

(defun dequeue-event (timeout &optional (evq *editor-event-queue*))
  (dequeue evq :timeout timeout :timeout-value :timeout))

(defun send-event (obj &optional (evq *editor-event-queue*))
  (enqueue evq obj))

(defun send-abort-event (editor-thread force)
  (bt:interrupt-thread editor-thread
                       (lambda ()
                         (lem-base::interrupt force))))

(defun receive-event (timeout)
  (loop
    (let ((e (dequeue-event timeout)))
      (cond ((null e)
             (return nil))
            ((eql e :timeout)
             (assert timeout)
             (return nil))
            ((eql e :resize)
             (when (>= 1 (event-queue-length))
               (update-on-display-resized)))
            ((consp e)
             (eval e)
             (return t))
            ((or (functionp e) (symbolp e))
             (funcall e))
            (t
             (return e))))))
