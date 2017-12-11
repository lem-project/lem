(in-package :lem)

(export '(send-event
          send-abort-event))

(declaim (inline make-queue enqueue dequeue empty-queue-p))

(defun make-queue ()
  (cons nil nil))

(defun enqueue (queue obj)
  (cond ((null (car queue))
         (setf (cdr queue)
               (setf (car queue) (list obj))))
        (t
         (setf (cddr queue) (list obj))
         (setf (cdr queue) (cddr queue))))
  obj)

(defun dequeue (queue)
  (pop (car queue)))

(defun empty-queue-p (queue)
  (null (car queue)))

(defstruct event-queue
  (wait (bt:make-condition-variable))
  (lock (bt:make-lock))
  (queue (make-queue)))

(defvar *editor-event-queue* (make-event-queue))

(defun event-queue-length (&optional (evq *editor-event-queue*))
  (bt:with-lock-held ((event-queue-lock evq))
    (length (car (event-queue-queue evq)))))

(defun dequeue-event (timeout &optional (evq *editor-event-queue*))
  (bt:with-lock-held ((event-queue-lock evq))
    (if (not (empty-queue-p (event-queue-queue evq)))
        (dequeue (event-queue-queue evq))
        (cond ((if timeout
                   (bt:condition-wait (event-queue-wait evq) (event-queue-lock evq)
                                      :timeout timeout)
                   (bt:condition-wait (event-queue-wait evq) (event-queue-lock evq)))
               (dequeue (event-queue-queue evq)))
              (t
               :timeout)))))

(defun send-event (obj &optional (evq *editor-event-queue*))
  (bt:with-lock-held ((event-queue-lock evq))
    (enqueue (event-queue-queue evq) obj)
    (bt:condition-notify (event-queue-wait evq))))

(defun send-abort-event (editor-thread force)
  (bt:interrupt-thread editor-thread
                       (lambda ()
                         (lem-base::interrupt force))))

(defun receive-event (timeout)
  (loop
    (let ((e (dequeue-event timeout)))
      (cond ((eql e :timeout)
             (assert timeout)
             (return nil))
            ((eql e :resize)
             (change-display-size-hook))
            ((consp e)
             (eval e)
             (return t))
            ((or (functionp e) (symbolp e))
             (funcall e)
             (return t))
            (t
             (return e))))))
