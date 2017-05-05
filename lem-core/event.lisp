(in-package :lem)

(export '(read-event
          send-event
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

(let ((wait (bt:make-condition-variable))
      (lock (bt:make-lock))
      (queue (make-queue)))

  (defun event-queue-length ()
    (bt:with-lock-held (lock)
      (length (car queue))))

  (defun dequeue-event (timeout)
    (bt:with-lock-held (lock)
      (if (not (empty-queue-p queue))
          (dequeue queue)
          (cond ((progn
                   #-ecl
                   (if timeout
                       (bt:condition-wait wait lock :timeout timeout)
                       (bt:condition-wait wait lock))
                   #+ecl
                   (bt:condition-wait wait lock))
                 (let ((obj (dequeue queue)))
                   obj))
                (t
                 :timeout)))))

  (defun send-event (obj)
    (bt:with-lock-held (lock)
      (enqueue queue obj)
      (bt:condition-notify wait))))

(defun send-abort-event (editor-thread)
  (bt:interrupt-thread editor-thread
                       (lambda ()
                         (error 'editor-interrupt))))

(defun receive-event (timeout)
  (loop
    (let ((e (dequeue-event timeout)))
      (cond ((characterp e)
             (return e))
            ((eql e :timeout)
             (assert timeout)
             (return nil))
            (t
             (return e))))))

(defun read-event (&optional timeout)
  (let ((event (receive-event timeout)))
    (cond ((characterp event)
           event)
          ((listp event)
           (eval event)
           t)
          ((or (functionp event) (symbolp event))
           (funcall event)
           t)
          (t
           event))))
