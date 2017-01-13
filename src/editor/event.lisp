(in-package :lem)

(declaim (inline make-queue enqueue dequeue))

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

(let ((wait (bt:make-condition-variable))
      (lock (bt:make-lock))
      (queue (make-queue)))

  (defun dequeue-event (timeout)
    (bt:with-lock-held (lock)
      (if (not (null (car queue)))
          (dequeue queue)
          (cond ((if timeout
                     (bt:condition-wait wait lock :timeout timeout)
                     (bt:condition-wait wait lock))
                 (let ((obj (dequeue queue)))
                   obj))
                (t
                 :timeout)))))

  (defun enqueue-event (obj)
    (bt:with-lock-held (lock)
      (enqueue queue obj)
      (bt:condition-notify wait)))

  (defun empty-event-p ()
    (null (car queue)))

  )
