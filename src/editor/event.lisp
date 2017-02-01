(in-package :lem)

(export '(read-event
          send-event))

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

(defparameter +resize-screen+ (make-symbol "RESIZE-SCREEN"))

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
          (cond ((if timeout
                     (bt:condition-wait wait lock :timeout timeout)
                     (bt:condition-wait wait lock))
                 (let ((obj (dequeue queue)))
                   obj))
                (t
                 :timeout)))))

  (defun send-event (obj)
    (bt:with-lock-held (lock)
      (enqueue queue obj)
      (bt:condition-notify wait)))
  )

(defun send-resize-screen-event (width height)
  (send-event (list +resize-screen+ width height)))

(defun receive-event (timeout)
  (let ((prev-time nil)
        (undone-p))
    (prog1 (loop
             (let ((e (dequeue-event timeout)))
               (cond ((characterp e)
                      (return e))
                     ((eql e :timeout)
                      (assert timeout)
                      (return nil))
                     ((and (consp e) (eq +resize-screen+ (car e)))
                      (let ((curr-time (get-internal-real-time)))
                        (cond ((or (null prev-time) (< 100 (- curr-time prev-time)))
                               (setf undone-p nil)
                               (setf prev-time curr-time)
                               (destructuring-bind (width height) (cdr e)
                                 (update-display-size width height)))
                              (prev-time
                               (setf undone-p (cdr e))))))
                     (t
                      (return e)))))
      (when undone-p
        (destructuring-bind (width height) undone-p
          (update-display-size width height))))))

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
