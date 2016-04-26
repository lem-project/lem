(in-package :cl-user)
(defpackage :lem.queue
  (:use :cl)
  (:export
   :queue
   :queue-p
   :copy-queue
   :make-queue
   :queue-empty-p
   :enqueue
   :dequeue
   :pop
   :queue-to-list
   :queue-first-datum
   :queue-last-datum)
  (:shadow :pop))
(in-package :lem.queue)

(defstruct (queue (:constructor %make-queue))
  data
  first
  last)

(defun make-queue (size)
  (%make-queue :data (make-array size)
               :first 0
               :last 0))

(defun queue-empty-p (queue)
  (= (queue-first queue) (queue-last queue)))

(defun enqueue (queue datum)
  (let* ((data (queue-data queue))
         (size (length data)))
    (when (= (queue-first queue)
             (mod (1+ (queue-last queue)) size))
      (dequeue queue))
    (setf (aref data (queue-last queue)) datum)
    (setf (queue-last queue)
          (mod (1+ (queue-last queue)) size)))
  queue)

(defun dequeue (queue)
  (unless (queue-empty-p queue)
    (prog1 (aref (queue-data queue)
                 (queue-first queue))
      (setf (queue-first queue)
            (mod (1+ (queue-first queue))
                 (length (queue-data queue)))))))

(defun pop (queue)
  (unless (queue-empty-p queue)
    (setf (queue-last queue)
          (mod (1- (queue-last queue))
               (length (queue-data queue))))
    (aref (queue-data queue) (queue-last queue))))

(defun queue-to-list (queue)
  (let* ((data (queue-data queue))
         (size (length data))
         (first (queue-first queue))
         (last (queue-last queue))
         (acc))
    (do ((i first (mod (1+ i) size)))
        ((= i last))
      (push (aref data i) acc))
    (nreverse acc)))

(defun queue-first-datum (queue)
  (unless (queue-empty-p queue)
    (aref (queue-data queue)
          (queue-first queue))))

(defun queue-last-datum (queue)
  (unless (queue-empty-p queue)
    (aref (queue-data queue)
          (mod (1- (queue-last queue))
               (length (queue-data queue))))))
