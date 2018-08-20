(defpackage :lem.history
  (:use :cl)
  (:export :make-history
           :last-history
           :add-history
           :prev-history
           :next-history
           :previous-matching)
  #+sbcl
  (:lock t))
(in-package :lem.history)

(defstruct (history (:constructor %make-history))
  data
  index
  novelty-check)

(defun history-default-novelty-check (x y)
  (not (equal x y)))

(defun make-history (&optional (novelty-check #'history-default-novelty-check))
  (%make-history
   :data (make-array 0 :fill-pointer 0 :adjustable t)
   :index 0
   :novelty-check novelty-check))

(defun last-history (history)
  (when (< 0 (length (history-data history)))
    (aref (history-data history)
          (1- (length (history-data history))))))

(defun add-history (history x)
  (when (funcall (history-novelty-check history)
                 x
                 (last-history history))
    (vector-push-extend x (history-data history)))
  (setf (history-index history)
        (length (history-data history)))
  x)

(defun prev-history (history)
  (when (< 0 (history-index history))
    (values (aref (history-data history)
                  (decf (history-index history)))
            t)))

(defun next-history (history)
  (when (< (history-index history)
           (1- (length (history-data history))))
    (values (aref (history-data history)
                  (incf (history-index history)))
            t)))

(defun previous-matching (history regexp)
  (loop :for i :downfrom (1- (history-index history)) :to 0
        :do (when (ppcre:scan regexp (aref (history-data history) i))
              (setf (history-index history) i)
              (return (values (aref (history-data history) i)
                              t)))))
