(defpackage :lem-common.ring
  (:use :cl)
  (:export :invalid-index-error
           :ring
           :make-ring
           :empty-p
           :size-of
           :push-back
           :peek-back))
(in-package :lem-common.ring)

(define-condition invalid-index-error (program-error)
  ((index :initarg :index)
   (size :initarg :size))
  (:report (lambda (c s)
             (with-slots (index size) c
               (format s "The index ~D is too large for a ring of size ~D." index size)))))

(defclass ring ()
  ((data :initarg :data
         :reader ring-data)
   (front :initform 0
          :accessor ring-front)
   (rear :initform 0
         :accessor ring-rear)
   (empty-p :initform t
            :accessor ring-empty-p)))

(defmethod print-object ((object ring) stream)
  (print-unreadable-object (object stream :type t)
    (format stream
            "data: ~A front: ~D rear: ~D"
            (ring-data object)
            (ring-front object)
            (ring-rear object))))

(defun make-ring (size)
  (make-instance 'ring :data (make-array size)))

(defmethod empty-p ((ring ring))
  (ring-empty-p ring))

(defmethod size-of ((ring ring))
  (cond ((ring-empty-p ring)
         0)
        ((< (ring-front ring) (ring-rear ring))
         (- (ring-rear ring) (ring-front ring)))
        (t
         (+ (- (ring-rear ring) (ring-front ring))
            (length (ring-data ring))))))

(defmethod ring-size ((ring ring))
  (length (ring-data ring)))

(defmethod push-back ((ring ring) value)
  (setf (aref (ring-data ring) (ring-rear ring))
        value)
  (when (and (not (ring-empty-p ring))
             (= (ring-rear ring)
                (ring-front ring)))
    (setf (ring-front ring)
          (mod (1+ (ring-front ring))
               (ring-size ring))))
  (setf (ring-rear ring)
        (mod (1+ (ring-rear ring))
             (ring-size ring)))
  (setf (ring-empty-p ring) nil)
  ring)

(defmethod peek-back ((ring ring) n)
  (unless (<= 0 n (1- (size-of ring)))
    (error 'invalid-index-error :index n :size (size-of ring)))
  (aref (ring-data ring)
        (mod (- (ring-rear ring) n 1)
             (ring-size ring))))
