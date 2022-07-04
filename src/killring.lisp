(in-package :lem)

(defstruct (killring-element (:constructor make-killring-element (string options)))
  string
  options)

(defclass killring ()
  ((ring :initarg :ring
         :reader killring-ring)
   (offset :initform 0
           :accessor killring-offset)
   (appending :initform nil
              :accessor killring-appending)))

(defun make-killring (size)
  (make-instance 'killring :ring (make-ring size)))

(defmethod killring-concat ((killring killring) element before-p)
  (when (ring-empty-p (killring-ring killring))
    (ring-push (killring-ring killring) element)
    (return-from killring-concat element))
  (let ((existing-element (ring-ref (killring-ring killring) 0)))
    (cond (before-p
           (setf (killring-element-string existing-element)
                 (concatenate 'string
                              (killring-element-string element)
                              (killring-element-string existing-element)))
           (setf (killring-element-options existing-element)
                 (append (killring-element-options element)
                         (killring-element-options existing-element))))
          (t
           (setf (killring-element-string existing-element)
                 (concatenate 'string
                              (killring-element-string existing-element)
                              (killring-element-string element)))
           (setf (killring-element-options existing-element)
                 (append (killring-element-options existing-element)
                         (killring-element-options element)))))
    existing-element))

(defmethod killring-add ((killring killring) element before-p)
  (setf (killring-offset killring) 0)
  (cond ((killring-appending killring)
         (killring-concat killring element before-p))
        (t
         (setf (killring-appending killring) t)
         (ring-push (killring-ring killring) element)
         element)))

(defmethod killring-nth ((killring killring) n)
  (handler-case
      (let ((element (ring-ref (killring-ring killring)
                               (+ n (killring-offset killring)))))
        (apply #'values
               (cons (killring-element-string element)
                     (killring-element-options element))))
    (invalid-index-error ()
      nil)))

(defmethod killring-rotate ((killring killring))
  (setf (killring-offset killring)
        (mod (1+ (killring-offset killring))
             (ring-length (killring-ring killring)))))

(defmethod killring-rotate-undo ((killring killring))
  (setf (killring-offset killring)
        (mod (1- (killring-offset killring))
             (ring-length (killring-ring killring)))))

(defmethod killring-new ((killring killring))
  (setf (killring-appending killring) nil))
