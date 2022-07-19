(in-package :lem)

(defstruct (killring-item (:constructor make-killring-item (string options)))
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
           (setf (killring-item-string existing-element)
                 (concatenate 'string
                              (killring-item-string element)
                              (killring-item-string existing-element)))
           (setf (killring-item-options existing-element)
                 (append (killring-item-options element)
                         (killring-item-options existing-element))))
          (t
           (setf (killring-item-string existing-element)
                 (concatenate 'string
                              (killring-item-string existing-element)
                              (killring-item-string element)))
           (setf (killring-item-options existing-element)
                 (append (killring-item-options existing-element)
                         (killring-item-options element)))))
    existing-element))

(defmethod killring-add ((killring killring) element before-p)
  (setf (killring-offset killring) 0)
  (cond ((killring-appending killring)
         (killring-concat killring element before-p))
        (t
         (setf (killring-appending killring) t)
         (ring-push (killring-ring killring) element)
         element)))

(defmethod killring-add :after ((killring killring) element before-p)
  (when (enable-clipboard-p)
    (copy-to-clipboard (killring-item-string element))))

(defmethod killring-nth ((killring killring) n)
  (handler-case
      (let ((element (ring-ref (killring-ring killring)
                               (+ n (killring-offset killring)))))
        (apply #'values
               (cons (killring-item-string element)
                     (killring-item-options element))))
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

;;;
(defvar *kill-before-p* nil)
(defvar *kill-options* '())

(defvar *killring* (make-killring 10))

(defun call-with-killring (function &key options before repeat)
  (let ((*kill-options* options)
        (*kill-before-p* before))
    (unless repeat
      (killring-new *killring*))
    (funcall function)))

(defmacro with-killring ((&key options (before '*kill-before-p*) (repeat t)) &body body)
  `(call-with-killring (lambda () ,@body)
                       :options ,options
                       :before ,before
                       :repeat ,repeat))

(defun kill-push (string)
  (killring-add *killring*
                (make-killring-item string *kill-options*)
                *kill-before-p*))

(defun current-kill-ring (&key (use-clipboard (enable-clipboard-p)))
  (or (and use-clipboard (get-clipboard-data))
      (killring-nth *killring* 0)))
