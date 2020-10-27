(defpackage :lem-lsp-mode/json
  (:use :cl)
  (:import-from :alexandria)
  (:import-from :st-json)
  (:import-from :yason)
  (:import-from :closer-mop)
  (:import-from :cl-change-case)
  (:import-from :trivia)
  (:import-from :trivial-types)
  (:export :missing-parameter
           :missing-parameter-slot-name
           :missing-parameter-class-name
           :object
           :object-class-p
           :*json-backend*
           :st-json-backend
           :yason-backend
           :to-json
           :to-json-string
           :json-null
           :json-true
           :json-false
           :json-get
           :json-array-p
           :json-object-p))
(in-package :lem-lsp-mode/json)

(cl-package-locks:lock-package :lem-lsp-mode/json)

(define-condition missing-parameter ()
  ((slot-name
    :initarg :slot-name
    :reader missing-parameter-slot-name)
   (class-name
    :initarg :class-name
    :reader missing-parameter-class-name))
  (:report (lambda (condition stream)
             (format stream
                     "Required parameter ~A missing for ~A"
                     (slot-value condition 'slot-name)
                     (slot-value condition 'class-name)))))

(defun optional-parameter-name-p (symbol)
  (let ((string (symbol-name symbol)))
    (char= #\? (char string (1- (length string))))))

(defun check-required-initarg (instance)
  (loop :with class := (class-of instance)
        :for slot :in (closer-mop:class-slots class)
        :for slot-name := (closer-mop:slot-definition-name slot)
        :unless (or (optional-parameter-name-p slot-name)
                    (slot-boundp instance slot-name))
        :do (cerror "Ignore warning"
                    'missing-parameter
                    :slot-name slot-name
                    :class-name (class-name class))))

(defclass object ()
  ())

(defmethod initialize-instance ((object object) &key no-error &allow-other-keys)
  (let ((instance (call-next-method)))
    (unless no-error (check-required-initarg instance))
    instance))

(defun map-object (function object)
  (labels ((rec (value)
             (if (typep value 'object)
                 (loop :for slot :in (closer-mop:class-slots (class-of value))
                       :for slot-name := (closer-mop:slot-definition-name slot)
                       :when (slot-boundp value slot-name)
                       :do (funcall function
                                    (cl-change-case:camel-case (string slot-name))
                                    (map-object function (slot-value value slot-name))))
                 value)))
    (rec object)))

(defun object-class-p (class)
  (unless (closer-mop:class-finalized-p class)
    (closer-mop:finalize-inheritance class))
  (and (not (typep class 'closer-mop:built-in-class))
       (member 'object (closer-mop:class-precedence-list class) :key #'class-name)))


(defgeneric to-json-internal (json-backend object))
(defgeneric json-get-internal (json-backend json key))

(defclass json-backend ()
  ((null :initarg :null :reader json-backend-null)
   (false :initarg :false :reader json-backend-false)
   (true :initarg :true :reader json-backend-true)
   (array-type :initarg :array-type :reader json-backend-array-type)
   (object-type :initarg :object-type :reader json-backend-object-type)))

(defclass st-json-backend (json-backend)
  ()
  (:default-initargs
   :null :null
   :false :false
   :true :true
   :array-type 'trivial-types:proper-list
   :object-type 'st-json:jso))

(defmethod to-json-internal ((json-backend st-json-backend) object)
  (let ((fields '()))
    (map-object (lambda (k v)
                  (push k fields)
                  (push v fields))
                object)
    (apply #'st-json:jso (nreverse fields))))

(defmethod json-get-internal ((json-backend st-json-backend) json key)
  (st-json:getjso key json))


(defclass yason-backend (json-backend)
  ()
  (:default-initargs
   :null :null
   :false nil
   :true t
   :array-type 'trivial-types:proper-list
   :object-type 'hash-table))

(defmethod to-json-internal ((json-backend yason-backend) object)
  (let ((table (make-hash-table :test 'equal)))
    (map-object (lambda (k v)
                  (setf (gethash k table) v))
                object)
    table))

(defmethod json-get-internal ((json-backend yason-backend) json key)
  (gethash key json))


(defparameter *json-backend* (make-instance 'yason-backend))

(defun to-json (object)
  (to-json-internal *json-backend* object))

(defun json-null ()
  (json-backend-null *json-backend*))

(defun json-true ()
  (json-backend-true *json-backend*))

(defun json-false ()
  (json-backend-false *json-backend*))

(defun json-get (json key)
  (json-get-internal *json-backend* json key))

(defun json-array-p (value)
  (typep value (json-backend-array-type *json-backend*)))

(defun json-object-p (value)
  (typep value (json-backend-object-type *json-backend*)))
