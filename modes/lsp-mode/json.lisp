(defpackage :lem-lsp-mode/json
  (:use :cl)
  (:import-from :alexandria)
  (:import-from :st-json)
  (:import-from :yason)
  (:import-from :closer-mop)
  (:import-from :cl-change-case)
  (:export :missing-parameter
           :missing-parameter-slot-name
           :missing-parameter-class-name
           :object
           :st-json-backend
           :yason-backend
           :to-json
           :to-json-string
           :json-null
           :json-true
           :json-false
           :json-get
           :from-json))
(in-package :lem-lsp-mode/json)

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

(defmethod initialize-instance ((object object) &key &allow-other-keys)
  (let ((instance (call-next-method)))
    (check-required-initarg instance)
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


(defgeneric to-json-internal (json-library object))
(defgeneric json-get-internal (json-library json key))

(defclass json-library ()
  ((null :initarg :null :reader json-library-null)
   (false :initarg :false :reader json-library-false)
   (true :initarg :true :reader json-library-true)))

(defclass st-json-backend (json-library)
  ()
  (:default-initargs
   :null :null
   :false :false
   :true :true))

(defmethod to-json-internal ((json-library st-json-backend) object)
  (let ((fields '()))
    (map-object (lambda (k v)
                  (push k fields)
                  (push v fields))
                object)
    (apply #'st-json:jso (nreverse fields))))

(defmethod json-get-internal ((json-library st-json-backend) json key)
  (st-json:getjso key json))

(defclass yason-backend (json-library)
  ()
  (:default-initargs
   :null :null
   :false nil
   :true t))

(defmethod to-json-internal ((json-library yason-backend) object)
  (let ((table (make-hash-table :test 'equal)))
    (map-object (lambda (k v)
                  (setf (gethash k table) v))
                object)
    table))

(defmethod json-get-internal ((json-library yason-backend) json key)
  (gethash key json))


(defparameter *json-library* (make-instance 'yason-backend))

(defun to-json (object)
  (to-json-internal *json-library* object))

(defun json-null ()
  (json-library-null *json-library*))

(defun json-true ()
  (json-library-true *json-library*))

(defun json-false ()
  (json-library-false *json-library*))

(defun json-get (json key)
  (json-get-internal *json-library* json key))

(defun from-json (json json-class-name)
  (let ((object (make-instance json-class-name)))
    (loop :for slot :in (closer-mop:class-slots (class-of object))
          :for slot-name := (closer-mop:slot-definition-name slot)
          :do (setf (slot-value object slot-name)
                    (json-get json (cl-change-case:camel-case (string slot-name)))))
    object))
