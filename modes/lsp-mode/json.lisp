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
           :make-json
           :object-to-json
           :json-null
           :json-true
           :json-false
           :json-array
           :json-get
           :json-get*
           :json-object-length
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

(defmethod initialize-instance ((object object) &key (dont-check-required-initarg nil) &allow-other-keys)
  (let ((instance (call-next-method)))
    (unless dont-check-required-initarg (check-required-initarg instance))
    instance))

(defun map-object (function object &key (recursivep t))
  (labels ((rec (value)
             (if (typep value 'object)
                 (loop :for slot :in (closer-mop:class-slots (class-of value))
                       :for slot-name := (closer-mop:slot-definition-name slot)
                       :when (slot-boundp value slot-name)
                       :do (funcall function
                                    (cl-change-case:camel-case (string slot-name))
                                    (if recursivep
                                        (map-object function (slot-value value slot-name))
                                        (slot-value value slot-name))))
                 value)))
    (rec object)))

(defun object-class-p (class)
  (unless (closer-mop:class-finalized-p class)
    (closer-mop:finalize-inheritance class))
  (and (not (typep class 'closer-mop:built-in-class))
       (member 'object (closer-mop:class-precedence-list class) :key #'class-name)))


(defgeneric make-json-internal (json-backend alist))
(defgeneric object-to-json-internal (json-backend object))
(defgeneric to-json-internal (json-backend object))
(defgeneric json-get-internal (json-backend json key))
(defgeneric json-object-length-internal (json-backend json))
(defgeneric json-array-internal (json-backend vector))

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

(defmethod make-json-internal ((json-backend st-json-backend) plist)
  (apply #'st-json:jso plist))

(defmethod object-to-json-internal ((json-backend st-json-backend) object)
  (let ((fields '()))
    (map-object (lambda (k v)
                  (push k fields)
                  (push (if (typep v 'object)
                            (object-to-json-internal json-backend v)
                            v)
                        fields))
                object
                :recursivep nil)
    (apply #'st-json:jso (nreverse fields))))

(defmethod json-get-internal ((json-backend st-json-backend) json key)
  (st-json:getjso key json))

(defmethod json-object-length-internal ((json-backend st-json-backend) json)
  (length (st-json::jso-alist json)))

(defmethod json-array-internal ((json-backend st-json-backend) vector)
  (coerce vector 'list))


(defclass yason-backend (json-backend)
  ()
  (:default-initargs
   :null :null
   :false nil
   :true t
   :array-type 'trivial-types:proper-list
   :object-type 'hash-table))

(defmethod make-json-internal ((json-backend yason-backend) plist)
  (alexandria:plist-hash-table plist :test 'equal))

(defmethod object-to-json-internal ((json-backend yason-backend) object)
  (let ((table (make-hash-table :test 'equal)))
    (map-object (lambda (k v)
                  (setf (gethash k table)
                        (to-json-internal json-backend v)))
                object
                :recursivep nil)
    table))

(defmethod to-json-internal ((json-backend yason-backend) (string string))
  string)

(defmethod to-json-internal ((json-backend yason-backend) (vector vector))
  (map 'list
       (lambda (item)
         (to-json-internal json-backend item))
       vector))

(defmethod to-json-internal ((json-backend yason-backend) (hash-table hash-table))
  (let ((new-table (make-hash-table :test 'equal)))
    (maphash (lambda (k v)
               (setf (gethash k new-table)
                     (to-json-internal json-backend v)))
             hash-table)
    new-table))

(defmethod to-json-internal ((json-backend yason-backend) object)
  (if (typep object 'object)
      (object-to-json-internal json-backend object)
      object))

(defmethod json-get-internal ((json-backend yason-backend) json key)
  (gethash key json))

(defmethod json-object-length-internal ((json-backend yason-backend) json)
  (hash-table-count json))

(defmethod json-array-internal ((json-backend yason-backend) vector)
  vector)


(defparameter *json-backend* (make-instance 'yason-backend))

(defun make-json (&rest plist)
  (make-json-internal *json-backend*
                      (loop :for (k v) :on plist :by #'cddr
                            :collect (etypecase k
                                       (symbol (cl-change-case:camel-case (string k)))
                                       (string k))
                            :collect v)))

(defun object-to-json (object)
  (object-to-json-internal *json-backend* object))

(defun json-null ()
  (json-backend-null *json-backend*))

(defun json-true ()
  (json-backend-true *json-backend*))

(defun json-array (&rest args)
  (json-array-internal *json-backend* (apply #'vector args)))

(defun json-false ()
  (json-backend-false *json-backend*))

(defun json-get (json key)
  (json-get-internal *json-backend* json key))

(defun json-get* (json &rest keys)
  (dolist (key keys json)
    (setq json (json-get json key))))

(defun json-object-length (json)
  (json-object-length-internal *json-backend* json))

(defun json-array-p (value)
  (typep value (json-backend-array-type *json-backend*)))

(defun json-object-p (value)
  (typep value (json-backend-object-type *json-backend*)))
