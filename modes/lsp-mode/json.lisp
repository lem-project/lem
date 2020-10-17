(defpackage :lem-lsp-mode/json
  (:use :cl)
  (:import-from :alexandria)
  (:import-from :st-json)
  (:import-from :yason)
  (:import-from :closer-mop)
  (:import-from :cl-change-case)
  (:import-from :rove)
  (:export :missing-parameter
           :object
           :to-json
           :to-json-string
           :json-null
           :json-true
           :json-false))
(in-package :lem-lsp-mode/json)

(define-condition missing-parameter ()
  ((slot-name
    :initarg :slot-name)
   (class-name
    :initarg :class-name))
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
(defgeneric to-json-string-internal (json-library json))
(defgeneric json-get-internal (json-library json key))

(defclass json-library ()
  ((null :initarg :null :reader json-library-null)
   (false :initarg :false :reader json-library-false)
   (true :initarg :true :reader json-library-true)))

(defclass st-json (json-library)
  ()
  (:default-initargs
   :null :null
   :false :false
   :true :true))

(defmethod to-json-internal ((json-library st-json) object)
  (let ((fields '()))
    (map-object (lambda (k v)
                  (push k fields)
                  (push v fields))
                object)
    (apply #'st-json:jso (nreverse fields))))

(defmethod to-json-string-internal ((json-library st-json) json)
  (st-json:write-json-to-string json))

(defmethod json-get-internal ((json-library st-json) json key)
  (st-json:getjso key json))

(defclass yason (json-library)
  ()
  (:default-initargs
   :null :null
   :false nil
   :true t))

(defmethod to-json-internal ((json-library yason) object)
  (let ((table (make-hash-table :test 'equal)))
    (map-object (lambda (k v)
                  (setf (gethash k table) v))
                object)
    table))

(defmethod to-json-string-internal ((json-library yason) object)
  (with-output-to-string (out)
    (yason:encode object out)))

(defmethod json-get-internal ((json-library yason) json key)
  (gethash key json))


(defparameter *json-library* (make-instance 'yason))

(defun to-json (object)
  (to-json-internal *json-library* object))

(defun to-json-string (object)
  (to-json-string-internal *json-library* (to-json object)))

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


(defclass test-params (object)
  ((a
    :initarg :a)
   (b?
    :initarg :b)
   (c
    :initarg :c)))

(defun json-match-p (jso alist)
  (equalp (st-json::jso-alist jso)
          alist))

(rove:deftest check-required-initarg
  (rove:testing "Missing parameters"
    (flet ((make ()
             (let ((conditions '()))
               (handler-bind ((missing-parameter
                                (lambda (c)
                                  (push c conditions)
                                  (continue c))))
                 (make-instance 'test-params))
               (nreverse conditions)))
           (equals (condition class-name slot-name)
             (and (eq (slot-value condition 'class-name) class-name)
                  (eq (slot-value condition 'slot-name) slot-name))))
      (let ((conditions (make)))
        (rove:ok (= 2 (length conditions)))
        (rove:ok (equals (first conditions) 'test-params 'a))
        (rove:ok (equals (second conditions) 'test-params 'c))))))

(rove:deftest primitive-value
  (rove:testing "st-json"
    (let ((*json-library* (make-instance 'st-json)))
      (rove:ok (eq :null (json-null)))
      (rove:ok (eq :true (json-true)))
      (rove:ok (eq :false (json-false)))))
  (rove:testing "yason"
    (let ((*json-library* (make-instance 'yason)))
      (rove:ok (eq :null (json-null)))
      (rove:ok (eq t (json-true)))
      (rove:ok (eq nil (json-false))))))

(rove:deftest to-json
  (let ((test-params
          (make-instance 'test-params
                         :a "test"
                         :b 100
                         :c '(1 2))))
    (rove:testing "st-json"
      (let* ((*json-library* (make-instance 'st-json))
             (json (to-json test-params)))
        (rove:ok (typep json 'st-json:jso))
        (rove:ok (json-match-p json '(("a" . "test") ("b" . 100) ("c" 1 2))))))
    (rove:testing "yason"
      (let* ((*json-library* (make-instance 'yason))
             (json (to-json test-params)))
        (rove:ok (hash-table-p json))
        (rove:ok (= 3 (hash-table-count json)))
        (rove:ok (equal "test" (gethash "a" json)))
        (rove:ok (equal 100 (gethash "b" json)))
        (rove:ok (equal '(1 2) (gethash "c" json)))))))

(rove:deftest to-json-string
  (rove:testing "st-json"
    (let ((*json-library* (make-instance 'st-json)))
      (rove:ok
       (string= (to-json-string
                 (make-instance 'test-params
                                :a "test"
                                :b 100
                                :c '(1 2)))
                "{\"a\":\"test\",\"b\":100,\"c\":[1,2]}"))))
  (rove:testing "yason"
    (let ((*json-library* (make-instance 'yason)))
      (rove:ok
       (string= (to-json-string
                 (make-instance 'test-params
                                :a "test"
                                :b 100
                                :c '(1 2)))
                "{\"a\":\"test\",\"b\":100,\"c\":[1,2]}")))))

(rove:deftest json-get
  (rove:testing "st-json"
    (let ((*json-library* (make-instance 'st-json)))
      (rove:ok (equal 1
                      (json-get (st-json:jso "foo" 1 "bar" 2)
                                "foo")))
      (rove:ok (equal nil
                      (json-get (st-json:jso "foo" 1 "bar" 2)
                                "xxx")))))
  (rove:testing "yason"
    (let ((*json-library* (make-instance 'yason)))
      (rove:ok (equal 1
                      (json-get (alexandria:plist-hash-table (list "foo" 1 "bar" 2) :test 'equal)
                                "foo")))
      (rove:ok (equal nil
                      (json-get (alexandria:plist-hash-table (list "foo" 1 "bar" 2) :test 'equal)
                                "xxx"))))))
