(defpackage :lem-lsp-mode/json
  (:use :cl)
  (:import-from :alexandria)
  (:import-from :st-json)
  (:import-from :yason)
  (:import-from :closer-mop)
  (:import-from :cl-change-case)
  (:import-from :trivia)
  (:import-from :lem-lsp-mode/type)
  (:import-from :rove)
  (:export :missing-parameter
           :missing-parameter-slot-name
           :missing-parameter-class-name
           :object
           :*json-backend*
           :st-json-backend
           :yason-backend
           :to-json
           :to-json-string
           :json-null
           :json-true
           :json-false
           :json-get
           :coerce-json))
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


(defgeneric to-json-internal (json-backend object))
(defgeneric json-get-internal (json-backend json key))

(defclass json-backend ()
  ((null :initarg :null :reader json-backend-null)
   (false :initarg :false :reader json-backend-false)
   (true :initarg :true :reader json-backend-true)))

(defclass st-json-backend (json-backend)
  ()
  (:default-initargs
   :null :null
   :false :false
   :true :true))

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
   :true t))

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


(define-condition json-type-error ()
  ((type :initarg :type)
   (value :initarg :value))
  (:report (lambda (c s)
             (with-slots (value type) c
               (format s "~S is not a ~S" value type)))))

(defun assert-type (value type)
  (unless (typep value type)
    (error 'json-type-error :value value :type type))
  (values))

(defun object-class-p (class)
  (and (not (typep class 'closer-mop:built-in-class))
       (member 'object (closer-mop:class-precedence-list class) :key #'class-name)))

(defun coerce-element (value type)
  (trivia:match type
    ((list 'lem-lsp-mode/type:lsp-array item-type)
     (assert-type value 'list)
     (loop :for item :in value
           :collect (coerce-element item item-type)))
    ((cons 'lem-lsp-mode/type:interface elements)
     (assert-type value 'hash-table)
     (let ((new-hash-table (make-hash-table :test 'equal)))
       (dolist (element elements)
         (destructuring-bind (name &key type) element
           (setf (gethash name new-hash-table)
                 (coerce-element (gethash name value) type))))
       new-hash-table))
    ((list 'lem-lsp-mode/type:equal-specializer value-spec)
     (unless (equal value value-spec)
       (error 'json-type-error :type type :value value))
     value)
    ((list 'lem-lsp-mode/type:object key-type value-type)
     (assert-type value 'hash-table)
     (let ((new-hash-table (make-hash-table :test 'equal)))
       (maphash (lambda (key value)
                  (setf (gethash (coerce-element key key-type) new-hash-table)
                        (coerce-element value value-type)))
                value)
       new-hash-table))
    ((cons 'lem-lsp-mode/type:tuple types)
     (assert-type value 'list)
     (unless (alexandria:length= value types)
       (error 'json-type-error :type type :value value))
     (loop :for type :in types
           :for item :in value
           :collect (coerce-element item type)))
    ;; ((cons 'or types)
    ;;  )
    (otherwise
     (let ((class (and (symbolp type)
                       (find-class type))))
       (cond ((and class
                   (object-class-p class))
              (coerce-json value class))
             (t
              (assert-type value type)
              value))))))

(defun coerce-json (json json-class-name)
  (let ((object (make-instance json-class-name :no-error t)))
    (loop :for slot :in (closer-mop:class-slots (class-of object))
          :for slot-name := (closer-mop:slot-definition-name slot)
          :do (setf (slot-value object slot-name)
                    (coerce-element (json-get json (cl-change-case:camel-case (string slot-name)))
                                    (closer-mop:slot-definition-type slot))))
    object))


(defclass position/test (object)
  ((line :initarg :line :type number)
   (character :initarg :character :type number)))

(defclass range/test (object)
  ((start :initarg :start :type position/test)
   (end :initarg :end :type position/test)))

(defun hash-equal (ht1 ht2)
  (and (= (hash-table-count ht1)
          (hash-table-count ht2))
       (let ((default '#:default))
         (maphash (lambda (k v)
                    (unless (equal v (gethash k ht2 default))
                      (return-from hash-equal nil)))
                  ht1)
         t)))

(defun contain-hash-keys-p (ht keys)
  (alexandria:set-equal (alexandria:hash-table-keys ht)
                        keys
                        :test #'equal))

(defun hash (&rest plist)
  (alexandria:plist-hash-table plist :test 'equal))

(rove:deftest coerce-element
  (rove:testing "lsp-array"
    (rove:ok (rove:signals (coerce-element 100 '(lem-lsp-mode/type:lsp-array integer))
                           'json-type-error))
    (rove:ok (rove:signals (coerce-element '(1 "a") '(lem-lsp-mode/type:lsp-array integer))
                           'json-type-error))
    (rove:ok (equal '(1 2 3)
                    (coerce-element '(1 2 3) '(lem-lsp-mode/type:lsp-array integer)))))
  (rove:testing "equal-specializer"
    (rove:ok (rove:signals (coerce-element 1 '(lem-lsp-mode/type:equal-specializer "foo"))
                           'json-type-error))
    (rove:ok (equal "foo" (coerce-element "foo" '(lem-lsp-mode/type:equal-specializer "foo")))))
  (rove:testing "object"
    (rove:ok (rove:signals (coerce-element 1
                                           '(lem-lsp-mode/type:object string integer))
                           'json-type-error))
    (rove:ok (rove:signals (coerce-element (hash "foo" 100 'bar 200)
                                           '(lem-lsp-mode/type:object string integer))
                           'json-type-error))
    (rove:ok (hash-equal (coerce-element (hash "foo" 100 "bar" 200)
                                         '(lem-lsp-mode/type:object string integer))
                         (hash "foo" 100 "bar" 200)))
    (rove:ok (contain-hash-keys-p (coerce-element (hash "foo" '(100 200) "bar" '(1 2 3))
                                                  '(lem-lsp-mode/type:object string (lem-lsp-mode/type:lsp-array integer)))
                                  '("foo" "bar")))
    (rove:ok (rove:signals (coerce-element (hash "foo" '(100 200) "bar" '(1 "a" 3))
                                           '(lem-lsp-mode/type:object string (lem-lsp-mode/type:lsp-array integer)))
                           'json-type-error)))
  (rove:testing "tuple"
    (rove:ok (rove:signals (coerce-element "foo" '(lem-lsp-mode/type:tuple integer))
                           'json-type-error))
    (rove:ok (rove:signals (coerce-element '(1 2) '(lem-lsp-mode/type:tuple integer))
                           'json-type-error))
    (rove:ok (rove:signals (coerce-element '(1 2) '(lem-lsp-mode/type:tuple integer string))
                           'json-type-error))
    (rove:ok (equal (coerce-element '(1 2) '(lem-lsp-mode/type:tuple integer integer))
                    '(1 2)))
    (rove:ok (rove:signals (coerce-element '(1 2 "foo") '(lem-lsp-mode/type:tuple string integer string))
                           'json-type-error))
    (rove:ok (equal (coerce-element '(1 2 "foo") '(lem-lsp-mode/type:tuple integer integer string))
                    '(1 2 "foo")))))

(rove:deftest coerce-json
  (flet ((position-equals (object &key line character)
           (and (typep object 'position/test)
                (= line (slot-value object 'line))
                (= character (slot-value object 'character)))))
    (let ((object (coerce-json (hash "line" 10
                                     "character" 3)
                               'position/test)))
      (rove:ok (position-equals object :line 10 :character 3)))
    (let ((object (coerce-json (hash "start" (hash "line" 3 "character" 0)
                                     "end" (hash "line" 5 "character" 10))
                               'range/test)))
      (rove:ok (typep object 'range/test))
      (rove:ok (position-equals (slot-value object 'start) :line 3 :character 0))
      (rove:ok (position-equals (slot-value object 'end) :line 5 :character 10)))))
