(defpackage :lem-language-server/protocol/type
  (:use :cl)
  (:export :lsp-uri
           :lsp-document-uri
           :lsp-integer
           :lsp-uinteger
           :lsp-decimal
           :lsp-regexp
           :lsp-string
           :lsp-boolean
           :lsp-null
           :lsp-array
           :lsp-map
           :lsp-tuple
           :lsp-interface
           :protocol-object
           :protocol-class
           :define-enum
           :define-type-alias
           :define-class
           :protocol-class-slots
           :pascal-to-lisp-case
           :lisp-to-pascal-case
           :make-lsp-map))
(in-package :lem-language-server/protocol/type)

(define-condition required-argument-error (error)
  ((slot-name :initarg :slot-name)
   (class-name :initarg :class-name))
  (:report (lambda (condition stream)
             (with-slots (slot-name class-name) condition
               (format stream
                       "Required argument ~A missing for ~A."
                       slot-name
                       class-name)))))

(deftype lsp-uri () 'string)
(deftype lsp-document-uri () 'string)
(deftype lsp-integer () 'integer)
(deftype lsp-uinteger () '(integer 0 *))
(deftype lsp-decimal () 'integer)
(deftype lsp-regexp () 'string)
(deftype lsp-string () 'string)
(deftype lsp-boolean () 'boolean)
(deftype lsp-null () '(eql :null))

(deftype lsp-array (&optional element-type)
  (declare (ignore element-type))
  'vector)

(deftype lsp-map (key value)
  (declare (ignore key value))
  'hash-table)

(deftype lsp-tuple (&rest types)
  (declare (ignore types))
  'vector)

(deftype lsp-interface (properties &key &allow-other-keys)
  (declare (ignore properties))
  'hash-table)

(defclass protocol-class (c2mop:standard-class)
  ((deprecated :initarg :deprecated
               :reader protocol-class-deprecated)
   (proposed :initarg :proposed
             :reader protocol-class-proposed)
   (since :initarg :since
          :reader protocol-class-since)))

(defmethod c2mop:validate-superclass ((class protocol-class)
                                      (super c2mop:standard-class))
  t)

(defmethod c2mop:validate-superclass ((class c2mop:standard-class)
                                      (super protocol-class))
  t)

(defclass protocol-slot (c2mop:standard-direct-slot-definition)
  ((optional :initarg :optional
             :initform nil
             :reader protocol-slot-optional-p)
   (deprecated :initarg :deprecated
               :initform nil
               :reader protocol-slot-deprecated)
   (proposed :initarg :proposed
             :reader protocol-slot-proposed)
   (since :initarg :since
          :reader protocol-slot-since)))

(defmethod c2mop:direct-slot-definition-class ((class protocol-class) &rest initargs)
  (declare (ignore initargs))
  (find-class 'protocol-slot))

(defclass protocol-object () ()
  (:metaclass protocol-class))

(defun protocol-class-slots (class)
  (unless (c2mop:class-finalized-p class)
    (c2mop:finalize-inheritance class))
  (loop :with base := (find-class 'protocol-class)
        :for superclass :in (c2mop:class-precedence-list class)
        :while (eq base (class-of superclass))
        :append (c2mop:class-direct-slots superclass)))

(defun check-required-initarg (protocol-object)
  (loop :with class := (class-of protocol-object)
        :for slot :in (protocol-class-slots class)
        :for slot-name := (c2mop:slot-definition-name slot)
        :do (unless (protocol-slot-optional-p slot)
              (unless (slot-boundp protocol-object slot-name)
                (error 'required-argument-error
                       :slot-name slot-name
                       :class-name (class-name class))))
            (let ((value (slot-value protocol-object slot-name))
                  (expected-type (c2mop:slot-definition-type slot)))
              (unless (typep value expected-type)
                (error 'type-error
                       :datum value
                       :expected-type expected-type)))))

(defmethod initialize-instance ((instance protocol-object) &rest initargs &key &allow-other-keys)
  (declare (ignore initargs))
  (let ((instance (call-next-method)))
    (check-required-initarg instance)
    instance))

(defmacro define-enum (name (&rest fields) &body options)
  (declare (ignore options))
  (alexandria:with-unique-names (f x anon-name)
    (let ((field-values (mapcar #'second fields)))
      `(progn
         (deftype ,name ()
           (labels ((,f (,x) (member ,x ',field-values :test #'equal)))
             (let ((,anon-name (gensym)))
               (setf (symbol-function ,anon-name) #',f)
               `(satisfies ,,anon-name))))
         ,@(loop :for (field-name value) :in fields
                 :for variable := (intern (format nil "~A-~A" name field-name))
                 :collect `(defparameter ,variable ,value))
         ',name))))

(defmacro define-type-alias (name type &body options)
  (let ((doc (second (assoc :documentation options))))
    `(deftype ,name () ,@(when `(,doc)) ',type)))

(defmacro define-class (name superclasses &body args)
  `(defclass ,name ,(if (null superclasses)
                        '(protocol-object)
                        superclasses)
     ,@args
     (:metaclass protocol-class)))

(defun pascal-to-lisp-case (string)
  (string-upcase
   (if (alexandria:starts-with-subseq "_" string)
       (uiop:strcat "_" (cl-change-case:param-case string))
       (cl-change-case:param-case string))))

(defun lisp-to-pascal-case (string)
  (if (alexandria:starts-with-subseq "_" string)
      (uiop:strcat "_" (cl-change-case:camel-case string))
      (cl-change-case:camel-case string)))

(defun make-lsp-map (&rest key-value-pairs)
  (let ((hash-table (make-hash-table :test 'equal)))
    (loop :for (key value) :on key-value-pairs :by #'cddr
          :do (setf (gethash key hash-table) value))
    hash-table))
