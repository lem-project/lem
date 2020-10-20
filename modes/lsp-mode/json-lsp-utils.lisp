(defpackage :lem-lsp-mode/json-lsp-utils
  (:use :cl
        :lem-lsp-mode/json
        :lem-lsp-mode/type)
  (:export :json-type-error
           :coerce-element
           :coerce-json))
(in-package :lem-lsp-mode/json-lsp-utils)

#+sbcl
(sb-ext:lock-package :lem-lsp-mode/json-lsp-utils)

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

(defun coerce-element (value type)
  (trivia:match type
    ((list 'ts-array item-type)
     (assert-type value 'list)
     (loop :for item :in value
           :collect (coerce-element item item-type)))
    ((cons 'ts-interface elements)
     (assert-type value 'hash-table)
     (let ((new-hash-table (make-hash-table :test 'equal)))
       (dolist (element elements)
         (destructuring-bind (name &key type) element
           (setf (gethash name new-hash-table)
                 (coerce-element (gethash name value) type))))
       new-hash-table))
    ((list 'ts-equal-specializer value-spec)
     (unless (equal value value-spec)
       (error 'json-type-error :type type :value value))
     value)
    ((list 'ts-object key-type value-type)
     (assert-type value 'hash-table)
     (let ((new-hash-table (make-hash-table :test 'equal)))
       (maphash (lambda (key value)
                  (setf (gethash (coerce-element key key-type) new-hash-table)
                        (coerce-element value value-type)))
                value)
       new-hash-table))
    ((cons 'ts-tuple types)
     (assert-type value 'list)
     (unless (alexandria:length= value types)
       (error 'json-type-error :type type :value value))
     (loop :for type :in types
           :for item :in value
           :collect (coerce-element item type)))
    ((cons 'or types)
     (dolist (type1 types (error 'json-type-error :type type :value value))
       (handler-case (coerce-element value type1)
         (json-type-error ())
         (:no-error (result)
           (return result)))))
    (otherwise
     (let ((class (and (symbolp type)
                       (find-class type nil))))
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
