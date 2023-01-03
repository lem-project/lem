(in-package :lem-language-server)

(defvar *method-classes* '())

(defgeneric call (lsp-method json))

(defclass lsp-method ()
  ((name :initarg :name
         :reader lsp-method-name)))

(defun call-with-error-handler (function)
  (handler-bind ((error (lambda (condition)
                          (let ((error-message
                                  (with-output-to-string (stream)
                                    (format stream "~&~A~%" condition)
                                    (uiop:print-backtrace :stream stream :condition condition))))
                            (log:debug error-message)
                            (return-from call-with-error-handler)))))
    (funcall function)))

(defmacro with-error-handler (() &body body)
  `(call-with-error-handler (lambda () ,@body)))

(defmacro define-request ((class-name method-name)
                          (&optional (params (gensym "params")) (params-type nil params-type-p))
                          &body body)
  (with-unique-names (instance json)
    `(progn
       (defclass ,class-name (lsp-method)
         ()
         (:default-initargs :name ,method-name))
       (defmethod call ((,instance ,class-name) ,json)
         (log-request ,method-name ,json)
         (with-error-handler ()
           (let ((,params ,(if params-type-p
                               `(convert-from-json ,json ',params-type)
                               json)))
             ,@(unless params-type-p `((declare (ignore ,params))))
             ,@body)))
       (pushnew ',class-name *method-classes*))))

(defun log-request (method-name json)
  (let ((json-string (with-output-to-string (stream)
                       (yason:encode json stream))))
    (log:info method-name
              json-string)))
