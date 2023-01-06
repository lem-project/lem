(in-package :lem-language-server)

(defvar *method-classes* '())

(defgeneric call (lsp-method json))
(defgeneric call-aux (lsp-method params))

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

(defclass lsp-method ()
  ((name :initarg :name
         :reader lsp-method-name)
   (params-type :initarg :params-type
                :reader lsp-method-params-type)))

(defmethod call :before (lsp-method json)
  (log-request (lsp-method-name lsp-method) json))

(defmethod call (lsp-method json)
  (with-error-handler ()
    (let* ((params (if-let ((params-type (lsp-method-params-type lsp-method)))
                     (convert-from-json json params-type)
                     json))
           (response (call-aux lsp-method params)))
      (log-response response)
      response)))

(defmacro define-request ((class-name method-name)
                          (&optional (params (gensym "params")) params-type)
                          &body body)
  (with-unique-names (instance)
    `(progn
       (pushnew ',class-name *method-classes*)
       (defclass ,class-name (lsp-method)
         ()
         (:default-initargs
          :name ,method-name
          :params-type ',params-type))
       (defmethod call-aux ((,instance ,class-name) ,params)
         ,@body))))

(defun log-request (method-name json)
  (let ((json-string (with-output-to-string (stream)
                       (yason:encode json stream))))
    (log:info method-name
              json-string)))

(defun log-response (response)
  (let ((json
          (with-output-to-string (output)
            (with-open-stream (stream (yason:make-json-output-stream output))
              (yason:encode response stream)))))
    (log:info json)))
