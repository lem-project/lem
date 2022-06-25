(cl-lsp/defpackage:defpackage :cl-lsp/server
  (:use :cl)
  (:import-from :cl-lsp/logger
                :log-format)
  (:local-nicknames (:protocol :lem-lsp-utils/protocol)
                    (:json :lem-lsp-utils/json))
  (:export
   :call
   :request-method-name
   :abstract-server
   :server-text-document-controller
   :set-server-text-document-controller
   :register-request
   :server-listen
   :this-server
   :with-server
   :set-client-capabilities
   :tcp-server
   :stdio-server
   :define-method))
(in-package :cl-lsp/server)

(defgeneric call (request params))
(defgeneric request-method-name (request))

;;; abstract server
(defvar *server*)

(defgeneric register-request (server request))
(defgeneric server-listen (server))

(defclass abstract-server ()
  ((client-capabilities
    :initform nil
    :reader server-client-capabilities
    :writer set-client-capabilities)
   (text-document-controller
    :initform nil
    :initarg :text-document-controller
    :reader server-text-document-controller
    :writer set-server-text-document-controller)))

(defun register-all-methods (server)
  (dolist (class (closer-mop:class-direct-subclasses (find-class 'request)))
    (register-request server (make-instance class))))

(defmethod server-listen :before ((server abstract-server))
  (register-all-methods server))

(defmethod server-listen ((server abstract-server)))

(defun this-server () *server*)

(defmacro with-server ((server) &body body)
  `(let ((*server* ,server)) ,@body))

;;; json-object-converter
(defclass json-object-converter ()
  ((params-type
    :initarg :params-type
    :reader request-params-type
    :initform nil))
  (:metaclass closer-mop:funcallable-standard-class))

(defmethod initialize-instance :after ((request json-object-converter) &key)
  (closer-mop:set-funcallable-instance-function
   request
   (lambda (params)
     (call request params))))

(defun convert-params (params params-type)
  (etypecase params-type
    (null params)
    (symbol
     (alexandria:switch ((symbol-package params-type))
       ((load-time-value (find-package :cl-lsp/protocol)) ;fallback
        (cl-lsp/protocol:convert-from-hash-table params-type params))
       ((load-time-value (find-package :lem-lsp-utils/protocol))
        (json:coerce-json params params-type))))))

(defun convert-response (response)
  (if (typep response 'json:object)
      (json:object-to-json response)
      response))

(defmethod call :around ((request json-object-converter) params)
  (let* ((params (convert-params params (request-params-type request)))
         (response (call-next-method request params)))
    (convert-response response)))

;;; request-logger
(defclass request-logger () ())

(defun pprint-json-to-string (object)
  (with-output-to-string (stream)
    (yason:encode object (yason:make-json-output-stream stream))))

(defun request-log (name params)
  (log-format "~%* from client~%")
  (log-format "name: ~A~%" name)
  (log-format "params: ~A~%"
              (pprint-json-to-string params)))

(defun response-log (response)
  (log-format "~%* to server~%~A~%"
              (pprint-json-to-string response)))

(defmethod call :around ((request request-logger) params)
  (request-log (request-method-name request) params)
  (let ((response (call-next-method)))
    (response-log response)
    response))

;;; request-error-handler
(defclass request-error-handler () ())

(defmethod call :around ((request request-error-handler) params)
  (handler-bind ((error (lambda (c)
                          (log-format "~A~2%~A~%"
                                      c
                                      (with-output-to-string (stream)
                                        (uiop:print-backtrace :stream stream
                                                              :condition c))))))
    (call-next-method)))

;;; lifetime
(defclass lifetime ()
  ())

(defmethod call :around ((request lifetime) params)
  (if (or (server-client-capabilities (this-server))
          (string= "initialize" (request-method-name request)))
      (call-next-method)
      (alexandria:plist-hash-table
       (list "code" -32002
             "message" "did not initialize")
       :test 'equal)))

;;; request
(defclass request (request-logger
                   request-error-handler
                   json-object-converter
                   lifetime)
  ((method-name
    :initarg :method-name
    :reader request-method-name
    :initform (alexandria:required-argument :method-name))))

;;; server
(defclass server (abstract-server)
  ((connection :initform (jsonrpc:make-server)
               :reader server-connection)
   (lock :initform (bt:make-lock)
         :reader server-lock)))

(defmethod register-request ((server server) request)
  (jsonrpc:expose (server-connection server)
                  (request-method-name request)
                  (lambda (params)
                    (with-server (server)
                      (bt:with-lock-held ((server-lock server))
                        (funcall request params))))))

;;; tcp-server
(defclass tcp-server (server)
  ((port :initarg :port
         :reader tcp-server-port)))

(defmethod server-listen ((server tcp-server))
  (jsonrpc:server-listen (server-connection server)
                         :mode :tcp
                         :port (tcp-server-port server)))

;;; stdio-server
(defclass stdio-server (server)
  ())

(defmethod server-listen ((server stdio-server))
  (jsonrpc:server-listen (server-connection server) :mode :stdio))

;;;
(defmacro define-method (name (&optional (params (gensym "PARAMS")) params-type) () &body body)
  (alexandria:with-unique-names (g-request)
    (let ((class-name (intern name)))
      `(progn
         (defclass ,class-name (request)
           ()
           (:metaclass closer-mop:funcallable-standard-class)
           (:default-initargs
            :method-name ,name
            :params-type ',params-type))
         (defmethod call ((,g-request ,class-name) ,params)
           ,@body)))))
