(in-package :lem-mcp-server)

;;; MCP Method Definition Framework
;;; Similar pattern to lem-language-server/method.lisp

(defvar *mcp-method-classes* '()
  "List of registered MCP method classes.")

(defvar *debug-on-error* nil
  "When non-nil, don't catch errors for debugging.")

(defclass mcp-method ()
  ((name :initarg :name
         :reader mcp-method-name
         :type string)
   (params-type :initarg :params-type
                :reader mcp-method-params-type)))

(defgeneric call-mcp-method (method params &key server session)
  (:documentation "Call an MCP method with the given params, server, and session."))

(defgeneric call-mcp-method-aux (method params server session)
  (:documentation "Auxiliary method for MCP method dispatch."))

(defun call-with-error-handler (function)
  "Call FUNCTION with error handling."
  (if *debug-on-error*
      (funcall function)
      (handler-case (funcall function)
        (mcp-error (e)
          (error e))
        (error (e)
          (mcp-error +internal-error+
                     (format nil "Internal error: ~A" e))))))

(defmacro with-error-handler (() &body body)
  "Execute BODY with MCP error handling."
  `(call-with-error-handler (lambda () ,@body)))

(defmethod call-mcp-method ((method mcp-method) params &key server session)
  "Default implementation of call-mcp-method."
  (log-mcp-request (mcp-method-name method) params)
  (with-error-handler ()
    (let ((response (call-mcp-method-aux method params server session)))
      (log-mcp-response (mcp-method-name method) response)
      response)))

(defmacro define-mcp-request ((class-name method-name)
                               ((&optional (params (gensym "params")))
                                &key
                                (server (gensym "server"))
                                (session (gensym "session")))
                               &body body)
  "Define an MCP request handler.

CLASS-NAME is the class name for this method.
METHOD-NAME is the JSON-RPC method name string.
PARAMS is the parameter variable name.
SERVER is the server instance variable name.
SESSION is the session variable name.
BODY is the handler implementation."
  (with-gensyms (instance)
    `(progn
       (pushnew ',class-name *mcp-method-classes*)
       (defclass ,class-name (mcp-method)
         ()
         (:default-initargs
          :name ,method-name
          :params-type nil))
       (defmethod call-mcp-method-aux ((,instance ,class-name) ,params ,server ,session)
         (declare (ignorable ,params ,server ,session))
         ,@body))))

(defmacro define-mcp-notification ((class-name method-name)
                                    ((&optional (params (gensym "params")))
                                     &key
                                     (server (gensym "server"))
                                     (session (gensym "session")))
                                    &body body)
  "Define an MCP notification handler (no response expected).

SERVER is the server instance variable name.
SESSION is the session variable name."
  (with-gensyms (instance)
    `(progn
       (pushnew ',class-name *mcp-method-classes*)
       (defclass ,class-name (mcp-method)
         ()
         (:default-initargs
          :name ,method-name
          :params-type nil))
       (defmethod call-mcp-method-aux ((,instance ,class-name) ,params ,server ,session)
         (declare (ignorable ,params ,server ,session))
         ,@body
         nil))))

;; Logging
(defvar *mcp-log-stream* nil
  "Stream for MCP logging. When nil, logging is disabled.")

(defun log-mcp-request (method-name params)
  "Log an MCP request."
  (when *mcp-log-stream*
    (format *mcp-log-stream* "~&[MCP Request] ~A: ~S~%" method-name params)
    (force-output *mcp-log-stream*)))

(defun log-mcp-response (method-name response)
  "Log an MCP response."
  (when *mcp-log-stream*
    (format *mcp-log-stream* "~&[MCP Response] ~A: ~S~%" method-name response)
    (force-output *mcp-log-stream*)))

(defun log-mcp-error (method-name error)
  "Log an MCP error."
  (when *mcp-log-stream*
    (format *mcp-log-stream* "~&[MCP Error] ~A: ~A~%" method-name error)
    (force-output *mcp-log-stream*)))
