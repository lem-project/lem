(defpackage :lem-lsp-mode/request
  (:use :cl)
  (:import-from :lem-lsp-mode/utils)
  (:import-from :lem-lsp-mode/json-lsp-utils
                :make-json
                :coerce-json
                :object-to-json)
  (:import-from :lem-lsp-mode/client
                :client-connection)
  (:export :lsp-call-method
           :initialize-request
           :initialized-request
           :text-document-did-open
           :hover-request))
(in-package :lem-lsp-mode/request)

(cl-package-locks:lock-package :lem-lsp-mode/request)

(lem-lsp-mode/project:local-nickname :protocol :lem-lsp-mode/protocol)
(lem-lsp-mode/project:local-nickname :utils :lem-lsp-mode/utils)

(defgeneric lsp-call-method (client request))

(defvar *log-stream* nil)

(defun do-log (string &rest args)
  (fresh-line *log-stream*)
  (apply #'format *log-stream* string args)
  (terpri *log-stream*))

(defun pretty-json (params)
  (with-output-to-string (stream)
    (yason:encode params (yason:make-json-output-stream stream))))

(defun jsonrpc-call (jsonrpc method params)
  (do-log "request: ~A ~A" method (pretty-json params))
  (let ((response (jsonrpc:call jsonrpc method params)))
    (do-log "response: ~A" (pretty-json response))
    response))

(defun jsonrpc-notify (jsonrpc method params)
  (do-log "notify: ~A ~A" method (pretty-json params))
  (jsonrpc:notify jsonrpc method params))

;;;
(defclass abstract-request ()
  ((method
    :initarg :method
    :reader request-method)
   (params
    :initarg :params
    :reader request-params)))

(defclass request (abstract-request)
  ((response-class-name
    :initarg :response-class-name
    :reader request-response-class-name)))

(defclass notification (abstract-request)
  ())

(defmethod lsp-call-method (client (request request))
  (coerce-json (jsonrpc-call (client-connection client)
                             (request-method request)
                             (object-to-json (request-params request)))
               (request-response-class-name request)))

(defmethod lsp-call-method (client (request notification))
  (jsonrpc-notify (client-connection client)
                  (request-method request)
                  (object-to-json (request-params request))))

;;;
(defclass initialize-request (request)
  ((params :type protocol:initialize-params))
  (:default-initargs
   :method "initialize"
   :response-class-name 'protocol:initialize-result))

#|
(defclass server-info ()
  ((name
    :initarg :name)
   (version
    :initarg :version)))

(defun json-to-server-info (json)
  (make-instance 'server-info
                 :name (json-get json "name")
                 :version (json-get json "version")))
|#

;;;
(defclass initialized-request (notification)
  ((params :type protocol:initialized-params))
  (:default-initargs
   :method "initialized"
   :params (make-instance 'protocol:initialized-params)))

;;;
(defclass text-document-did-open (notification)
  ((params :type protocol:did-open-text-document-params))
  (:default-initargs
   :method "textDocument/didOpen"))

(defclass hover-request (request)
  ((params :type protocol:hover-params))
  (:default-initargs
   :method "textDocument/hover"
   :response-class-name '(or null protocol:hover)))
