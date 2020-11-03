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
           :initialized-request))
(in-package :lem-lsp-mode/request)

(cl-package-locks:lock-package :lem-lsp-mode/request)

(lem-lsp-mode/project:local-nickname :protocol :lem-lsp-mode/protocol)
(lem-lsp-mode/project:local-nickname :utils :lem-lsp-mode/utils)

(defgeneric lsp-call-method (client request))

;;;
(defclass abstract-request ()
  ((method-name
    :initarg :method-name
    :reader request-method-name)
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
  (coerce-json (jsonrpc:call (client-connection client)
                             (request-method-name request)
                             (object-to-json (request-params request)))
               (request-response-class-name request)))

(defmethod lsp-call-method (client (request notification))
  (jsonrpc:notify (client-connection client)
                  (request-method-name request)
                  (object-to-json (request-params request))))

;;;
(defclass initialize-request (request)
  ()
  (:default-initargs
   :method-name "initialize"
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

#+(or)
(defmethod lsp-call-method (client (request initialize-request))
  (coerce-json (jsonrpc:call (client-connection client)
                             "initialize"
                             (object-to-json
                              (make-instance 'protocol:initialize-params
                                             :process-id (utils:get-pid)
                                             :client-info (make-json :name "lem" #|:version "0.0.0"|#)
                                             :root-uri (initialize-request-root-uri request)
                                             :capabilities (make-instance 'protocol:client-capabilities
                                                                          :workspace (make-json :apply-edit nil
                                                                                                :workspace-edit nil
                                                                                                :did-change-configuration nil
                                                                                                :symbol nil
                                                                                                :execute-command nil)
                                                                          :text-document nil
                                                                          :experimental nil)
                                             :trace nil
                                             :workspace-folders nil)))
               'protocol:initialize-result))

;;;
(defclass initialized-request (notification)
  ()
  (:default-initargs
   :method-name "initialized"
   :params (make-instance 'protocol:initialized-params)))
