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
(defclass initialize-request ()
  ((root-uri
    :initform nil
    :initarg :root-uri
    :reader initialize-request-root-uri)))

(defclass server-info ()
  ((name
    :initarg :name)
   (version
    :initarg :version)))

(defun json-to-server-info (json)
  (make-instance 'server-info
                 :name (json-get json "name")
                 :version (json-get json "version")))

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
(defclass initialized-request ()
  ())

(defmethod lsp-call-method (client (request initialized-request))
  (jsonrpc:notify (client-connection client)
                  "initialized"
                  (object-to-json
                   (make-instance 'protocol:initialized-params)))
  (values))
