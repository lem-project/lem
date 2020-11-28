(defpackage :lem-lsp-mode/client
  (:use :cl)
  (:import-from :jsonrpc)
  (:export :client
           :client-connection
           :tcp-client
           :jsonrpc-connect))
(in-package :lem-lsp-mode/client)

(cl-package-locks:lock-package :lem-lsp-mode/client)
(lem-lsp-mode/project:local-nickname :protocol :lem-lsp-mode/protocol)

(defgeneric jsonrpc-connect (client))

(defclass client ()
  ((connection
    :initform (jsonrpc:make-client)
    :reader client-connection)
   (server-info
    :initarg :server-info
    :type server-info
    :writer set-server-info)
   (server-capabilities
    :initarg :server-capabilities
    :type protocol:server-capabilities
    :writer set-server-capabilities)))

(defclass tcp-client (client)
  ((port
    :initarg :port
    :reader tcp-client-port)))

(defmethod jsonrpc-connect ((client tcp-client))
  (jsonrpc:client-connect (client-connection client)
                          :mode :tcp
                          :port (tcp-client-port client)))
