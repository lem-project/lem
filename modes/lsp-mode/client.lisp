(defpackage :lem-lsp-mode/client
  (:use :cl)
  (:import-from :jsonrpc)
  (:import-from :lem-lsp-mode/lem-stdio-transport
                :lem-stdio-transport)
  (:export :jsonrpc-connect
           :client
           :client-connection
           :tcp-client
           :stdio-client))
(in-package :lem-lsp-mode/client)

(cl-package-locks:lock-package :lem-lsp-mode/client)

(defgeneric jsonrpc-connect (client))

(defclass client ()
  ((connection
    :initform (jsonrpc:make-client)
    :reader client-connection)))

(defclass tcp-client (client)
  ((port
    :initarg :port
    :reader tcp-client-port)))

(defmethod jsonrpc-connect ((client tcp-client))
  (jsonrpc:client-connect (client-connection client)
                          :mode :tcp
                          :port (tcp-client-port client)))

(defclass stdio-client (client)
  ((process :initarg :process
            :reader stdio-client-process)))

(defmethod jsonrpc-connect ((client stdio-client))
  (jsonrpc/class::client-connect-using-class (client-connection client)
                                             'lem-stdio-transport
                                             :process (stdio-client-process client)))
