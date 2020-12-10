(defpackage :lem-lsp-mode/client
  (:use :cl)
  (:import-from :jsonrpc)
  (:import-from :lem-lsp-mode/lem-stdio-transport
                :lem-stdio-transport)
  ;; 事前に:jsonrpc/transport/tcpをquickloadしておかないとjsonrpc:client-connect内のquickloadでデッドロック?を起こす事がある
  (:import-from :jsonrpc/transport/tcp)
  (:import-from :jsonrpc/transport/stdio)
  (:export :jsonrpc-connect
           :client
           :client-connection
           :tcp-client
           :stdio-client))
(in-package :lem-lsp-mode/client)

(lem-lsp-mode/project:local-nickname :protocol :lem-lsp-utils/protocol)
(cl-package-locks:lock-package :lem-lsp-mode/client)

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

(defclass stdio-client (client)
  ((process :initarg :process
            :reader stdio-client-process)))

(defmethod jsonrpc-connect ((client stdio-client))
  (jsonrpc/class::client-connect-using-class (client-connection client)
                                             'lem-stdio-transport
                                             :process (stdio-client-process client)))
