(defpackage :lem-lsp-mode/client
  (:use :cl)
  (:import-from :jsonrpc)
  (:import-from :lem-lsp-mode/lem-stdio-transport
                :lem-stdio-transport)
  (:export :tcp-client
           :stdio-client))
(in-package :lem-lsp-mode/client)

(cl-package-locks:lock-package :lem-lsp-mode/client)

(defclass tcp-client (lem-language-client/client:client)
  ((port
    :initarg :port
    :reader tcp-client-port)))

(defmethod lem-language-client/client:jsonrpc-connect ((client tcp-client))
  (jsonrpc:client-connect (lem-language-client/client:client-connection client)
                          :mode :tcp
                          :port (tcp-client-port client)))

(defclass stdio-client (lem-language-client/client:client)
  ((process :initarg :process
            :reader stdio-client-process)))

(defmethod lem-language-client/client:jsonrpc-connect ((client stdio-client))
  (jsonrpc/class::client-connect-using-class (lem-language-client/client:client-connection client)
                                             'lem-stdio-transport
                                             :process (stdio-client-process client)))
