(defpackage :lem-lsp-mode/main
  (:use :cl :lem-lsp-mode/json)
  (:import-from :lem-lsp-mode/protocol)
  (:import-from :jsonrpc))
(in-package :lem-lsp-mode/main)

#+sbcl
(sb-ext:lock-package :lem-lsp-mode/main)

(defun connect-tcp-client (port)
  (let ((client (jsonrpc:make-client)))
    (jsonrpc:client-connect client :mode :tcp :port port)
    client))

(defun initialize (client &key root-uri)
  (jsonrpc:call client
                "initialize"
                (to-json
                 (make-instance 'lem-lsp-mode/protocol::initialize-params
                                :process-id nil
                                :root-uri root-uri
                                :capabilities nil))))
                               

#|
(defvar *connection* (connect-tcp-client 2089))

(initialize *connection*)
|#
