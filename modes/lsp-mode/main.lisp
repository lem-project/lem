(defpackage :lem-lsp-mode/main
  (:use :cl :lem-lsp-mode/json)
  (:import-from :lem-lsp-mode/protocol)
  (:import-from :lem-lsp-mode/json-lsp-utils
                :coerce-json)
  (:import-from :jsonrpc))
(in-package :lem-lsp-mode/main)

(cl-package-locks:lock-package :lem-lsp-mode/main)

(defun connect-tcp-client (port)
  (let ((client (jsonrpc:make-client)))
    (jsonrpc:client-connect client :mode :tcp :port port)
    client))

(defun initialize (client &key root-uri)
  (coerce-json
   (jsonrpc:call client
                 "initialize"
                 (to-json
                  (make-instance 'lem-lsp-mode/protocol::initialize-params
                                 :process-id nil
                                 :root-uri root-uri
                                 :capabilities nil)))
   'lem-lsp-mode/protocol::initialize-result))

#|
(defvar *connection* (connect-tcp-client 2089))

(initialize *connection*)
|#
