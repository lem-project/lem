(defpackage :lem-lsp-mode/main
  (:use :cl :lem-lsp-mode/json)
  (:import-from :lem-lsp-mode/protocol)
  (:import-from :jsonrpc))
(in-package :lem-lsp-mode/main)

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
(defvar *connection* (lem-lsp-mode/jsonrpc::connect :port 2089))

(lem-lsp-mode/jsonrpc::send-message
 *connection*
 (make-instance 'lem-lsp-mode/jsonrpc::request-message
                :id 1
                :method "initialize"
                :params (make-instance 'initialize-params
                                       :process-id nil
                                       :root-uri nil
                                       :capabilities nil)))

(lem-lsp-mode/jsonrpc::receive-message *connection*)
|#
