(defpackage :lem-lsp-mode/main
  (:use :cl :lem-lsp-mode/json)
  (:import-from :jsonrpc))
(in-package :lem-lsp-mode/main)

(defclass initialize-params (lem-lsp-mode/json:object)
  ((process-id
    :initarg :process-id
    :documentation "/**
 * The process Id of the parent process that started
 * the server. Is null if the process has not been started by another process.
 * If the parent process is not alive then the server should exit (see exit notification) its process.
 */")
   (class-info?
    :initarg :class-info
    :documentation "/**
 * Information about the client
 *
 * @since 3.15.0
 */")
   (root-path?
    :initarg :root-path
    :documentation "/**
 * The rootPath of the workspace. Is null
 * if no folder is open.
 *
 * @deprecated in favour of rootUri.
 */")
   (root-uri
    :initarg :root-uri
    :documentation "/**
 * The rootUri of the workspace. Is null if no
 * folder is open. If both `rootPath` and `rootUri` are set
 * `rootUri` wins.
 */")
   (initialization-options?
    :initarg :initialization-options
    :documentation "/**
 * User provided initialization options.
 */")
   (capabilities
    :initform lem-lsp-mode/json::+json-null+
    :initarg :capabilities
    :documentation "/**
 * The capabilities provided by the client (editor or tool)
 */")
   (trace?
    :initarg :trace
    :documentation "/**
 * The initial trace setting. If omitted trace is disabled ('off').
 */")
   (workspace-folders?
    :initarg :workspace-folders
    :documentation "/**
 * The workspace folders configured in the client when the server starts.
 * This property is only available if the client supports workspace folders.
 * It can be `null` if the client supports workspace folders but none are
 * configured.
 *
 * @since 3.6.0
 */")))

(defun connect-tcp-client (port)
  (let ((client (jsonrpc:make-client)))
    (jsonrpc:client-connect client :mode :tcp :port port)
    client))

(defun initialize (client &key root-uri)
  (jsonrpc:call client
                "initialize"
                (to-json
                 (make-instance 'initialize-params
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
