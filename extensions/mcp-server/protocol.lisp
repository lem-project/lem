(in-package :lem-mcp-server)

;;; MCP Protocol Types
;;; Based on MCP Specification 2024-11-05

;; Server Capabilities
(defun make-server-capabilities ()
  "Create server capabilities object."
  `(("tools" . (("listChanged" . t)))
    ("resources" . (("subscribe" . :false)
                    ("listChanged" . t)))))

;; Server Info
(defun make-server-info ()
  "Create server info object."
  `(("name" . ,*mcp-server-name*)
    ("version" . ,*mcp-server-version*)))

;; Tool definition
(defclass mcp-tool ()
  ((name :initarg :name
         :reader tool-name
         :type string)
   (description :initarg :description
                :reader tool-description
                :type string)
   (input-schema :initarg :input-schema
                 :reader tool-input-schema
                 :type list)
   (handler :initarg :handler
            :reader tool-handler
            :type function)))

(defvar *registered-tools* (make-hash-table :test 'equal)
  "Hash table of registered MCP tools.")

(defun register-tool (name description input-schema handler)
  "Register an MCP tool."
  (setf (gethash name *registered-tools*)
        (make-instance 'mcp-tool
                       :name name
                       :description description
                       :input-schema input-schema
                       :handler handler)))

(defun get-tool (name)
  "Get a registered tool by name."
  (gethash name *registered-tools*))

(defun list-all-tools ()
  "Return list of all registered tools."
  (loop :for tool :being :the :hash-values :of *registered-tools*
        :collect tool))

(defun tool-to-json (tool)
  "Convert tool to JSON-compatible alist."
  `(("name" . ,(tool-name tool))
    ("description" . ,(tool-description tool))
    ("inputSchema" . ,(tool-input-schema tool))))

;; Content types for tool responses
(defun make-text-content (text)
  "Create text content object."
  `(("type" . "text")
    ("text" . ,text)))

(defun make-error-content (message)
  "Create error content object."
  `(("type" . "text")
    ("text" . ,message)))

;; Resource definition
(defclass mcp-resource ()
  ((uri :initarg :uri
        :reader resource-uri
        :type string)
   (name :initarg :name
         :reader resource-name
         :type string)
   (description :initarg :description
                :reader resource-description
                :type (or string null))
   (mime-type :initarg :mime-type
              :reader resource-mime-type
              :type string)))

(defun resource-to-json (resource)
  "Convert resource to JSON-compatible alist."
  (let ((result `(("uri" . ,(resource-uri resource))
                  ("name" . ,(resource-name resource))
                  ("mimeType" . ,(resource-mime-type resource)))))
    (when (resource-description resource)
      (push `("description" . ,(resource-description resource)) result))
    result))

;; JSON-RPC error codes
(defconstant +parse-error+ -32700)
(defconstant +invalid-request+ -32600)
(defconstant +method-not-found+ -32601)
(defconstant +invalid-params+ -32602)
(defconstant +internal-error+ -32603)
(defconstant +server-error+ -32000)

(define-condition mcp-error (error)
  ((code :initarg :code :reader mcp-error-code)
   (message :initarg :message :reader mcp-error-message)
   (data :initarg :data :initform nil :reader mcp-error-data))
  (:report (lambda (condition stream)
             (format stream "MCP Error ~A: ~A"
                     (mcp-error-code condition)
                     (mcp-error-message condition)))))

(defun mcp-error (code message &optional data)
  "Signal an MCP error."
  (error 'mcp-error :code code :message message :data data))
