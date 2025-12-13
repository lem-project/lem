(in-package :lem-mcp-server)

;;; MCP Tools Interface
;;; Handles tools/list and tools/call methods

;; tools/list - Return list of available tools
(define-mcp-request (tools-list-request "tools/list") (params)
  (let ((tools (list-all-tools)))
    `(("tools" . ,(mapcar #'tool-to-json tools)))))

;; tools/call - Execute a tool
(define-mcp-request (tools-call-request "tools/call") (params)
  (let* ((name (cdr (assoc "name" params :test #'string=)))
         (arguments (cdr (assoc "arguments" params :test #'string=)))
         (tool (get-tool name)))
    (unless tool
      (mcp-error +method-not-found+
                 (format nil "Tool not found: ~A" name)))
    (handler-case
        (let ((result (funcall (tool-handler tool) arguments)))
          `(("content" . ,(list (make-text-content result)))
            ("isError" . :false)))
      (mcp-error (e)
        `(("content" . ,(list (make-error-content (mcp-error-message e))))
          ("isError" . t)))
      (error (e)
        `(("content" . ,(list (make-error-content (format nil "Error: ~A" e))))
          ("isError" . t))))))

;;; Tool Registration Helper
(defmacro define-mcp-tool (name (&rest args) (&key description input-schema) &body body)
  "Define and register an MCP tool.

NAME is the tool name (string).
ARGS are the argument names extracted from the params alist.
DESCRIPTION is the tool description.
INPUT-SCHEMA is the JSON Schema for the input parameters.
BODY is the tool implementation."
  (let* ((params-var (gensym "params"))
         (arg-bindings (mapcar (lambda (arg)
                                 ;; Convert lisp-style names (buffer-name) to JSON style (buffer_name)
                                 `(,arg (cdr (assoc ,(substitute #\_ #\- (string-downcase (symbol-name arg)))
                                                    ,params-var :test #'string=))))
                               args)))
    `(register-tool
      ,name
      ,description
      ',input-schema
      (lambda (,params-var)
        (let ,arg-bindings
          ,@body)))))
