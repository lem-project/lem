(defpackage :lem-tests/mcp-server/utils
  (:use :cl)
  (:export :with-mcp-test-env
           :mcp-request
           :mcp-tool-call
           :lines
           :extract-text-content
           :extract-result
           :result-is-error-p))
(in-package :lem-tests/mcp-server/utils)

;;; Test utilities for MCP Server tests
;;; Following Classical/Detroit school approach:
;;; - Use real code, minimal mocks
;;; - Only bypass HTTP layer (shared dependency)
;;; - Use real Lem buffers via fake-interface

(defvar *test-mcp-server* nil
  "The MCP server instance for testing.")

(defmacro with-mcp-test-env (() &body body)
  "Set up MCP test environment.
Bypasses HTTP layer but uses real MCP handlers and Lem buffers."
  `(lem-fake-interface:with-fake-interface ()
     (let* ((server (make-instance 'lem-mcp-server::mcp-server :port 0))
            (*test-mcp-server* server)
            (lem-mcp-server::*current-mcp-server* server)
            (lem-mcp-server::*current-session*
             (make-instance 'lem-mcp-server::mcp-session :id "test-session")))
       (lem-mcp-server::register-all-methods server)
       ,@body)))

(defun mcp-request (method &optional params)
  "Send an MCP request directly to handlers (bypassing HTTP).
Returns the result alist or signals an error."
  (let* ((handlers (lem-mcp-server::mcp-server-method-handlers *test-mcp-server*))
         (handler (gethash method handlers)))
    (unless handler
      (error "Unknown MCP method: ~A" method))
    (lem-mcp-server::call-mcp-method handler params)))

(defun mcp-tool-call (tool-name &optional arguments)
  "Call an MCP tool by name with optional arguments.
Returns the tools/call response."
  (mcp-request "tools/call"
               `(("name" . ,tool-name)
                 ("arguments" . ,(or arguments '())))))

(defun lines (&rest strings)
  "Join strings with newlines."
  (format nil "~{~A~%~}" strings))

(defun extract-text-content (tool-response)
  "Extract the text content from a tools/call response."
  (let* ((content (cdr (assoc "content" tool-response :test #'string=)))
         (first-item (first content)))
    (when first-item
      (cdr (assoc "text" first-item :test #'string=)))))

(defun extract-result (response key)
  "Extract a value from an MCP response by key."
  (cdr (assoc key response :test #'string=)))

(defun result-is-error-p (tool-response)
  "Check if a tools/call response indicates an error."
  (let ((is-error (cdr (assoc "isError" tool-response :test #'string=))))
    (and is-error (not (eq is-error :false)))))
