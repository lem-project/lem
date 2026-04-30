(defpackage :lem-tests/lsp-mode/mock-client
  (:use :cl)
  (:import-from :lem-language-client/client
                :client
                :client-connection)
  (:import-from :lem-language-client/request
                :request-async)
  (:import-from :lem-lsp-base/type
                :request-message
                :request-message-method)
  (:export :mock-client
           :mock-client-responses
           :mock-client-request-history
           :mock-client-should-error-p
           :set-mock-client-response
           :clear-mock-client-responses
           :make-mock-client))
(in-package :lem-tests/lsp-mode/mock-client)

(defclass mock-client (client)
  ((responses
    :initform (make-hash-table :test 'equal)
    :accessor mock-client-responses
    :documentation "Hash table mapping method names to canned responses")
   (request-history
    :initform '()
    :accessor mock-client-request-history
    :documentation "List of (method params) pairs for introspection")
   (should-error-p
    :initform nil
    :initarg :should-error-p
    :accessor mock-client-should-error-p
    :documentation "If T, error callback will be called instead of success"))
  (:documentation "A mock LSP client that returns pre-configured responses."))

(defun make-mock-client (&key responses should-error-p)
  "Create a new mock client with optional pre-configured responses.
RESPONSES should be an alist of (method-name . response)."
  (let ((client (make-instance 'mock-client :should-error-p should-error-p)))
    (when responses
      (loop :for (method . response) :in responses
            :do (set-mock-client-response client method response)))
    client))

(defun set-mock-client-response (client method response)
  "Set a canned response for a specific method.
METHOD should be a string like \"textDocument/completion\"."
  (setf (gethash method (mock-client-responses client)) response))

(defun clear-mock-client-responses (client)
  "Clear all canned responses and request history."
  (clrhash (mock-client-responses client))
  (setf (mock-client-request-history client) '()))

(defmethod lem-language-client/client:jsonrpc-connect ((client mock-client))
  "No-op for mock client - no actual connection needed."
  t)

(defmethod request-async ((client mock-client) (message request-message) params callback
                          &optional error-callback)
  "Mock implementation that calls callback synchronously with canned response.
This bypasses the event queue for simpler testing."
  (let ((method (request-message-method message)))
    ;; Record the request for later inspection
    (push (list :method method :params params)
          (mock-client-request-history client))
    ;; Either error or return canned response
    (if (mock-client-should-error-p client)
        (when error-callback
          (funcall error-callback "Mock error" -32000))
        (let ((response (gethash method (mock-client-responses client))))
          (funcall callback response)))))
