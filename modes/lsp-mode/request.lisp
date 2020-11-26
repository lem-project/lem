(defpackage :lem-lsp-mode/request
  (:use :cl)
  (:import-from :lem-lsp-mode/utils)
  (:import-from :lem-lsp-mode/json-lsp-utils
                :make-json
                :coerce-json
                :object-to-json)
  (:import-from :lem-lsp-mode/client
                :client-connection)
  (:import-from :lem-lsp-mode/type
                :ts-array)
  (:export :request
           :request-async
           :initialize-request
           :initialized-request
           :text-document-did-open
           :text-document-did-change
           :hover-request
           :completion-request
           :signature-help
           :definition
           :type-definition
           :implementation
           :references
           :document-highlight
           :document-symbol
           :document-formatting
           :document-range-formatting))
(in-package :lem-lsp-mode/request)

(cl-package-locks:lock-package :lem-lsp-mode/request)

(lem-lsp-mode/project:local-nickname :protocol :lem-lsp-mode/protocol)
(lem-lsp-mode/project:local-nickname :utils :lem-lsp-mode/utils)

(defvar *log-stream* nil)
(defvar *log-mutex* (bt:make-lock))

(defun do-log (string &rest args)
  (when *log-stream*
    (bt:with-lock-held (*log-mutex*)
      (fresh-line *log-stream*)
      (apply #'format *log-stream* string args)
      (terpri *log-stream*))))

(defun pretty-json (params)
  (with-output-to-string (stream)
    (yason:encode params (yason:make-json-output-stream stream))))

(defun do-request-log (method params)
  (do-log "request: ~A ~A" method (pretty-json params)))

(defun do-response-log (response)
  (do-log "response: ~A" (pretty-json response)))

(defun do-error-response-log (condition)
  (do-log "response: ~A" condition))

(defun jsonrpc-call (jsonrpc method params)
  (handler-bind ((jsonrpc/errors:jsonrpc-callback-error
                   #'do-error-response-log))
    (do-request-log method params)
    (let ((response (jsonrpc:call jsonrpc method params)))
      (do-response-log response)
      response)))

(defun jsonrpc-call-async (jsonrpc method params callback)
  (handler-bind ((jsonrpc/errors:jsonrpc-callback-error
                   #'do-error-response-log))
    (do-request-log method params)
    (jsonrpc:call-async jsonrpc
                        method
                        params
                        (lambda (response)
                          (do-response-log response)
                          (funcall callback response)))))

(defun jsonrpc-notify (jsonrpc method params)
  (do-log "notify: ~A ~A" method (pretty-json params))
  (jsonrpc:notify jsonrpc method params))

(defclass abstract-request ()
  ((method
    :initarg :method
    :reader request-method)
   (params
    :initarg :params
    :reader request-params)))

(defclass request (abstract-request)
  ((response-class-name
    :initarg :response-class-name
    :reader request-response-class-name)))

(defclass notification (abstract-request)
  ())

(defun coerce-response (request response)
  (coerce-json response (request-response-class-name request)))

(defmethod request (client (request request))
  (coerce-response request
                   (jsonrpc-call (client-connection client)
                                 (request-method request)
                                 (object-to-json (request-params request)))))

(defmethod request-async (client (request request) callback)
  (jsonrpc-call-async (client-connection client)
                      (request-method request)
                      (object-to-json (request-params request))
                      (lambda (response)
                        (let ((value (coerce-response request response)))
                          (funcall callback value)))))

(defmethod request (client (request notification))
  (jsonrpc-notify (client-connection client)
                  (request-method request)
                  (object-to-json (request-params request))))

(defclass initialize-request (request)
  ((params :type protocol:initialize-params))
  (:default-initargs
   :method "initialize"
   :response-class-name 'protocol:initialize-result))

(defclass initialized-request (notification)
  ((params :type protocol:initialized-params))
  (:default-initargs
   :method "initialized"
   :params (make-instance 'protocol:initialized-params)))

(defclass text-document-did-open (notification)
  ((params :type protocol:did-open-text-document-params))
  (:default-initargs
   :method "textDocument/didOpen"))

(defclass text-document-did-change (notification)
  ((params :type protocol:did-change-text-document-params))
  (:default-initargs
   :method "textDocument/didChange"))

(defclass hover-request (request)
  ((params :type protocol:hover-params))
  (:default-initargs
   :method "textDocument/hover"
   :response-class-name '(or null protocol:hover)))

(defclass completion-request (request)
  ((params :type protocol:completion-params))
  (:default-initargs
   :method "textDocument/completion"
   :response-class-name '(or
                          (ts-array protocol:completion-item)
                          protocol:completion-list
                          null)))

(defclass signature-help (request)
  ((params :type protocol:signature-help-params))
  (:default-initargs
   :method "textDocument/signatureHelp"
   :response-class-name '(or protocol:signature-help null)))

(defclass definition (request)
  ((params :type protocol:definition-params))
  (:default-initargs
   :method "textDocument/definition"
   :response-class-name '(or
                          protocol:location
                          (ts-array protocol:location)
                          (ts-array protocol:location-link)
                          null)))

(defclass type-definition (request)
  ((params :type protocol:type-definition-params))
  (:default-initargs
   :method "textDocument/typeDefinition"
   :response-class-name '(or
                          protocol:location
                          (ts-array protocol:location)
                          (ts-array protocol:location-link)
                          null)))

(defclass implementation (request)
  ((params :type protocol:implementation-params))
  (:default-initargs
   :method "textDocument/implementation"
   :response-class-name '(or
                          protocol:location
                          (ts-array protocol:location)
                          (ts-array protocol:location-link)
                          null)))

(defclass references (request)
  ((parmas :type protocol:reference-params))
  (:default-initargs
   :method "textDocument/references"
   :response-class-name '(or
                          (ts-array protocol:location)
                          null)))

(defclass document-highlight (request)
  ((params :type protocol:document-highlight-params))
  (:default-initargs
   :method "textDocument/documentHighlight"
   :response-class-name '(or
                          (ts-array protocol:document-highlight)
                          null)))

(defclass document-symbol (request)
  ((params :type protocol:document-symbol-params))
  (:default-initargs
   :method "textDocument/documentSymbol"
   :response-class-name '(or
                          (ts-array protocol:document-symbol)
                          (ts-array protocol:symbol-information)
                          null)))

(defclass document-formatting (request)
  ((params :type protocol:document-formatting-params))
  (:default-initargs
   :method "textDocument/formatting"
   :response-class-name '(or
                          (ts-array protocol:text-edit)
                          null)))

(defclass document-range-formatting (request)
  ((params :type protocol:document-range-formatting-params))
  (:default-initargs
   :method "textDocument/rangeFormatting"
   :response-class-name '(or
                          (ts-array protocol:text-edit)
                          null)))

;;; TODO
;;; response-class-nameのnullは特定のjsonライブラリに依存していないか確認する
