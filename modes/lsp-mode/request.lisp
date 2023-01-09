(defpackage :lem-lsp-mode/request
  (:use :cl
        :lem-language-server/protocol/lsp-type)
  (:import-from :lem-lsp-mode/client
                :client-connection)
  (:import-from :lem-lsp-mode/utils)
  (:import-from :lem-language-server/protocol/converter
                :convert-from-json
                :convert-to-json)
  (:export :request
           :request-async
           :initialize-request
           :initialized-request
           :text-document-did-open
           :text-document-did-change
           :text-document-did-save
           :text-document-did-close
           :execute-command
           :hover-request
           :completion-request
           :signature-help
           :definition
           :type-definition
           :implementation
           :references
           :document-highlight
           :document-symbol
           :code-action
           :document-formatting
           :document-range-formatting
           :document-on-type-formatting
           :rename))
(in-package :lem-lsp-mode/request)

(cl-package-locks:lock-package :lem-lsp-mode/request)

(defvar *log-pathname* (merge-pathnames "lsp.log" (lem:lem-home)))
(defvar *log-enable* nil)
(defvar *log-mutex* (bt:make-lock))

(defun do-log (string &rest args)
  (when *log-enable*
    (bt:with-lock-held (*log-mutex*)
      (with-open-file (out *log-pathname*
                           :direction :output
                           :if-exists :append
                           :if-does-not-exist :create)
        (fresh-line out)
        (apply #'format out string args)
        (terpri out)))))

(defun pretty-json (params)
  (with-output-to-string (stream)
    (yason:encode params (yason:make-json-output-stream stream))))

(defun do-request-log (method params &key (from :client))
  (check-type from (member :client :server))
  (do-log "~:[<-~;->~] request: ~A ~A" (eq from :client) method (pretty-json params)))

(defun do-response-log (response &key (from :client))
  (check-type from (member :client :server))
  (do-log "~:[->~;<-~] response: ~A" (eq from :client) (pretty-json response)))

(defun do-error-response-log (condition &key (from :client))
  (check-type from (member :client :server))
  (do-log "~:[->~;<-~] response: ~A" (eq from :client) condition))

(defun jsonrpc-call (jsonrpc method params)
  (handler-bind ((jsonrpc/errors:jsonrpc-callback-error
                   #'do-error-response-log))
    (do-request-log method params)
    (let ((response (jsonrpc:call jsonrpc method params)))
      (do-response-log response)
      response)))

(defun jsonrpc-call-async (jsonrpc method params callback &optional error-callback)
  (handler-bind ((jsonrpc/errors:jsonrpc-callback-error
                   #'do-error-response-log))
    (do-request-log method params)
    (jsonrpc:call-async jsonrpc
                        method
                        params
                        (lambda (response)
                          (do-response-log response)
                          (funcall callback response))
                        error-callback)))

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
  (convert-from-json response
                     (request-response-class-name request)))

(defmethod request (client (request request))
  (coerce-response request
                   (jsonrpc-call (client-connection client)
                                 (request-method request)
                                 (convert-to-json (request-params request)))))

(defmethod request-async (client (request request) callback &optional error-callback)
  (jsonrpc-call-async (client-connection client)
                      (request-method request)
                      (convert-to-json (request-params request))
                      (lambda (response)
                        (let ((value (coerce-response request response)))
                          (funcall callback value)))
                      error-callback))

(defmethod request (client (request notification))
  (jsonrpc-notify (client-connection client)
                  (request-method request)
                  (convert-to-json (request-params request))))

(defclass initialize-request (request)
  ((params :type lsp:initialize-params))
  (:default-initargs
   :method "initialize"
   :response-class-name 'lsp:initialize-result))

(defclass initialized-request (notification)
  ((params :type lsp:initialized-params))
  (:default-initargs
   :method "initialized"
   :params (make-instance 'lsp:initialized-params)))

(defclass text-document-did-open (notification)
  ((params :type lsp:did-open-text-document-params))
  (:default-initargs
   :method "textDocument/didOpen"))

(defclass text-document-did-change (notification)
  ((params :type lsp:did-change-text-document-params))
  (:default-initargs
   :method "textDocument/didChange"))

(defclass text-document-did-save (notification)
  ((params :type lsp:did-save-text-document-params))
  (:default-initargs
   :method "textDocument/didSave"))

(defclass text-document-did-close (notification)
  ((params :type lsp:did-close-text-document-params))
  (:default-initargs
   :method "textDocument/didClose"))

(defclass execute-command (request)
  ((params :type lsp:execute-command-params))
  (:default-initargs
   :method "workspace/executeCommand"))

(defclass hover-request (request)
  ((params :type lsp:hover-params))
  (:default-initargs
   :method "textDocument/hover"
   :response-class-name '(or lsp-null lsp:hover)))

(defclass completion-request (request)
  ((params :type lsp:completion-params))
  (:default-initargs
   :method "textDocument/completion"
   :response-class-name '(or
                          (lsp-array lsp:completion-item)
                          lsp:completion-list
                          lsp-null)))

(defclass signature-help (request)
  ((params :type lsp:signature-help-params))
  (:default-initargs
   :method "textDocument/signatureHelp"
   :response-class-name '(or lsp:signature-help lsp-null)))

(defclass definition (request)
  ((params :type lsp:definition-params))
  (:default-initargs
   :method "textDocument/definition"
   :response-class-name '(or
                          lsp:location
                          (lsp-array lsp:location)
                          (lsp-array lsp:location-link)
                          lsp-null)))

(defclass type-definition (request)
  ((params :type lsp:type-definition-params))
  (:default-initargs
   :method "textDocument/typeDefinition"
   :response-class-name '(or
                          lsp:location
                          (lsp-array lsp:location)
                          (lsp-array lsp:location-link)
                          lsp-null)))

(defclass implementation (request)
  ((params :type lsp:implementation-params))
  (:default-initargs
   :method "textDocument/implementation"
   :response-class-name '(or
                          lsp:location
                          (lsp-array lsp:location)
                          (lsp-array lsp:location-link)
                          lsp-null)))

(defclass references (request)
  ((parmas :type lsp:reference-params))
  (:default-initargs
   :method "textDocument/references"
   :response-class-name '(or
                          (lsp-array lsp:location)
                          lsp-null)))

(defclass document-highlight (request)
  ((params :type lsp:document-highlight-params))
  (:default-initargs
   :method "textDocument/documentHighlight"
   :response-class-name '(or
                          (lsp-array lsp:document-highlight)
                          lsp-null)))

(defclass document-symbol (request)
  ((params :type lsp:document-symbol-params))
  (:default-initargs
   :method "textDocument/documentSymbol"
   :response-class-name '(or
                          (lsp-array lsp:document-symbol)
                          (lsp-array lsp:symbol-information)
                          lsp-null)))

(defclass code-action (request)
  ((params :type lsp:code-action-params))
  (:default-initargs
   :method "textDocument/codeAction"
   :response-class-name '(or
                          lsp:command
                          (lsp-array lsp:code-action)
                          lsp-null)))

(defclass document-formatting (request)
  ((params :type lsp:document-formatting-params))
  (:default-initargs
   :method "textDocument/formatting"
   :response-class-name '(or
                          (lsp-array lsp:text-edit)
                          lsp-null)))

(defclass document-range-formatting (request)
  ((params :type lsp:document-range-formatting-params))
  (:default-initargs
   :method "textDocument/rangeFormatting"
   :response-class-name '(or
                          (lsp-array lsp:text-edit)
                          lsp-null)))

(defclass document-on-type-formatting (request)
  ((params :type lsp:document-on-type-formatting-params))
  (:default-initargs
   :method "textDocument/onTyepFormatting"
   :response-class-name '(or
                          (lsp-array lsp:text-edit)
                          lsp-null)))

(defclass rename (request)
  ((params :type lsp:rename-params))
  (:default-initargs
   :method "textDocument/rename"
   :response-class-name '(or
                          lsp:workspace-edit
                          lsp-null)))

;;; TODO
;;; response-class-nameのnullは特定のjsonライブラリに依存していないか確認する
