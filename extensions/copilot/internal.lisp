(uiop:define-package :lem-copilot/internal
  (:use :cl)
  (:export :copilot-root
           :copilot-path
           :hash
           :pretty-json
           :run-agent
           :connect
           :initialize
           :set-editor-info
           :sign-in-initiate
           :sign-in-confirm
           :check-status
           :text-document/did-open
           :text-document/did-close
           :text-document/did-change
           :text-document/did-focus
           :get-completions
           :notify-shown
           :notify-accepted
           :notify-rejected
           :get-completions-cycling))
(in-package :lem-copilot/internal)

(defparameter *logging-output* t)
(defparameter *logging-request* nil)

(defvar *logs* (lem/common/ring:make-ring 1000))
(defvar *log-lock* (bt2:make-lock :name "copilot log lock"))

(defgeneric copilot-root ())

(defun copilot-path ()
  (merge-pathnames "lib/node_modules/copilot-node-server/copilot/dist/agent.js"
                   (copilot-root)))

(defun hash (&rest args)
  (alexandria:plist-hash-table args :test 'equal))

(defun pretty-json (params)
  (with-output-to-string (stream)
    (yason:encode params (yason:make-json-output-stream stream))))

(defun do-log (output)
  (bt2:with-lock-held (*log-lock*)
    (lem/common/ring:ring-push *logs* output)))

(defun debug-log (type method params)
  (when *logging-request*
    (do-log (format nil "~A ~A ~A~%" type method (pretty-json params)))))

(defun logger (output)
  (when (search "MaxListenersExceededWarning: Possible EventEmitter memory leak detected. 11 change listeners added to [EventEmitter]. MaxListeners is 10. Use emitter.setMaxListeners() to increase limit" output)
    (defparameter $ t))
  (do-log output))

(defun write-log (stream)
  (loop :for i :downfrom (1- (lem/common/ring:ring-length *logs*)) :to 0
        :for log := (lem/common/ring:ring-ref *logs* i)
        :do (write-line log stream)))

(defstruct agent
  client
  process
  stream)

(defun run-agent ()
  (let* ((process
           (async-process:create-process
            (list "node" (namestring (copilot-path)) "--stdio")))
         (stream (lem-lsp-mode/async-process-stream:make-input-stream
                  process
                  :logger (when *logging-output* 'logger)))
         (client (jsonrpc:make-client)))
    (make-agent :client client
                :process process
                :stream stream)))

(defun connect (agent)
  (jsonrpc/client:client-connect-using-class (agent-client agent)
                                             'lem-lsp-mode/lem-stdio-transport:lem-stdio-transport
                                             :process (agent-process agent)
                                             :stream (agent-stream agent)))

(defun request (agent method params)
  (debug-log :request method params)
  (jsonrpc:call (agent-client agent) method params))

(defun request-async (agent method params &key callback error-callback)
  (debug-log :request-async method params)
  (jsonrpc:call-async (agent-client agent)
                      method
                      params
                      callback
                      error-callback))

(defun notify (agent method params)
  (debug-log :notify method params)
  (jsonrpc:notify (agent-client agent) method params))

(defun initialize (agent)
  (request agent
           "initialize"
           (hash "capabilities"
                 (hash "workspace"
                       (hash "workspaceFolders" t)))))

(defun set-editor-info (agent)
  (request agent
           "setEditorInfo"
           (hash "editorInfo"
                 (hash "name" "Lem"
                       "version" (lem:get-version-string))
                 "editorPluginInfo" (hash "name" "lem-copilot"
                                          "version" "0.0"))))

(defun sign-in-initiate (agent)
  (request agent
           "signInInitiate"
           (hash)))

(defun sign-in-confirm (agent user-code &key callback error-callback)
  (request-async agent
                 "signInConfirm"
                 (hash "userCode" user-code)
                 :callback callback
                 :error-callback error-callback))

(defun check-status (agent)
  (request agent
           "checkStatus"
           (hash)))

(defun text-document/did-open (agent &key uri language-id version text)
  (notify agent
          "textDocument/didOpen"
          (hash "textDocument" (hash "uri" uri
                                     "languageId" language-id
                                     "version" version
                                     "text" text))))

(defun text-document/did-close (agent &key uri)
  (notify agent
          "textDocument/didClose"
          (hash "textDocument" (hash "uri" uri))))

(defun text-document/did-change (agent &key uri version content-changes)
  (notify agent
          "textDocument/didChange"
          (hash "textDocument" (hash "uri" uri
                                     "version" version)
                "contentChanges" content-changes)))

(defun text-document/did-focus (agent &key uri)
  (notify agent
          "textDocument/didFocus"
          (hash "textDocument" (hash "uri" uri))))

(defun get-completions (agent &key doc callback error-callback)
  (request-async agent
                 "getCompletions"
                 (hash "doc" doc)
                 :callback callback
                 :error-callback error-callback))

(defun notify-shown (agent uuid)
  (request-async agent "notifyShown" (hash "uuid" uuid)))

(defun notify-accepted (agent uuid)
  (request-async agent
                 "notifyAccepted"
                 (hash "uuid" uuid)
                 :error-callback #'default-error-callback))

(defun notify-rejected (agent uuid)
  (request-async agent
                 "notifyRejected"
                 (hash "uuids" (vector uuid))
                 :error-callback #'default-error-callback))

(defun get-completions-cycling (agent &key doc callback error-callback)
  (request-async agent
                 "getCompletionsCycling"
                 (hash "doc" doc)
                 :callback callback
                 :error-callback error-callback))

(defun default-error-callback (&rest args)
  (lem:send-event (lambda () (error "~A" args))))
