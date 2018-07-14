(in-package :lem-language-client)

(defvar *response-methods* '())

(defmacro define-response-method (name (&rest vars) &body body)
  (alexandria:with-gensyms (params)
    `(pushnew
      (defun ,name (,params)
        (let ,(mapcar (lambda (var)
                        `(,var (gethash ,(string var) ,params)))
                      vars)
          ,@body))
      *response-methods*)))

(defun get-root-path () (probe-file "."))

(defparameter *log-stream* *error-output*)

(defstruct workspace
  connection
  server-capabilities
  root)

(defun {} (&rest plist)
  (alexandria:plist-hash-table plist :test 'equal))

(defun workspace-client-capabilities ()
  ({} "applyEdit" 'yason:false
      "workspaceEdit" ({} "documentChanges" 'yason:false)
      "didChangeConfiguration" ({} "dynamicRegistration" 'yason:false)
      "didChangeWatchedFiles" ({} "dynamicRegistration" 'yason:false)
      "symbol" ({} "dynamicRegistration" 'yason:false
                   "symbolKind" ({} "valueSet" #()))
      "executeCommand" ({} "dynamicRegistration" 'yason:false)
      "workspaceFolders" 'yason:false
      "configuration" 'yason:false))

(defun text-document-client-capabilities ()
  ({} "synchronization" ({} "dynamicRegistration" 'yason:false
                            "willSave" 'yason:false
                            "willSaveWaitUntil" 'yason:false
                            "didSave" 'yason:false)
      "completion" ({} "dynamicRegistration" 'yason:false
                       "completionItem" ({} "snippetSupport" 'yason:false
                                            "commitCharacterSupport" 'yason:false
                                            "documentationFormat" #()
                                            "deprecatedSupport" 'yason:false
                                            "preselectSupport" 'yason:false)
                       "completionItemKind" ({} "valueSet" #())
                       "contextSupport" 'yason:false)
      "hover" ({} "dynamicRegistration" 'yason:false
                  "contentFormat" #())
      "signatureHelp" ({} "dynamicRegistration" 'yason:false
                          "signatureInformation" ({} "documentationFormat" #()))
      "references" ({} "dynamicRegistration" 'yason:false)
      "documentHighlight" ({} "dynamicRegistration" 'yason:false)
      "documentSymbol" ({} "dynamicRegistration" 'yason:false
                           "symbolKind" ({} "valueSet" #()))
      "formatting" ({} "dynamicRegistration" 'yason:false)
      "rangeFormatting" ({} "dynamicRegistration" 'yason:false)
      "onTypeFormatting" ({} "dynamicRegistration" 'yason:false)
      "definition" ({} "dynamicRegistration" 'yason:false)
      "typeDefinition" ({} "dynamicRegistration" 'yason:false)
      "implementation" ({} "dynamicRegistration" 'yason:false)
      "codeAction" ({} "dynamicRegistration" 'yason:false
                       "codeActionLiteralSupport" ({}
                                                   "codeActionKind" ({}
                                                                     "valueset" #())))
      "codeLens" ({} "dynamicRegistration" 'yason:false)
      "documentLink" ({} "dynamicRegistration" 'yason:false)
      "colorProvider" ({} "dynamicRegistration" 'yason:false)
      "rename" ({} "dynamicRegistration" 'yason:false)
      "publishDiagnostics" ({} "relatedInformation" 'yason:false)))

(defun client-capabilities ()
  ({} "workspace" (workspace-client-capabilities)
      "textDocument" (text-document-client-capabilities)
      #|"experimental"|#))

(defun method-initialize (workspace)
  (let* ((root (workspace-root workspace))
         (response (jsonrpc:call (workspace-connection workspace)
                                 "initialize"
                                 ({}
                                  "processId" (getpid)
                                  #|"rootPath" root|#
                                  "rootUri" (format nil "file://~A" root)
                                  #|"initializationOptions"|#
                                  "capabilities" (client-capabilities)
                                  #|"trace" "off"|#
                                  #|"workspaceFolders" nil|#))))
    (setf (workspace-server-capabilities workspace)
          (gethash "capabilities" response))))

(defun method-initialized (workspace)
  (jsonrpc:call-async (workspace-connection workspace) "initialized" ({})))

(defun method-shutdown (workspace)
  (jsonrpc:call (workspace-connection workspace) "shutdown" ({})))

(defun method-exit (workspace)
  (jsonrpc:call-async (workspace-connection workspace) "exit" ({})))

(define-response-method |window/showMessage| (|type| |message|)
  (declare (ignore |type|))
  (message "~A" message))

(define-response-method |window/showMessageRequest| (|type| |message|)
  (|window/showMessage| |type| |message|))

(define-response-method |window/logMessage| (|type| |message|)
  (format *log-stream* "~A: ~A" type message))

(defun start ()
  (let* ((connection (jsonrpc:make-client))
         (workspace (make-workspace :connection connection
                                    :root (get-root-path))))
    (dolist (response-method *response-methods*)
      (jsonrpc:expose connection (string response-method) response-method))
    (jsonrpc:client-connect (workspace-connection workspace)
                            :mode :tcp
                            :port 4389)
    (method-initialize workspace)
    (method-initialized workspace)
    workspace))
