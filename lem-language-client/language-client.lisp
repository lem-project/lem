(in-package :lem-language-client)

(defvar *client* (jsonrpc:make-client))

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

(defun method-initialize ()
  (let* ((root-path (namestring (probe-file ".")))
         (response (jsonrpc:call *client*
                                 "initialize"
                                 ({}
                                  "processId" (getpid)
                                  "rootPath" root-path
                                  "rootUri" (format nil "file://~A" root-path)
                                  #|"initializationOptions"|#
                                  "capabilities" (client-capabilities)
                                  #|"trace" "off"|#
                                  #|"workspaceFolders" nil|#))))
    (gethash "capabilities" response)))

(defun start ()
  (jsonrpc:client-connect *client* :mode :tcp :port 4389)
  (method-initialize))
