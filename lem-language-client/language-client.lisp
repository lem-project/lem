(in-package :lem-language-client)

(defparameter *root-path* (probe-file "."))
(defparameter *language-id* "go")

(defparameter *log-stream* *error-output*)

(defparameter |TextDocumentSaveReason.Manual| 1)
(defparameter |TextDocumentSaveReason.AfterDelay| 2)
(defparameter |TextDocumentSaveReason.FocusOut| 3)

(defvar *workspaces* '())
(defvar *response-methods* '())

(defstruct workspace
  connection
  server-capabilities
  root
  language-id
  (file-version-table (make-hash-table)))

(defmacro define-response-method (name (&rest vars) &body body)
  (alexandria:with-gensyms (params)
    `(pushnew
      (defun ,name (,params)
        (let ,(mapcar (lambda (var)
                        `(,var (gethash ,(string var) ,params)))
                      vars)
          ,@body))
      *response-methods*)))

(defun pathname-to-uri (pathname)
  (format nil "file://~A" (truename pathname)))

(defun {} (&rest plist)
  (alexandria:plist-hash-table plist :test 'equal))

(defun merge-table (parent child)
  (maphash (lambda (key value)
             (setf (gethash key child) value))
           parent)
  child)

(defun find-workspace (root-path)
  (find root-path *workspaces*
        :test #'equal
        :key #'workspace-root))

(defun buffer-language-id (buffer)
  (lem:mode-name (lem:buffer-major-mode buffer)))

(defun buffer-workspace (buffer)
  (lem:buffer-value buffer 'workspace))

(defun (setf buffer-workspace) (workspace buffer)
  (setf (lem:buffer-value buffer 'workspace) workspace))

(defun buffer-file-version (buffer)
  (gethash buffer (workspace-file-version-table (buffer-workspace buffer)) 0))

(defun (setf buffer-file-version) (value buffer)
  (setf (gethash buffer (workspace-file-version-table (buffer-workspace buffer))) value))

(defun buffer-uri (buffer)
  (pathname-to-uri (lem:buffer-filename buffer)))

(defun lsp-position (point)
  ({} "line" (1- (lem:line-number-at-point point))
      "character" (lem:point-charpos point)))

(defun lsp-range (start end)
  (when (lem:point< end start)
    (rotatef start end))
  ({} "start" (lsp-position start)
      "end" (lsp-position end)))

(defun lsp-location (start end)
  (let ((buffer (lem:point-buffer start)))
    ({} "uri" (buffer-uri buffer)
        "range" (lsp-range start end))))

(defun text-document-identifier (buffer)
  ({} "uri" (pathname-to-uri (lem:buffer-filename buffer))))

(defun text-document-item (buffer)
  ({} "uri" (buffer-uri buffer)
      "languageId" (workspace-language-id (buffer-workspace buffer))
      "version" (buffer-file-version buffer)
      "text" (lem:points-to-string (lem:buffer-start-point buffer)
                                   (lem:buffer-end-point buffer))))

(defun versioned-text-document-identifier (buffer)
  (let ((text-document-identifier (text-document-identifier buffer))
        (version (buffer-file-version buffer)))
    (merge-table text-document-identifier
                 ({} "version" version))))

(defun text-document-position-params (point)
  ({} "textDocument" (text-document-identifier (lem:point-buffer point))
      "position" (lsp-position point)))

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

(defun initialize (workspace)
  (let* ((root (workspace-root workspace))
         (response (jsonrpc:call (workspace-connection workspace)
                                 "initialize"
                                 ({}
                                  "processId" (getpid)
                                  #|"rootPath" root|#
                                  "rootUri" (pathname-to-uri root)
                                  #|"initializationOptions"|#
                                  "capabilities" (client-capabilities)
                                  #|"trace" "off"|#
                                  #|"workspaceFolders" nil|#))))
    (setf (workspace-server-capabilities workspace)
          (gethash "capabilities" response))))

(defun initialized (workspace)
  (jsonrpc:notify (workspace-connection workspace) "initialized" ({})))

(defun shutdown (workspace)
  (jsonrpc:call (workspace-connection workspace) "shutdown" ({})))

(defun exit (workspace)
  (jsonrpc:notify (workspace-connection workspace) "exit" ({})))

(define-response-method |window/showMessage| (|type| |message|)
  (declare (ignore |type|))
  (lem:message "~A" |message|))

(define-response-method |window/showMessageRequest| (|type| |message|)
  (declare (ignore |type|))
  (lem:message "~A" |message|))

(define-response-method |window/logMessage| (|type| |message|)
  (format *log-stream* "~A: ~A" |type| |message|))

(defun text-document-did-open (buffer)
  (jsonrpc:notify (workspace-connection (buffer-workspace buffer))
                  "textDocument/didOpen"
                  ({} "textDocument" (text-document-item buffer))))

(defun text-document-did-change (buffer changes)
  (jsonrpc:notify (workspace-connection (buffer-workspace buffer))
                  "textDocument/didChange"
                  ({} "textDocument" (versioned-text-document-identifier buffer)
                      "contentChanges" changes)))

(defun text-document-will-save (buffer &optional (reason |TextDocumentSaveReason.Manual|))
  (jsonrpc:notify (workspace-connection (buffer-workspace buffer))
                  "textDocument/willSave"
                  ({} "textDocument"
                      "reason" reason)))

(defun text-document-will-save-wait-until (buffer &optional (reason |TextDocumentSaveReason.Manual|))
  (jsonrpc:notify (workspace-connection (buffer-workspace buffer))
                  "textDocument/willSaveWaitUntil"
                  ({} "textDocument"
                      "reason" reason)))

(defun text-document-content-change-event (point string-or-number)
  (let ((start-position (lsp-position point)))
    (etypecase string-or-number
      (string
       ({} "range" ({} "start" start-position
                       "end" start-position)
           "rangeLength" 0
           "text" string-or-number))
      (number
       (lem:with-point ((end point))
         (lem:character-offset end string-or-number)
         (let ((end-position (lsp-position end)))
           ({} "range" ({} "start" start-position
                           "end" end-position)
               "rangeLength" (lem:count-characters end point)
               "text" "")))))))

(defun hover-contents-to-string (contents)
  (typecase contents
    (string contents)
    (list
     (with-output-to-string (out)
       (dolist (c contents)
         (write-string (hover-contents-to-string c) out)
         (terpri out))))
    (hash-table
     (gethash "value" contents ""))
    (otherwise
     "")))

(defun hover (point)
  (let ((workspace (buffer-workspace (lem:point-buffer point))))
    (handler-case
        (let ((hover (jsonrpc:call (workspace-connection workspace)
                                   "textDocument/hover"
                                   (text-document-position-params point))))
          (let ((contents (gethash "contents" hover)))
            (hover-contents-to-string contents)))
      (jsonrpc:jsonrpc-error (e)
        (jsonrpc:jsonrpc-error-message e)))))

(defun on-change (point arg)
  (let ((buffer (lem:point-buffer point)))
    (incf (buffer-file-version buffer))
    (text-document-did-change buffer
                              (list (text-document-content-change-event
                                     point
                                     (if (characterp arg)
                                         (string arg)
                                         arg))))))

(defun initialize-hooks (buffer)
  (lem:add-hook (lem:variable-value 'lem:before-change-functions :buffer buffer) 'on-change)
  (lem:add-hook (lem:variable-value 'lem:before-save-hook :buffer buffer) 'text-document-will-save))

(defun start (buffer)
  (let* ((root-path *root-path*)
         (workspace (find-workspace root-path)))
    (cond
      (workspace
       (setf (buffer-workspace buffer) workspace))
      (t
       (let* ((connection (jsonrpc:make-client))
              (workspace (make-workspace :connection connection
                                         :root root-path
                                         :language-id *language-id*)))
         (push workspace *workspaces*)
         (setf (buffer-workspace buffer) workspace)
         (dolist (response-method *response-methods*)
           (jsonrpc:expose connection (string response-method) response-method))
         (jsonrpc:client-connect (workspace-connection workspace)
                                 :mode :tcp
                                 :port 4389)
         (initialize workspace)
         (initialized workspace)
         workspace))))
  (text-document-did-open buffer)
  (initialize-hooks buffer)
  (values))

(defun wrap-text-1 (str width)
  (setq str (concatenate 'string str " "))
  (do* ((len (length str))
        (lines nil)
        (begin-curr-line 0)
        (prev-space 0 pos-space)
        (pos-space (position #\Space str)
                   (when (< (1+ prev-space) len)
                     (position #\Space str :start (1+ prev-space)))))
      ((null pos-space) (progn (push (subseq str begin-curr-line (1- len)) lines) (nreverse lines)))
    (when (> (- pos-space begin-curr-line) width)
      (push (subseq str begin-curr-line prev-space) lines)
      (setq begin-curr-line (1+ prev-space)))))

(defun wrap-text (str width)
  (format nil "~{~A~^~%~}" (wrap-text-1 str width)))

(lem:define-command lsp-hover () ()
  (alexandria:when-let ((message (hover (lem:current-point))))
    (lem:display-popup-message (wrap-text message 80))))
