(in-package :lem-lsp-mode)

(defparameter *root-path* (probe-file "."))
(defparameter *log-stream* *error-output*)

(defparameter |TextDocumentSyncKind.None| 0)
(defparameter |TextDocumentSyncKind.Full| 1)
(defparameter |TextDocumentSyncKind.Incremental| 2)

(defparameter |TextDocumentSaveReason.Manual| 1)
(defparameter |TextDocumentSaveReason.AfterDelay| 2)
(defparameter |TextDocumentSaveReason.FocusOut| 3)

(defvar *workspaces* '())
(defvar *response-methods* '())

(defstruct workspace
  connection
  server-capabilities
  text-document-sync
  root
  language-id
  (triggers (make-hash-table))
  (file-version-table (make-hash-table)))

(defstruct text-document-sync
  open-close
  change
  will-save
  will-save-wait-until
  save)

(defgeneric find-root-directory (mode-name client buffer)
  (:method (mode-name client buffer)
    (declare (ignore mode-name client))
    (lem:buffer-directory buffer)))

(defclass client (jsonrpc:client)
  ((command
    :initarg :command
    :reader client-command)
   (language-id
    :initarg :language-id
    :reader client-language-id)))

(defclass tcp-client (client)
  ((port
    :initarg :port
    :reader client-port)))

(defclass stdio-client (client) ())

(lem:define-minor-mode lsp-mode
    (:name "Language Client"
     :enable-hook 'enable-lsp-mode))

(defun enable-lsp-mode ()
  (setf (lem:variable-value 'lem.language-mode:completion-spec)
        (lem.completion-mode:make-completion-spec
         'completion
         :prefix-search t))
  (setf (lem:variable-value 'lem.language-mode:find-definitions-function)
        'generic-definition)
  (setf (lem:variable-value 'lem.language-mode:find-references-function)
        'references)
  (let* ((buffer (lem:current-buffer))
         (client (get (lem:buffer-major-mode buffer) 'client)))
    (unless client
      (lem:editor-error "undefined client: ~A"
                        (lem:buffer-major-mode buffer)))
    (start-lsp client buffer)))

(defmacro define-response-method (name (&rest vars) &body body)
  (alexandria:with-gensyms (params)
    `(pushnew
      (defun ,name (,params)
        (let ,(mapcar (lambda (var)
                        `(,var (gethash ,(string var) ,params)))
                      vars)
          ,@body))
      *response-methods*)))

(defun ensure-workspace (value)
  (etypecase value
    (lem:buffer (buffer-workspace value))
    (lem:point (buffer-workspace (lem:point-buffer value)))
    (workspace value)))

(defun incremental-sync-p (workspace)
  (setf workspace (ensure-workspace workspace))
  (eql (text-document-sync-change
        (workspace-text-document-sync workspace))
       |TextDocumentSyncKind.Incremental|))

(defun hover-provider-p (workspace)
  (setf workspace (ensure-workspace workspace))
  (values (gethash "hoverProvider" (workspace-server-capabilities workspace))))

(defun completion-provider-p (workspace)
  (setf workspace (ensure-workspace workspace))
  (values (gethash "completionProvider" (workspace-server-capabilities workspace))))

(defun signature-help-provider-p (workspace)
  (setf workspace (ensure-workspace workspace))
  (values (gethash "signatureHelpProvider" (workspace-server-capabilities workspace))))

(defun definition-provider-p (workspace)
  (setf workspace (ensure-workspace workspace))
  (values (gethash "definitionProvider" (workspace-server-capabilities workspace))))

(defun type-definition-provider-p (workspace)
  (setf workspace (ensure-workspace workspace))
  (values (gethash "typeDefinitionProvider" (workspace-server-capabilities workspace))))

(defun implementation-provider-p (workspace)
  (setf workspace (ensure-workspace workspace))
  (values (gethash "implementationProvider" (workspace-server-capabilities workspace))))

(defun reference-provider-p (workspace)
  (setf workspace (ensure-workspace workspace))
  (values (gethash "referencesProvider" (workspace-server-capabilities workspace))))

(defun document-highlight-provider-p (workspace)
  (setf workspace (ensure-workspace workspace))
  (values (gethash "documentHighlightProvider" (workspace-server-capabilities workspace))))

(defun document-symbol-provider-p (workspace)
  (setf workspace (ensure-workspace workspace))
  (values (gethash "documentSymbolProvider" (workspace-server-capabilities workspace))))

(defun get-completion-trigger-characters (workspace)
  (mapcar #'string-to-char
          (-> (workspace-server-capabilities workspace)
              "completionProvider" "triggerCharacters")))

(defun get-signature-help-trigger-characters (workspace)
  (mapcar #'string-to-char
          (-> (workspace-server-capabilities workspace)
              "signatureHelpProvider" "triggerCharacters")))

(defun pathname-to-uri (pathname)
  (let ((filename (namestring pathname)))
    (format nil "file://~A"
            (if (alexandria:starts-with-subseq "~/" filename)
                (merge-pathnames (subseq filename 2) (user-homedir-pathname))
                filename))))

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

(defun buffer-text (buffer)
  (lem:points-to-string (lem:buffer-start-point buffer)
                        (lem:buffer-end-point buffer)))

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

(defun move-to-lsp-position (point position)
  (let-hash (|line| |character|) position
    (lem:move-to-line point (1+ |line|))
    (lem:line-offset point 0 |character|)
    point))

(defun decode-lsp-range (buffer range)
  (let-hash (|start| |end|) range
    (lem:with-point ((start-point (lem:buffer-point buffer))
                     (end-point (lem:buffer-point buffer)))
      (move-to-lsp-position start-point |start|)
      (move-to-lsp-position (lem:move-point end-point start-point) |end|)
      (values start-point end-point))))

(defun decode-lsp-text-edit (buffer text-edit)
  (let-hash (|range| |newText|) text-edit
    (multiple-value-bind (start end)
        (decode-lsp-range buffer |range|)
      (values start end |newText|))))

(defun text-document-identifier (buffer)
  ({} "uri" (buffer-uri buffer)))

(defun text-document-item (buffer)
  ({} "uri" (buffer-uri buffer)
      "languageId" (workspace-language-id (buffer-workspace buffer))
      "version" (buffer-file-version buffer)
      "text" (buffer-text buffer)))

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

(defun convert-text-document-sync (sync)
  (etypecase sync
    (number
     (make-text-document-sync :change sync))
    (hash-table
     (make-text-document-sync :open-close (gethash "openClose" sync)
                              :change (gethash "change" sync)
                              :will-save (gethash "willSave" sync)
                              :will-save-wait-until (gethash "willSaveWaitUntil" sync)
                              :save (gethash "save" sync)))))

(defun initialize (workspace)
  (let* ((root (workspace-root workspace))
         (response (jsonrpc-call (workspace-connection workspace)
                                 "initialize"
                                 ({}
                                  "processId" (getpid)
                                  "rootPath" (princ-to-string root)
                                  "rootUri" (pathname-to-uri root)
                                  #|"initializationOptions"|#
                                  "capabilities" (client-capabilities)
                                  #|"trace" "off"|#
                                  #|"workspaceFolders" nil|#)))
         (server-capabilities (gethash "capabilities" response)))
    (setf (workspace-server-capabilities workspace)
          server-capabilities)
    (alexandria:when-let (sync (gethash "textDocumentSync" server-capabilities))
      (setf (workspace-text-document-sync workspace)
            (convert-text-document-sync sync)))))

(defun initialized (workspace)
  (jsonrpc-notify (workspace-connection workspace) "initialized" ({})))

(defun shutdown (workspace)
  (jsonrpc-call (workspace-connection workspace) "shutdown" ({})))

(defun exit (workspace)
  (jsonrpc-notify (workspace-connection workspace) "exit" ({})))

(define-response-method |window/showMessage| (|type| |message|)
  (declare (ignore |type|))
  (lem:message "~A" |message|))

(define-response-method |window/showMessageRequest| (|type| |message|)
  (declare (ignore |type|))
  (lem:message "~A" |message|))

(define-response-method |window/logMessage| (|type| |message|)
  (format *log-stream* "~A: ~A" |type| |message|))

(defun text-document-did-open (buffer)
  (jsonrpc-notify (workspace-connection (buffer-workspace buffer))
                  "textDocument/didOpen"
                  ({} "textDocument" (text-document-item buffer))))

(defun text-document-did-change (buffer changes)
  (jsonrpc-notify (workspace-connection (buffer-workspace buffer))
                  "textDocument/didChange"
                  ({} "textDocument" (versioned-text-document-identifier buffer)
                      "contentChanges" changes)))

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

(defun text-document-will-save (buffer &optional (reason |TextDocumentSaveReason.Manual|))
  (jsonrpc-notify (workspace-connection (buffer-workspace buffer))
                  "textDocument/willSave"
                  ({} "textDocument"
                      "reason" reason)))

(defun text-document-will-save-wait-until (buffer &optional (reason |TextDocumentSaveReason.Manual|))
  (let ((workspace (buffer-workspace buffer)))
    (jsonrpc-notify (workspace-connection workspace)
                    "textDocument/willSaveWaitUntil"
                    ({} "textDocument"
                        "reason" reason))))

(defun text-document-did-save (buffer)
  (jsonrpc-notify (workspace-connection (buffer-workspace buffer))
                  "textDocument/didSave"
                  ({} "textDocument" (text-document-identifier buffer)
                      #|"text" (buffer-text buffer)|#)))

(defun text-document-did-close (buffer)
  (let* ((workspace (buffer-workspace buffer))
         (file-version-table (workspace-file-version-table workspace)))
    (remhash buffer file-version-table)
    (jsonrpc-notify (workspace-connection workspace)
                    "textDocument/didClose"
                    ({} "textDocument" (text-document-identifier buffer)))
    (when (zerop (hash-table-count file-version-table))
      (shutdown workspace))))

(defun sync-text-document (buffer)
  (unless (incremental-sync-p buffer)
    (let ((file-version (buffer-file-version buffer)))
      (unless (eql file-version
                   (lem:buffer-value buffer 'last-sync-file-version))
        (text-document-did-change buffer (list ({} "text" (buffer-text buffer))))
        (setf (lem:buffer-value buffer 'last-sync-file-version)
              file-version)))))

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

(defun completion-params (point &optional (trigger-kind 1))
  (setq trigger-kind 1)
  (merge-table (text-document-position-params point)
               ({} "triggerKind" trigger-kind
                   #|"triggerCharacter"|#)))

(defun completion-items (buffer items)
  (delete
   nil
   (map 'list
        (lambda (item)
          (let-hash (|label|
                     |kind| |detail| |documentation| |deprecated| |preselect|
                     |sortText| |filterText| |insertText| |insertTextFormat|
                     |textEdit| |additionalTextEdits| |commitCharacters|
                     |command| |data|)
              item
            (declare (ignorable |label|
                                |kind| |detail| |documentation| |deprecated| |preselect|
                                |sortText| |filterText| |insertText| |insertTextFormat|
                                |textEdit| |additionalTextEdits| |commitCharacters|
                                |command| |data|))
            (cond
              (|textEdit|
               (multiple-value-bind (start end newText)
                   (decode-lsp-text-edit buffer |textEdit|)
                 (lem.completion-mode:make-completion-item
                  :label newText :detail |detail| :start start :end end)))
              (|label|
               (lem.completion-mode:make-completion-item :label |label|)))))
        items)))

(defun completion (point)
  (when (completion-provider-p point)
    (sync-text-document (lem:point-buffer point))
    (let* ((workspace (buffer-workspace (lem:point-buffer point)))
           (result (jsonrpc-call (workspace-connection workspace)
                                 "textDocument/completion"
                                 (completion-params point)))
           (buffer (lem:point-buffer point)))
      (etypecase result
        (null)
        (vector
         (completion-items buffer result))
        (hash-table
         (completion-items buffer (gethash "items" result)))))))

(defun hover (point)
  (when (hover-provider-p point)
    (sync-text-document (lem:point-buffer point))
    (let ((workspace (buffer-workspace (lem:point-buffer point))))
      (handler-case
          (let ((hover (jsonrpc-call (workspace-connection workspace)
                                     "textDocument/hover"
                                     (text-document-position-params point))))
            (let ((contents (gethash "contents" hover)))
              (hover-contents-to-string contents)))
        (jsonrpc:jsonrpc-error (e)
          (jsonrpc:jsonrpc-error-message e))))))

(lem:define-attribute signature-help-active-parameter-attribute
  (t :foreground "white" :background "black" :underline-p t :bold-p t))

(defun fill-signature-help-window-with-space (window focus-line)
  (let ((buffer (lem:window-buffer window)))
    (lem:with-point ((p (lem:buffer-point buffer) :left-inserting))
      (lem:buffer-start p)
      (loop :for line :from 0
            :do (lem:line-end p)
                (let ((column (lem:point-virtual-line-column p window)))
                  (let ((offset (- (1- (lem:window-width window))
                                   column)))
                    (when (plusp offset)
                      (lem:insert-string p (make-string offset :initial-element #\space)
                                         :attribute (if (= focus-line line)
                                                        'lem:completion-attribute
                                                        'lem:non-focus-completion-attribute)))))
                (unless (lem:line-offset p 1) (return))))))

(defvar *signature-help-window*)

(defun signature-help-finalize ()
  (lem:delete-window *signature-help-window*)
  (lem:remove-hook lem:*pre-command-hook* 'signature-help-finalize))

(defun make-signature-help-window (signature-help)
  (let* ((buffer (lem:make-buffer "*Signature Help*" :enable-undo-p nil :temporary t))
         (p (lem:buffer-point buffer)))
    (setf (lem:variable-value 'lem::truncate-character :buffer buffer) #\space)
    (lem:erase-buffer buffer)
    (let-hash (|signatures| |activeSignature| |activeParameter|)
        signature-help
      (loop :for signature-index :from 0
            :for signature* :on |signatures|
            :for signature := (first signature*)
            :for active-signature-p := (eql signature-index |activeSignature|)
            :do (let-hash (|label| #||documentation||# |parameters|) signature
                  (lem:insert-string p |label|
                                     :attribute (if active-signature-p
                                                    'lem:completion-attribute
                                                    'lem:non-focus-completion-attribute))
                  (when (and active-signature-p)
                    (alexandria:when-let
                        (parameter (nth (min |activeParameter|
                                             (1- (length |parameters|)))
                                        |parameters|))
                      (lem:with-point ((p p))
                        (lem:line-start p)
                        (let ((label (gethash "label" parameter)))
                          (when (lem:search-forward p label)
                            (lem:with-point ((start p))
                              (lem:character-offset start (- (length label)))
                              (lem:put-text-property
                               start p
                               :attribute 'signature-help-active-parameter-attribute)))))))
                  (when (rest signature*)
                    (lem:insert-character p #\newline))))
      (let ((window (lem-if:display-popup-buffer (lem:current-window) buffer 60 20 nil)))
        (setf *signature-help-window* window)
        (lem:add-hook lem:*pre-command-hook* 'signature-help-finalize)
        (fill-signature-help-window-with-space window |activeSignature|)
        window))))

(defun signature-help (point)
  (when (signature-help-provider-p point)
    (sync-text-document (lem:point-buffer point))
    (let ((workspace (buffer-workspace (lem:point-buffer point))))
      (let ((signature-help (jsonrpc-call (workspace-connection workspace)
                                          "textDocument/signatureHelp"
                                          (text-document-position-params point))))
        signature-help))))

(defun location-to-xref-location (buffer location &optional content)
  (let-hash (|uri| |range|) location
    (multiple-value-bind (start end)
        (decode-lsp-range buffer |range|)
      (declare (ignore end))
      (lem.language-mode:make-xref-location :filespec (quri:uri-path (quri:uri |uri|))
                                            :position start
                                            :content (or content
                                                         (format nil "~A:~D:~D"
                                                                 |uri|
                                                                 (lem:line-number-at-point start)
                                                                 (lem:point-charpos start)))))))

(defun xref-location-equal (xref-1 xref-2)
  (and (equal (lem.language-mode::xref-location-filespec xref-1)
              (lem.language-mode::xref-location-filespec xref-2))
       (lem:point= (lem.language-mode::xref-location-position xref-1)
                   (lem.language-mode::xref-location-position xref-2))))

(flet ((f (point method)
         (sync-text-document (lem:point-buffer point))
         (let ((workspace (buffer-workspace (lem:point-buffer point))))
           (let ((definition (jsonrpc-call (workspace-connection workspace)
                                           method
                                           (text-document-position-params point))))
             (loop :for location :in (uiop:ensure-list definition)
                   :collect (location-to-xref-location (lem:point-buffer point) location))))))
  (defun definition (point)
    (when (definition-provider-p point)
      (f point "textDocument/definition")))
  (defun type-definition (point)
    (when (type-definition-provider-p point)
      (f point "textDocument/typeDefinition")))
  (defun implementation (point)
    (when (implementation-provider-p point)
      (f point "textDocument/implementation"))))

(defun generic-definition (point)
  (let ((xrefs (nconc (definition point)
                      (type-definition point)
                      (implementation point))))
    (delete-duplicates xrefs :test #'xref-location-equal)))

(defun references (point)
  (when (reference-provider-p point)
    (let* ((buffer (lem:point-buffer point))
           (workspace (buffer-workspace buffer)))
      (sync-text-document buffer)
      (let ((result (jsonrpc-call (workspace-connection workspace)
                                  "textDocument/references"
                                  (text-document-position-params point))))
        (lem.language-mode:make-xref-references
         :locations
         (mapcar (lambda (location)
                   (location-to-xref-location buffer location))
                 result))))))

(defun document-highlight (point)
  ;; TODO
  (when (document-highlight-provider-p point)
    #+(or)
    (let* ((buffer (lem:point-buffer point))
           (workspace (buffer-workspace buffer)))
      (sync-text-document buffer)
      (let ((document-highlights
              (jsonrpc-call (workspace-connection workspace)
                            "textDocument/documentHighlight"
                            (text-document-position-params point))))
        (mapcar (lambda (document-highlight)
                  (let-hash (|range| #||kind||#) document-highlight
                    (multiple-value-bind (start end) (decode-lsp-range buffer |range|)
                      )))
                document-highlights)))))

(defun document-symbol-params (buffer)
  ({} "textDocument" (text-document-identifier buffer)))

(defun document-symbol (point)
  (labels ((document-symbol-to-definition (object)
             (let-hash (|name| |range|)
                 object
               (multiple-value-bind (start end)
                   (decode-lsp-range (lem:point-buffer point) |range|)
                 (location-to-xref-location (lem:point-buffer point)
                                            (lsp-location start end)
                                            |name|))))
           (symbol-information-to-definition (object)
             (let-hash (|name| |location|)
                 object
               (location-to-xref-location (lem:point-buffer point) |location| |name|)))
           (symbol-to-definition (object)
             (funcall (if (gethash "range" object)
                          #'document-symbol-to-definition
                          #'symbol-information-to-definition)
                      object)))
    (when (document-symbol-provider-p point)
      (let* ((buffer (lem:point-buffer point))
             (workspace (buffer-workspace buffer)))
        (sync-text-document buffer)
        (let ((document-symbol
                (jsonrpc-call (workspace-connection workspace)
                              "textDocument/documentSymbol"
                              (document-symbol-params (lem:point-buffer point)))))
          ;(do-log "document-symbol: ~A" (pretty-json document-symbol))
          (let* ((origin-window (lem:current-window))
                 (completion-items
                   (mapcar (lambda (xref-location)
                             (lem.completion-mode:make-completion-item
                              :label (lem.language-mode:xref-location-content xref-location)
                              :focus-action (lambda ()
                                              (let* ((filespec
                                                       (lem.language-mode:xref-location-filespec
                                                        xref-location))
                                                     (buffer
                                                       (lem.language-mode:xref-filespec-to-buffer
                                                        filespec))
                                                     (point
                                                       (lem:buffer-point buffer)))
                                                (lem.language-mode:move-to-xref-location-position
                                                 point
                                                 (lem.language-mode:xref-location-position
                                                  xref-location))
                                                point)
                                              (unless (lem:deleted-window-p origin-window)
                                                (lem:window-see origin-window)
                                                (lem.sourcelist:jump-highlighting point)))))
                           (mapcar #'symbol-to-definition document-symbol))))
            (let (select-item)
              (lem:prompt-for-line
               "Document Symbol: " ""
               (lambda (str)
                 (lem:completion str completion-items
                                 :key #'lem.completion-mode:completion-item-label))
               (lambda (str)
                 (setf select-item
                       (find str completion-items
                             :test #'string=
                             :key #'lem.completion-mode:completion-item-label)))
               'mh-document-symbol)
              (when select-item
                (funcall (lem.completion-mode::completion-item-focus-action select-item))))))))))

(defun on-change (point arg)
  (let ((buffer (lem:point-buffer point)))
    (incf (buffer-file-version buffer))
    (when (incremental-sync-p buffer)
      (text-document-did-change buffer
                                (list (text-document-content-change-event
                                       point
                                       (if (characterp arg)
                                           (string arg)
                                           arg)))))))

(defun self-insert-hook (c)
  (alexandria:when-let* ((workspace (buffer-workspace (lem:current-buffer)))
                         (fn (gethash c (workspace-triggers workspace))))
    (funcall fn)))

(defun initialize-buffer (buffer workspace)
  (lem:add-hook (lem:variable-value 'lem:before-change-functions :buffer buffer) 'on-change)
  ;(lem:add-hook (lem:variable-value 'lem:before-save-hook :buffer buffer) 'text-document-will-save)
  (lem:add-hook (lem:variable-value 'lem:after-save-hook :buffer buffer) 'text-document-did-save)
  (lem:add-hook (lem:variable-value 'lem:kill-buffer-hook :buffer buffer) 'text-document-did-close)
  (lem:add-hook (lem:variable-value 'lem:self-insert-after-hook :buffer buffer)
                'self-insert-hook)
  (dolist (c (get-completion-trigger-characters workspace))
    (setf (gethash c (workspace-triggers workspace)) 'lem.language-mode::complete-symbol))
  (dolist (c (get-signature-help-trigger-characters workspace))
    (setf (gethash c (workspace-triggers workspace)) 'lsp-signature-help)))

(defun start-lsp (client buffer)
  (let* ((root-path (find-root-directory (lem:buffer-major-mode buffer) client buffer))
         (workspace (find-workspace root-path)))
    (cond
      (workspace
       (setf (buffer-workspace buffer) workspace))
      (t
       (let ((connection (jsonrpc:make-client)))
         (apply #'jsonrpc:client-connect
                connection
                (etypecase client
                  (tcp-client
                   (list :mode :tcp :port (client-port client)))
                  (stdio-client
                   (list :mode :lem-stdio
                         :program (first (client-command client))
                         :arguments (rest (client-command client))))))
         (setf workspace
               (make-workspace :connection connection
                               :root root-path
                               :language-id (client-language-id client)))
         (setf (buffer-workspace buffer) workspace)
         (dolist (response-method *response-methods*)
           (jsonrpc:expose connection (string response-method) response-method))
         (push workspace *workspaces*)
         (initialize workspace)
         (initialized workspace)
         workspace)))
    (text-document-did-open buffer)
    (initialize-buffer buffer workspace)
    (values)))

(lem:define-command lsp-hover () ()
  (alexandria:when-let ((message (hover (lem:current-point))))
    (lem:display-popup-message (wrap-text message 80))))

(lem:define-command lsp-signature-help () ()
  (let ((signature-help (signature-help (lem:current-point))))
    (make-signature-help-window signature-help)))

(lem:define-command lsp-document-symbol () ()
  (document-symbol (lem:current-point)))

(defmacro define-lsp-client (mode-name (&rest args
                                        &key (mode (error ":mode missing"))
                                        &allow-other-keys))
  (remf args :mode)
  `(progn
     (setf (get ',mode-name 'client)
           ,(ecase mode
              ((:tcp)
               `(make-instance 'tcp-client ,@args))
              ((:stdio)
               `(make-instance 'stdio-client ,@args))))
     ,(when (lem:mode-hook mode-name)
        `(lem:add-hook ,(lem:mode-hook mode-name) 'lsp-mode))))

(define-lsp-client lem-js-mode:js-mode
  (:mode :tcp
   :language-id "javascript"
   :command '("node" "/Users/user/src/javascript-typescript-langserver/lib/language-server")
   :port 2089))

(defmethod find-root-directory ((mode-name (eql 'lem-js-mode:js-mode))
                                client buffer)
  (declare (ignore client))
  (or (find-package-json (lem:buffer-directory buffer))
      (lem:buffer-directory buffer)))

(defun find-package-json (directory)
  (if (uiop:pathname-equal (user-homedir-pathname) directory)
      nil
      (or (dolist (pathname (uiop:directory-files directory))
            (when (and (equal "package" (pathname-name pathname))
                       (equal "json" (pathname-type pathname)))
              (return directory)))
          (find-package-json (uiop:pathname-parent-directory-pathname directory)))))

#+(or)
(define-lsp-client lem-js-mode:js-mode
  (:mode :stdio
   :language-id "javascript"
   :command (list "node"
                  "/Users/user/src/javascript-typescript-langserver/lib/language-server-stdio")))

(define-lsp-client lem-rust-mode:rust-mode
  (:mode :stdio
   :command '("rls")
   :language-id "rust"))
