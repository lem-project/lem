(cl-lsp/defpackage:defpackage :cl-lsp/protocol
  (:use :cl)
  (:export :convert-from-hash-table
           :convert-to-hash-table))
(in-package :cl-lsp/protocol)

(defvar null-slot-value (make-symbol "NULL"))

(defvar *protocol-symbols* '())

(defclass protocol () ())

(defmacro define-interface (name parent &body slots)
  `(progn
     (push ',name *protocol-symbols*)
     (export ',(cons name (mapcar #'first slots)))
     (defclass ,name ,(if (null parent)
                          `(protocol)
                          parent)
       ,(mapcar (lambda (slot)
                  (let ((slot-symbol (first slot))
                        (type (getf (rest slot) :type))
                        (optional (getf (rest slot) :optional))
                        (documentation (getf (rest slot) :documentation)))
                    `(,slot-symbol
                      :initarg ,(intern (string slot-symbol) :keyword)
                      ,@(if type
                            `(:type ,type))
                      ,@(if optional
                            `(:initform null-slot-value))
                      ,@(if documentation
                            `(:documentation ,documentation)))))
                slots))))

(deftype |DocumentUri| () 'string)

(define-interface |Position| ()
  (|line| :type number)
  (|character| :type number))

(define-interface |Range| ()
  (|start| :type |Position|)
  (|end| :type |Position|))

(define-interface |Location| ()
  (|uri| :type |DocumentUri|)
  (|range| :type |Range|))

(define-interface |Diagnostic| ()
  (|range| :type |Range|)
  (|severity| :optional t :type (or null number))
  (|code| :optional t :type (or null number string))
  (|source| :optional t :type (or null string))
  (|message| :type string))

(export (defparameter |DiagnosticSeverity.Error| 1))
(export (defparameter |DiagnosticSeverity.Warning| 2))
(export (defparameter |DiagnosticSeverity.Information| 3))
(export (defparameter |DiagnosticSeverity.Hint| 4))

(define-interface |Command| ()
  (|title| :type string)
  (|command| :type string)
  (|arguments| :type list))

(define-interface |TextEdit| ()
  (|range| :type |Range|)
  (|newText| :type string))

(define-interface |TextDocumentEdit| ()
  (|textDocument| :type |VersionedTextDocumentIdentifier|)
  (|edits| :type (trivial-types:proper-list |TextEdit|)))

(define-interface |WorkspaceEdit| ()
  (|changes| :optional t)
  (|documentChanges| :optional t :type (trivial-types:proper-list |TextDocumentEdit|)))

(define-interface |TextDocumentIdentifier| ()
  (|uri| :type |DocumentUri|))

(define-interface |TextDocumentItem| ()
  (|uri| :type |DocumentUri|)
  (|languageId| :type string)
  (|version| :type number)
  (|text| :type string))

(define-interface |VersionedTextDocumentIdentifier|
    (|TextDocumentIdentifier|)
  (|version| :type number))

(define-interface |TextDocumentPositionParams| ()
  (|textDocument| :type |TextDocumentIdentifier|)
  (|position| :type |Position|))

(define-interface |DocumentFilter| ()
  (|language| :optional t :type string)
  (|scheme| :optional t :type string)
  (|pattern| :optional t :type string))

(deftype |DocumentSelector| ()
  '(trivial-types:proper-list |DocumentFilter|))

(define-interface |InitializeParams| ()
  (|processId| :type (or number null))
  (|rootPath| :type (or string null))
  (|rootUri| :type (or |DocumentUri| null))
  (|initializationOptions| :optional t)
  (|capabilities| :type |ClientCapabilities|)
  (|trace| :optional t))

(define-interface |WorkspaceClientCapabilites| ()
  (|applyEdit| :optional t :type boolean)
  (|didChangeConfiguration| :optional t)
  (|didChangeWatchedFiles| :optional t)
  (|symbol| :optional t)
  (|executeCommand| :optional t))

(define-interface |TextDocumentClientCapabilities| ()
  (|synchronization| :optional t)
  (|completion| :optional t)
  (|hover| :optional t)
  (|signatureHelp| :optional t)
  (|references| :optional t)
  (|documentHighlight| :optional t)
  (|documentSymbol| :optional t)
  (|formatting| :optional t)
  (|rangeFormatting| :optional t)
  (|onTypeFormatting| :optional t)
  (|definition| :optional t)
  (|codeAction| :optional t)
  (|codeLens| :optional t)
  (|documentLink| :optional t)
  (|rename| :optional t))

(define-interface |ClientCapabilities| ()
  (|workspace| :optional t :type |WorkspaceClientCapabilites|)
  (|textDocument| :optional t :type |TextDocumentClientCapabilities|)
  (|experimental| :optional t :type t))

(define-interface |InitializeResult| ()
  (|capabilities| :type |ServerCapabilities|))

(define-interface |InitializeError| ()
  (|retry| :type boolean))

(export (defparameter |TextDocumentSyncKind.None| 0))
(export (defparameter |TextDocumentSyncKind.Full| 1))
(export (defparameter |TextDocumentSyncKind.Incremental| 2))

(define-interface |CompletionOptions| ()
  (|resolveProvider| :optional t :type boolean)
  (|triggerCharacters| :optional t :type (trivial-types:proper-list string)))

(define-interface |SignatureHelpOptions| ()
  (|triggerCharacters| :optional t :type (trivial-types:proper-list string)))

(define-interface |CodeLensOptions| ()
  (|resolveProvider| :optional t :type boolean))

(define-interface |DocumentOnTypeFormattingOptions| ()
  (|firstTriggerCharacter| :type string)
  (|moreTriggerCharacter| :optional t :type (trivial-types:proper-list string)))

(define-interface |DocumentLinkOptions| ()
  (|resolveProvider| :optional t :type boolean))

(define-interface |ExecuteCommandOptions| ()
  (|commands| :type (trivial-types:proper-list string)))

(define-interface |SaveOptions| ()
  (|includeText| :optional t :type boolean))

(define-interface |TextDocumentSyncOptions| ()
  (|openClose| :optional t :type boolean)
  (|change| :optional t :type number)
  (|willSave| :optional t :type boolean)
  (|willSaveWaitUntil| :optional t :type boolean)
  (|save| :optional t :type |SaveOptions|))

(define-interface |ServerCapabilities| ()
  (|textDocumentSync| :optional t :type (or |TextDocumentSyncOptions| number))
  (|hoverProvider| :optional t :type boolean)
  (|completionProvider| :optional t :type |CompletionOptions|)
  (|signatureHelpProvider| :optional t :type |SignatureHelpOptions|)
  (|definitionProvider| :optional t :type boolean)
  (|referencesProvider| :optional t :type boolean)
  (|documentHighlightProvider| :optional t :type boolean)
  (|documentSymbolProvider| :optional t :type boolean)
  (|workspaceSymbolProvider| :optional t :type boolean)
  (|codeActionProvider| :optional t :type boolean)
  (|codeLensProvider| :optional t :type |CodeLensOptions|)
  (|documentFormattingProvider| :optional t :type boolean)
  (|documentRangeFormattingProvider| :optional t :type boolean)
  (|documentOnTypeFormattingProvider| :optional t :type |DocumentOnTypeFormattingOptions|)
  (|renameProvider| :optional t :type boolean)
  (|documentLinkProvider| :optional t :type |DocumentLinkOptions|)
  (|executeCommandProvider| :optional t :type |ExecuteCommandOptions|)
  (|experimental| :optional t :type t))

(define-interface |ShowMessageParams| ()
  (|type| :type number)
  (|message| :type string))

(export (defparameter |MessageType.Error| 1))
(export (defparameter |MessageType.Warning| 2))
(export (defparameter |MessageType.Info| 3))
(export (defparameter |MessageType.Log| 4))

(define-interface |ShowMessageRequestParams| ()
  (|type| :type number)
  (|message| :type string)
  (|actions| :optional t :type (trivial-types:proper-list |MessageActionItem|)))

(define-interface |MessageActionItem| ()
  (|title| :type string))

(define-interface |LogMessageParams| ()
  (|type| :type number)
  (|message| :type string))

(define-interface |Registration| ()
  (|id| :type string)
  (|method| :type string)
  (|registerOptions| :optional t))

(define-interface |RegistrationParams| ()
  (|registrations| :type (trivial-types:proper-list |Registration|)))

(define-interface |TextDocumentRegistrationOptions| ()
  (|documentSelector| :type (or |DocumentSelector| |null|)))

(define-interface |TextDocumentChangeRegistrationOptions| (|TextDocumentRegistrationOptions|)
  (|syncKind| :type number))

(define-interface |Unregistration| ()
  (|id| :type string)
  (|method| :type string))

(define-interface |UnregistrationParams| ()
  (|unregisterations| :type (trivial-types:proper-list |Unregistration|)))

(define-interface |DidOpenTextDocumentParams| ()
  (|textDocument| :type |TextDocumentItem|))

(define-interface |DidChangeTextDocumentParams| ()
  (|textDocument| :type |VersionedTextDocumentIdentifier|)
  (|contentChanges| :type (trivial-types:proper-list |TextDocumentContentChangeEvent|)))

(define-interface |TextDocumentContentChangeEvent| ()
  (|range| :optional t :type |Range|)
  (|rangeLength| :optional t :type number)
  (|text| :type string))

(define-interface |DidSaveTextDocumentParams| ()
  (|textDocument| :type |TextDocumentIdentifier|)
  (|text| :optional t :type string))

(define-interface |DidCloseTextDocumentParams| ()
  (|textDocument| :type |TextDocumentIdentifier|))

(define-interface |PublishDiagnosticsParams| ()
  (|uri| :type |DocumentUri|)
  (|diagnostics| :type (trivial-types:proper-list |Diagnostic|)))

(define-interface |CompletionList| ()
  (|isIncomplete| :type boolean)
  (|items| :type (trivial-types:proper-list |CompletionItem|)))

(define-interface |CompletionItem| ()
  (|label| :type string)
  (|kind| :optional t :type number)
  (|detail| :optional t :type string)
  (|documentation| :optional t :type string)
  (|sortText| :optional t :type string)
  (|filterText| :optional t :type string)
  (|insertText| :optional t :type string)
  (|insertTextFormat| :optional t :type integer)
  (|textEdit| :optional t :type |TextEdit|)
  (|additionalTextEdits| :optional t :type (trivial-types:proper-list |TextEdit|))
  (|command| :optional t :type |Command|)
  (|data| :optional t :type t))

(define-interface |Hover| ()
  (|contents| :type t)
  (|range| :optional t :type |Range|))

(define-interface |SignatureHelp| ()
  (|signatures| :type (trivial-types:proper-list |SignatureInformation|))
  (|activeSignature| :optional t :type number)
  (|activeParameter| :optional t :type number))

(define-interface |SignatureInformation| ()
  (|label| :type string)
  (|documentation| :optional t :type string)
  (|parameters| :optional t :type (trivial-types:proper-list |ParameterInformation|)))

(define-interface |ParameterInformation| ()
  (|label| :type string)
  (|documentation| :optional t :type string))

(define-interface |ReferenceParams| (|TextDocumentPositionParams|)
 (|context| :type |ReferenceContext|))

(define-interface |ReferenceContext| ()
  (|includeDeclaration| :type boolean))

(define-interface |DocumentHighlight| ()
  (|range| :type |Range|)
  (|kind| :optional t :type number))

(export (defparameter |DocumentHighlightKind.Text| 1))
(export (defparameter |DocumentHighlightKind.Read| 2))
(export (defparameter |DocumentHighlightKind.Write| 3))

(define-interface |DocumentSymbolParams| ()
  (|textDocument| :type |TextDocumentIdentifier|))

(define-interface |SymbolInformation| ()
  (|name| :type string)
  (|kind| :type number)
  (|location| :type |Location|)
  (|containerName| :optional t :type string))

(export (defparameter |SymbolKind.File| 1))
(export (defparameter |SymbolKind.Module| 2))
(export (defparameter |SymbolKind.Namespace| 3))
(export (defparameter |SymbolKind.Package| 4))
(export (defparameter |SymbolKind.Class| 5))
(export (defparameter |SymbolKind.Method| 6))
(export (defparameter |SymbolKind.Property| 7))
(export (defparameter |SymbolKind.Field| 8))
(export (defparameter |SymbolKind.Constructor| 9))
(export (defparameter |SymbolKind.Enum| 10))
(export (defparameter |SymbolKind.Interface| 11))
(export (defparameter |SymbolKind.Function| 12))
(export (defparameter |SymbolKind.Variable| 13))
(export (defparameter |SymbolKind.Constant| 14))
(export (defparameter |SymbolKind.String| 15))
(export (defparameter |SymbolKind.Number| 16))
(export (defparameter |SymbolKind.Boolean| 17))
(export (defparameter |SymbolKind.Array| 18))

(define-interface |WorkspaceSymbolParams| ()
  (|query| :type string))

(define-interface |CodeLensParams| ()
  (|textDocument| :type |TextDocumentIdentifier|))

(define-interface |CodeLens| ()
  (|range| :type |Range|)
  (|command| :optional t :type |Command|)
  (|data| :optional t :type t))

(define-interface |DocumentLinkParams| ()
  (|textDocument| :type |TextDocumentIdentifier|))

(define-interface |DocumentLink| ()
  (|range| :type |Range|)
  (|target| :optional t :type |DocumentUri|))

(define-interface |DocumentFormattingParams| ()
  (|textDocument| :type |TextDocumentIdentifier|)
  (|options| :type |FormattingOptions|))

(define-interface |FormattingOptions| ()
  (|tabSize| :type number)
  (|insertSpaces| :type boolean)
  ;; [key: string]: boolean | number | string;
  )

(define-interface |DocumentRangeFormattingParams| ()
  (|textDocument| :type |TextDocumentIdentifier|)
  (|range| :type |Range|)
  (|options| :type |FormattingOptions|))

(define-interface |DocumentOnTypeFormattingParams| ()
  (|textDocument| :type |TextDocumentIdentifier|)
  (|position| :type |Position|)
  (|ch| :type string)
  (|options| :type |FormattingOptions|))

(define-interface |RenameParams| ()
  (|textDocument| :type |TextDocumentIdentifier|)
  (|position| :type |Position|)
  (|newName| :type string))

(defun protocol-symbol-p (type)
  (when (member type *protocol-symbols*)
    type))

(defun protocol-list-p (type)
  (and (consp type)
       (eq 'trivial-types:proper-list (car type))
       (protocol-symbol-p (second type))))

(defun maybe-protocol-type (type hash-value)
  (cond ((and (symbolp type)
              (protocol-symbol-p type)
              (hash-table-p hash-value))
         (convert-from-hash-table type hash-value))
        ((protocol-list-p type)
         (mapcar (lambda (hash-value-1)
                   (convert-from-hash-table (second type) hash-value-1))
                 hash-value))
        ((and (consp type)
              (eq 'or (first type)))
         (some (lambda (type-1)
                 (maybe-protocol-type type-1 hash-value))
               (rest type)))))

(defgeneric convert-from-hash-table (name hash-table)
  (:method (name hash-table)
    (make-instance name)
    (let ((object (make-instance name)))
      (loop :for slot :in (c2mop:class-slots (find-class name))
        :for slot-name := (c2mop:slot-definition-name slot)
        :for slot-type := (c2mop:slot-definition-type slot)
        :for hash-key := (string slot-name)
        :do (setf (slot-value object slot-name)
                  (let ((hash-value (gethash hash-key hash-table)))
                    (or (maybe-protocol-type slot-type hash-value)
                        hash-value))))
      object)))

(defgeneric convert-to-hash-table (instance)
  (:method (instance)
    (let ((hash-table (make-hash-table :test 'equal)))
      (loop :for slot :in (c2mop:class-slots (find-class (type-of instance)))
            :for name := (c2mop:slot-definition-name slot)
            :for type := (c2mop:slot-definition-type slot)
            :for value := (slot-value instance name)
            :do
            (unless (eq value null-slot-value)
              (setf (gethash (string name) hash-table)
                    (cond
                      ((protocol-symbol-p type)
                       (convert-to-hash-table value))
                      ((and (protocol-list-p type) (listp value))
                       (mapcar #'convert-to-hash-table value))
                      ((and (consp type)
                            (eq 'or (car type))
                            (typep value 'protocol))
                       (convert-to-hash-table value))
                      (t
                       value)))))
      hash-table)))

(defmethod convert-to-hash-table ((instance |WorkspaceEdit|))
  (let ((hash-table (make-hash-table :test 'equal)))
    (unless (eq null-slot-value (slot-value instance '|changes|))
      (let ((changes (slot-value instance '|changes|))
            (new-changes (make-hash-table :test 'equal)))
        (maphash (lambda (uri edits)
                   (setf (gethash uri new-changes)
                         (mapcar #'convert-to-hash-table edits)))
                 changes)
        (setf (gethash "changes" hash-table) new-changes)))
    (unless (eq null-slot-value (slot-value instance '|documentChanges|))
      (let ((doucment-changes (slot-value instance '|documentChanges|)))
        (setf (gethash "documentChanges" hash-table)
              (mapcar #'convert-to-hash-table doucment-changes))))
    hash-table))
