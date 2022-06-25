(cl-lsp/defpackage:defpackage :cl-lsp/methods
  (:use :cl
        :cl-lsp/protocol
        :cl-lsp/protocol-util
        :cl-lsp/logger
        :cl-lsp/slime
        :cl-lsp/swank
        :cl-lsp/formatting
        ;; :cl-lsp/server
        :lem-base)
  (:import-from :cl-lsp/server
                :define-method)
  (:import-from :lem-lisp-syntax
                :*syntax-table*
                :search-local-definition)
  (:local-nicknames (:protocol :lem-lsp-utils/protocol)
                    (:json :lem-lsp-utils/json))
  (:export :*initialized-hooks*
           :get-buffer-from-uri
           :with-document-position
           :with-text-document-position))
(in-package :cl-lsp/methods)

(defun get-buffer-from-uri (uri)
  (get-buffer uri))

(defun call-with-document-position (uri position function)
  (let ((buffer (get-buffer-from-uri uri)))
    (assert (bufferp buffer))
    (let ((point (buffer-point buffer)))
      (move-to-lsp-position point position)
      (funcall function point))))

(defmacro with-document-position ((point uri position) &body body)
  `(call-with-document-position ,uri ,position (lambda (,point) ,@body)))

(defun call-with-text-document-position (text-document-position-params function)
  (let ((position (slot-value text-document-position-params '|position|))
        (uri (slot-value (slot-value text-document-position-params '|textDocument|) '|uri|)))
    (call-with-document-position uri position function)))

(defmacro with-text-document-position ((point) params &body body)
  `(call-with-text-document-position ,params (lambda (,point) ,@body)))

(define-method "workspace/symbol" (params protocol:workspace-symbol-params) ()
  (let* ((query (protocol:workspace-symbol-params-query params))
         (limit 42))
    (list-to-object[]
     (when (string/= query "")
       (mapcar #'convert-to-hash-table
               (loop :with package := (find-package "CL-USER")
                     :repeat limit
                     :for name :in (swank-apropos-list query package)
                     :append (symbol-informations name package nil)))))))

(define-method "textDocument/didOpen" (params |DidOpenTextDocumentParams|) ()
  (let ((text-document
         (slot-value params
                     '|textDocument|)))
    (with-slots (|uri| |languageId| |version| |text|)
        text-document
      (let ((buffer (make-buffer |uri|
                                 :enable-undo-p nil
                                 :syntax-table *syntax-table*)))
        (setf (buffer-filename buffer) (uri-to-filename |uri|))
        (insert-string (buffer-point buffer) |text|)
        (setf (buffer-value buffer 'document)
              (list :languageId |languageId|
                    :version |version|)))))
  (values))

(define-method "textDocument/didChange" (params |DidChangeTextDocumentParams|) ()
  (let ((text-document (slot-value params '|textDocument|))
        (content-changes (slot-value params '|contentChanges|)))
    (let* ((buffer (get-buffer (slot-value text-document '|uri|)))
           (point (buffer-point buffer)))
      (dolist (content-change content-changes)
        (with-slots (|range| |rangeLength| |text|)
            content-change
          (cond ((or (null |range|) (null |rangeLength|))
                 (erase-buffer buffer)
                 (insert-string point |text|))
                (t
                 (with-slots (|start|) |range|
                   (move-to-lsp-position point |start|)
                   (delete-character point |rangeLength|)
                   (insert-string point |text|)))))))))

(define-method "textDocument/willSave" () ()
  )

(define-method "textDocument/willSaveWaitUntil" () ()
  )

(define-method "textDocument/didSave" (params |DidSaveTextDocumentParams|) ()
  (let* ((text
          (slot-value params '|text|))
         (text-document
          (slot-value params '|textDocument|))
         (uri
          (slot-value text-document '|uri|))
         (buffer
          (get-buffer uri)))
    (when text
      (erase-buffer buffer)
      (insert-string (buffer-point buffer) text)))
  (values))

(define-method "textDocument/didClose" (params |DidCloseTextDocumentParams|) ()
  (let* ((text-document
          (slot-value params '|textDocument|))
         (uri
          (slot-value text-document '|uri|))
         (buffer
          (get-buffer uri)))
    (delete-buffer buffer))
  (values))

(define-method "textDocument/completion" (params |TextDocumentPositionParams|) ()
  (with-text-document-position (point) params
    (with-point ((start point)
                 (end point))
      (skip-symbol-backward start)
      (skip-symbol-forward end)
      (let ((result
             (fuzzy-completions
              (points-to-string start end)
              (search-buffer-package point))))
        (when result
          (destructuring-bind (completions timeout) result
            (declare (ignore timeout))
            (convert-to-hash-table
             (make-instance
              '|CompletionList|
              :|isIncomplete| nil
              :|items| (loop :for completion :in completions
                             :collect (make-instance
                                       '|CompletionItem|
                                       :|label| (first completion)
                                       ;:|kind|
                                       :|detail| (fourth completion)
                                       ;:|documentation|
                                       ;:|sortText|
                                       ;:|filterText|
                                       ;:|insertText|
                                       ;:|insertTextFormat|
                                       :|textEdit| (make-instance
                                                    '|TextEdit|
                                                    :|range| (make-lsp-range start end)
                                                    :|newText| (first completion))
                                       ;:|additionalTextEdits|
                                       ;:|command|
                                       ;:|data|
                                       ))))))))))

(define-method "textDocument/hover" (params |TextDocumentPositionParams|) ()
  (with-text-document-position (point) params
    (let* ((symbol-string (symbol-string-at-point* point))
           (describe-string
            (describe-symbol symbol-string
                             (search-buffer-package point))))
      (convert-to-hash-table
       (if describe-string
           (with-point ((start point)
                        (end point))
             (skip-chars-backward start #'syntax-symbol-char-p)
             (skip-chars-forward end #'syntax-symbol-char-p)
             (make-instance '|Hover|
                            :|contents| describe-string
                            :|range| (make-lsp-range start end)))
           (make-instance '|Hover|
                          :|contents| ""))))))

(defun arglist (point)
  (loop :with start := (beginning-of-defun-point point 1)
        :while (form-offset point -1)
        :do (when (point< point start)
              (return-from arglist nil)))
  (skip-whitespace-forward point)
  (let ((symbol-string (symbol-string-at-point* point)))
    (when symbol-string
      (operator-arglist symbol-string
                        (search-buffer-package point)))))

(define-method "textDocument/signatureHelp" (params |TextDocumentPositionParams|) ()
  (with-text-document-position (point) params
    (let ((arglist (arglist point)))
      (convert-to-hash-table
       (make-instance
        '|SignatureHelp|
        :|signatures| (when arglist
                        (list (make-instance
                               '|SignatureInformation|
                               :|label| arglist))))))))

(defun xref-location (xref)
  (trivia:match xref
    ((list _
           (list :location
                 (list :file file)
                 (list :position offset)
                 (list :snippet _)))
     (convert-to-hash-table (file-location file offset)))))

(defun xref-locations-from-definitions (defs)
  (loop :for xref :in defs
        :for location := (xref-location xref)
        :when location
        :collect location))

(define-method "textDocument/definition" (params |TextDocumentPositionParams|) ()
  (with-text-document-position (point) params
    (alexandria:when-let ((name (symbol-string-at-point* point)))
      (alexandria:if-let ((p (search-local-definition point name)))
        (convert-to-hash-table (buffer-location p))
        (list-to-object-or-object[]
         (xref-locations-from-definitions
          (find-definitions name (search-buffer-package point))))))))

(define-method "textDocument/references" (params |ReferenceParams|) ()
  (with-text-document-position (point) params
    (let ((symbol-string (symbol-string-at-point* point)))
      (list-to-object-or-object[]
       (loop :for (type . definitions) :in (xrefs symbol-string
                                                  (search-buffer-package point))
             :nconc (xref-locations-from-definitions definitions))))))

(defun collect-symbol-range (buffer name function)
  (let ((regex (ppcre:create-scanner `(:sequence
                                       (:alternation
                                        (:positive-lookbehind
                                         (:char-class #\( #\) #\space #\tab #\:))
                                        :start-anchor)
                                       ,name
                                       (:alternation
                                        (:positive-lookahead
                                         (:char-class #\( #\) #\space #\tab #\:))
                                        :end-anchor))
                                     :case-insensitive-mode t)))
    (with-point ((point (buffer-start-point buffer)))
      (loop :while (search-forward-regexp point regex)
            :collect (with-point ((start point))
                       (character-offset start (- (length name)))
                       (funcall function (make-lsp-range start point)))))))

(defun symbol-name-at-point (point)
  (alexandria:when-let*
      ((string (symbol-string-at-point* point))
       (name (ignore-errors
              (symbol-name
               (let ((*package* (search-buffer-package point)))
                 (read-from-string string))))))
    name))

(define-method "textDocument/documentHighlight" (params |TextDocumentPositionParams|) ()
  (with-text-document-position (point) params
    (list-to-object[]
     (alexandria:when-let (name (symbol-name-at-point point))
       (collect-symbol-range (point-buffer point) name
                             (lambda (range)
                               (convert-to-hash-table
                                (make-instance '|DocumentHighlight|
                                               :|range| range))))))))

(defun type-to-symbol-kind (type)
  #+sbcl
  (case type
    (defvar |SymbolKind.Variable|)
    (defconstant |SymbolKind.Variable|)
    (deftype |SymbolKind.Class|)
    (define-symbol-macro |SymbolKind.Variable|)
    (defmacro |SymbolKind.Function|)
    (define-compiler-macro |SymbolKind.Function|)
    (defun |SymbolKind.Function|)
    (defgeneric |SymbolKind.Method|)
    (defmethod |SymbolKind.Method|)
    (define-setf-expander |SymbolKind.Function|)
    (defstruct |SymbolKind.Class|)
    (define-condition |SymbolKind.Class|)
    (defclass |SymbolKind.Class|)
    (define-method-combination |SymbolKind.Function|)
    (defpackage |SymbolKind.Namespace|)
    (:deftransform |SymbolKind.Function|)
    (:defoptimizer |SymbolKind.Function|)
    (:define-vop |SymbolKind.Function|)
    (:define-source-transform |SymbolKind.Function|)
    (:def-ir1-translator |SymbolKind.Function|)
    (declaim |SymbolKind.Function|)
    (:define-alien-type |SymbolKind.Function|)
    (otherwise
     |SymbolKind.Function|))
  #-sbcl
  |SymbolKind.Function|)

(defun xref-to-symbol-information (name xref buffer-file)
  (trivia:match xref
    ((list (cons type _)
           (list :location
                 (list :file file)
                 (list :position position)
                 (list :snippet _)))
     (when (and (probe-file file)
                (or (null buffer-file)
                    (equal file buffer-file)))
       (make-instance '|SymbolInformation|
                      :|name| name
                      :|kind| (type-to-symbol-kind type)
                      :|location| (file-location file position))))))

(defun symbol-informations (name package buffer-file)
  (loop :for xref :in (find-definitions name package)
        :for info := (xref-to-symbol-information name xref buffer-file)
        :when info
        :collect info))

(defun document-symbol (buffer)
  (let ((symbol-informations '())
        (used (make-hash-table :test 'equal))
        (package (search-buffer-package (buffer-start-point buffer)))
        (buffer-file (buffer-filename buffer)))
    (map-buffer-symbols
     buffer
     (lambda (symbol-string)
       (unless (gethash symbol-string used)
         (setf (gethash symbol-string used) t)
         (dolist (si (symbol-informations symbol-string package buffer-file))
           (push si symbol-informations)))))
    (if (null symbol-informations)
        (vector)
        (mapcar #'convert-to-hash-table
                symbol-informations))))

(define-method "textDocument/documentSymbol" (params |DocumentSymbolParams|) ()
  (let* ((text-document (slot-value params '|textDocument|))
         (uri (slot-value text-document '|uri|))
         (buffer (get-buffer uri)))
    (when buffer
      (document-symbol buffer))))

(define-method "textDocument/formatting" (params |DocumentFormattingParams|) ()
  (with-slots (|textDocument| |options|) params
    (let ((buffer (get-buffer-from-uri (slot-value |textDocument| '|uri|))))
      (buffer-formatting buffer |options|))))

(define-method "textDocument/rangeFormatting" (params |DocumentRangeFormattingParams|) ()
  (with-slots (|textDocument| |range| |options|) params
    (with-slots (|start| |end|) |range|
      (with-document-position (start (slot-value |textDocument| '|uri|) |start|)
        (with-point ((end start))
          (move-to-lsp-position end |end|)
          (range-formatting start end |options|))))))

(define-method "textDocument/onTypeFormatting" (params |DocumentOnTypeFormattingParams|) ()
  (with-slots (|textDocument| |position| |ch| |options|) params
    (with-document-position (point (slot-value |textDocument| '|uri|) |position|)
      (on-type-formatting point |ch| |options|))))

(define-method "textDocument/codeLens" () ()
  (vector))

(define-method "textDocument/documentLink" () ()
  (vector))

(define-method "textDocument/rename" (params |RenameParams|) ()
  (with-slots (|textDocument| |position| |newName|) params
    (with-document-position (point (slot-value |textDocument| '|uri|) |position|)
      (alexandria:when-let ((name (symbol-name-at-point point)))
        (let* ((buffer (point-buffer point))
               (uri (filename-to-uri (buffer-filename buffer)))
               (edits (collect-symbol-range
                       (point-buffer point)
                       name
                       (lambda (range)
                         (make-instance '|TextEdit|
                                        :|range| range
                                        :|newText| |newName|)))))
          (convert-to-hash-table
           (make-instance
            '|WorkspaceEdit|
            :|changes| (alexandria:plist-hash-table (list uri edits))
            ;; :|documentChanges| (list
            ;;                     (make-instance
            ;;                      '|TextDocumentEdit|
            ;;                      :|textDocument| (make-instance
            ;;                                       '|VersionedTextDocumentIdentifier|
            ;;                                       :|version| (buffer-version buffer)
            ;;                                       :|uri| uri)
            ;;                      :|edits| edits))
            )))))))
