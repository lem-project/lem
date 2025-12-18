(defpackage #:lem-living-canvas/lsp-call-hierarchy
  (:use #:cl)
  (:import-from #:alexandria #:when-let)
  (:local-nicknames (:request :lem-language-client/request))
  (:export
   ;; LSP request functions
   #:get-document-symbols
   #:prepare-call-hierarchy
   #:get-incoming-calls
   #:get-outgoing-calls
   ;; Conversion functions
   #:call-hierarchy-item-to-node-alist
   #:incoming-call-to-edge-alist
   #:outgoing-call-to-edge-alist
   ;; Utilities
   #:buffer-has-call-hierarchy-p
   #:make-node-id-from-item))
(in-package #:lem-living-canvas/lsp-call-hierarchy)

;;; LSP Call Hierarchy Wrapper
;;;
;;; This module provides wrapper functions for LSP Call Hierarchy protocol.
;;; It converts LSP data types to intermediate alist format compatible with
;;; the call-graph JSON format.

;;; ============================================================
;;; Internal utilities
;;; ============================================================

(defun buffer-workspace (buffer)
  "Get the LSP workspace for BUFFER, or nil if not connected."
  (lem-lsp-mode/lsp-mode::buffer-workspace buffer nil))

(defun workspace-client (workspace)
  "Get the LSP client from WORKSPACE."
  (lem-lsp-mode/lsp-mode::workspace-client workspace))

(defun workspace-server-capabilities (workspace)
  "Get server capabilities from WORKSPACE."
  (lem-lsp-mode/lsp-mode::workspace-server-capabilities workspace))

(defun make-text-document-identifier (buffer)
  "Create a TextDocumentIdentifier for BUFFER."
  (lem-lsp-mode/lsp-mode::make-text-document-identifier buffer))

(defun make-text-document-position-params (buffer point)
  "Create TextDocumentPositionParams for BUFFER at POINT."
  (make-instance 'lem-lsp-base/protocol-3-17:text-document-position-params
                 :text-document (make-text-document-identifier buffer)
                 :position (lem-lsp-base/utils:point-to-lsp-position point)))

;;; ============================================================
;;; Capability checking
;;; ============================================================

(defun buffer-has-call-hierarchy-p (buffer)
  "Return T if the LSP server for BUFFER supports call hierarchy."
  (when-let ((workspace (buffer-workspace buffer)))
    (handler-case
        (let ((caps (workspace-server-capabilities workspace)))
          (lem-lsp-base/protocol-3-17:server-capabilities-call-hierarchy-provider caps))
      (unbound-slot () nil))))

(defun buffer-has-document-symbol-p (buffer)
  "Return T if the LSP server for BUFFER supports document symbols."
  (when-let ((workspace (buffer-workspace buffer)))
    (handler-case
        (let ((caps (workspace-server-capabilities workspace)))
          (lem-lsp-base/protocol-3-17:server-capabilities-document-symbol-provider caps))
      (unbound-slot () nil))))

;;; ============================================================
;;; Task B-1: LSP Request Functions
;;; ============================================================

(defun get-document-symbols (buffer)
  "Get all document symbols from BUFFER using textDocument/documentSymbol.

Returns a list of DocumentSymbol or SymbolInformation objects, or nil if
the server doesn't support this feature or buffer is not connected."
  (when-let ((workspace (buffer-workspace buffer)))
    (when (buffer-has-document-symbol-p buffer)
      (request:request
       (workspace-client workspace)
       (make-instance 'lem-lsp-base/protocol-3-17:text-document/document-symbol)
       (make-instance 'lem-lsp-base/protocol-3-17:document-symbol-params
                      :text-document (make-text-document-identifier buffer))))))

(defun prepare-call-hierarchy (buffer point)
  "Prepare call hierarchy at POINT in BUFFER.

Returns a list of CallHierarchyItem objects, or nil if the server doesn't
support this feature or no item is found at the position."
  (when-let ((workspace (buffer-workspace buffer)))
    (when (buffer-has-call-hierarchy-p buffer)
      (request:request
       (workspace-client workspace)
       (make-instance 'lem-lsp-base/protocol-3-17:text-document/prepare-call-hierarchy)
       (make-instance 'lem-lsp-base/protocol-3-17:call-hierarchy-prepare-params
                      :text-document (make-text-document-identifier buffer)
                      :position (lem-lsp-base/utils:point-to-lsp-position point))))))

(defun get-incoming-calls (item)
  "Get incoming calls (callers) for a CallHierarchyItem.

ITEM should be a CallHierarchyItem returned from prepare-call-hierarchy.
Returns a list of CallHierarchyIncomingCall objects."
  ;; Note: We need the workspace from the original buffer context.
  ;; For now, we use the current buffer's workspace.
  (when-let ((workspace (buffer-workspace (lem:current-buffer))))
    (request:request
     (workspace-client workspace)
     (make-instance 'lem-lsp-base/protocol-3-17:call-hierarchy/incoming-calls)
     (make-instance 'lem-lsp-base/protocol-3-17:call-hierarchy-incoming-calls-params
                    :item item))))

(defun get-outgoing-calls (item)
  "Get outgoing calls (callees) for a CallHierarchyItem.

ITEM should be a CallHierarchyItem returned from prepare-call-hierarchy.
Returns a list of CallHierarchyOutgoingCall objects."
  (when-let ((workspace (buffer-workspace (lem:current-buffer))))
    (request:request
     (workspace-client workspace)
     (make-instance 'lem-lsp-base/protocol-3-17:call-hierarchy/outgoing-calls)
     (make-instance 'lem-lsp-base/protocol-3-17:call-hierarchy-outgoing-calls-params
                    :item item))))

;;; ============================================================
;;; Task B-2: CallHierarchyItem → alist Conversion
;;; ============================================================

(defun uri-to-filepath (uri)
  "Convert a file URI to a local file path.

Example: \"file:///path/to/file.go\" -> \"/path/to/file.go\""
  (if (and uri (>= (length uri) 7) (string= "file://" (subseq uri 0 7)))
      (subseq uri 7)
      uri))

(defun make-node-id-from-item (item)
  "Create a unique node ID from a CallHierarchyItem.

Format: \"<uri>:<name>:<line>\" to ensure uniqueness for overloaded functions."
  (let ((uri (lem-lsp-base/protocol-3-17:call-hierarchy-item-uri item))
        (name (lem-lsp-base/protocol-3-17:call-hierarchy-item-name item))
        (range (lem-lsp-base/protocol-3-17:call-hierarchy-item-selection-range item)))
    (format nil "~A:~A:~A"
            uri
            name
            (lem-lsp-base/protocol-3-17:position-line
             (lem-lsp-base/protocol-3-17:range-start range)))))

(defun symbol-kind-to-type-keyword (kind)
  "Convert LSP SymbolKind to a type keyword for call-graph."
  (case kind
    (#.lem-lsp-base/protocol-3-17:symbol-kind-function :function)
    (#.lem-lsp-base/protocol-3-17:symbol-kind-method :method)
    (#.lem-lsp-base/protocol-3-17:symbol-kind-constructor :constructor)
    (#.lem-lsp-base/protocol-3-17:symbol-kind-class :class)
    (#.lem-lsp-base/protocol-3-17:symbol-kind-interface :interface)
    (#.lem-lsp-base/protocol-3-17:symbol-kind-module :module)
    (#.lem-lsp-base/protocol-3-17:symbol-kind-namespace :namespace)
    (#.lem-lsp-base/protocol-3-17:symbol-kind-property :property)
    (#.lem-lsp-base/protocol-3-17:symbol-kind-field :field)
    (#.lem-lsp-base/protocol-3-17:symbol-kind-variable :variable)
    (#.lem-lsp-base/protocol-3-17:symbol-kind-constant :constant)
    (t :function)))

(defun call-hierarchy-item-to-node-alist (item)
  "Convert a CallHierarchyItem to a node alist for call-graph JSON format.

Returns an alist with keys: id, name, package, type, docstring, arglist,
sourceFile, sourceLine."
  (let* ((name (lem-lsp-base/protocol-3-17:call-hierarchy-item-name item))
         (kind (lem-lsp-base/protocol-3-17:call-hierarchy-item-kind item))
         (uri (lem-lsp-base/protocol-3-17:call-hierarchy-item-uri item))
         (detail (handler-case
                     (lem-lsp-base/protocol-3-17:call-hierarchy-item-detail item)
                   (unbound-slot () nil)))
         (range (lem-lsp-base/protocol-3-17:call-hierarchy-item-selection-range item))
         (line (1+ (lem-lsp-base/protocol-3-17:position-line
                    (lem-lsp-base/protocol-3-17:range-start range))))
         (filepath (uri-to-filepath uri)))
    (list (cons "id" (make-node-id-from-item item))
          (cons "name" name)
          (cons "package" (or (pathname-name filepath) ""))
          (cons "type" (string-downcase
                        (symbol-name (symbol-kind-to-type-keyword kind))))
          (cons "docstring" nil)
          (cons "arglist" detail)
          (cons "sourceFile" filepath)
          (cons "sourceLine" line))))

(defun incoming-call-to-edge-alist (incoming-call target-id)
  "Convert a CallHierarchyIncomingCall to an edge alist.

INCOMING-CALL is the LSP response object.
TARGET-ID is the node ID of the target (the function being called).

Returns an alist with keys: source, target, callType."
  (let* ((from-item (lem-lsp-base/protocol-3-17:call-hierarchy-incoming-call-from
                     incoming-call))
         (source-id (make-node-id-from-item from-item)))
    (list (cons "source" source-id)
          (cons "target" target-id)
          (cons "callType" "direct"))))

(defun outgoing-call-to-edge-alist (outgoing-call source-id)
  "Convert a CallHierarchyOutgoingCall to an edge alist.

OUTGOING-CALL is the LSP response object.
SOURCE-ID is the node ID of the source (the function making the call).

Returns an alist with keys: source, target, callType."
  (let* ((to-item (lem-lsp-base/protocol-3-17:call-hierarchy-outgoing-call-to
                   outgoing-call))
         (target-id (make-node-id-from-item to-item)))
    (list (cons "source" source-id)
          (cons "target" target-id)
          (cons "callType" "direct"))))

;;; ============================================================
;;; Additional utilities for document symbols
;;; ============================================================

(defun document-symbol-to-position (doc-symbol buffer)
  "Get the point position from a DocumentSymbol for use with prepare-call-hierarchy.

Returns a point at the start of the symbol's selection range."
  (let* ((range (lem-lsp-base/protocol-3-17:document-symbol-selection-range doc-symbol))
         (start (lem-lsp-base/protocol-3-17:range-start range))
         (line (lem-lsp-base/protocol-3-17:position-line start))
         (character (lem-lsp-base/protocol-3-17:position-character start)))
    (lem:with-point ((point (lem:buffer-point buffer)))
      (lem:move-to-line point (1+ line))
      (lem:line-offset point 0 character)
      (lem:copy-point point :temporary))))

(defun document-symbol-is-callable-p (doc-symbol)
  "Return T if the DocumentSymbol represents a callable (function, method, etc.)."
  (let ((kind (lem-lsp-base/protocol-3-17:document-symbol-kind doc-symbol)))
    (member kind (list lem-lsp-base/protocol-3-17:symbol-kind-function
                       lem-lsp-base/protocol-3-17:symbol-kind-method
                       lem-lsp-base/protocol-3-17:symbol-kind-constructor))))

(defun flatten-document-symbols (symbols)
  "Flatten nested DocumentSymbols into a flat list.

DocumentSymbol can have children; this recursively collects all symbols."
  (let ((result '()))
    (labels ((collect (syms)
               (dolist (sym (coerce syms 'list))
                 (push sym result)
                 (handler-case
                     (let ((children (lem-lsp-base/protocol-3-17:document-symbol-children sym)))
                       (when children
                         (collect children)))
                   (unbound-slot () nil)))))
      (collect symbols))
    (nreverse result)))
