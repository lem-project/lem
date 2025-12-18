(defpackage #:lem-living-canvas/lsp-call-hierarchy
  (:use #:cl)
  (:import-from #:alexandria #:when-let)
  (:local-nicknames (:request :lem-language-client/request)
                    (:lsp :lem-lsp-base/protocol-3-17))
  (:export
   ;; LSP request functions (Lem-specific)
   #:get-document-symbols
   #:prepare-call-hierarchy
   #:get-incoming-calls
   #:get-outgoing-calls
   ;; Buffer utilities (Lem-specific)
   #:buffer-has-call-hierarchy-p
   ;; Document symbol utilities (Lem-specific)
   #:document-symbol-to-position
   #:document-symbol-is-callable-p
   #:flatten-document-symbols))
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
;;; Document symbol utilities
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
