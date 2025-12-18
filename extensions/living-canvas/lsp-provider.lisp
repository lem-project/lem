(defpackage #:lem-living-canvas/lsp-provider
  (:use #:cl #:call-graph)
  (:import-from #:alexandria #:when-let #:hash-table-keys)
  (:local-nicknames (:lsp-ch :lem-living-canvas/lsp-call-hierarchy))
  (:export #:lsp-call-hierarchy-provider
           #:collect-file-call-hierarchy))
(in-package #:lem-living-canvas/lsp-provider)

;;; LSP Call Hierarchy Provider
;;;
;;; This provider uses the LSP Call Hierarchy API to analyze code.
;;; It works with any language server that supports the Call Hierarchy protocol.

;;; ============================================================
;;; Task C-1: Collect File Call Hierarchy
;;; ============================================================

(defun collect-callable-symbols (buffer)
  "Get all callable symbols (functions, methods, constructors) from BUFFER.

Uses textDocument/documentSymbol to get all symbols, then filters to callables.
Returns a list of DocumentSymbol objects."
  (when-let ((symbols (lsp-ch:get-document-symbols buffer)))
    (let ((all-symbols (lsp-ch:flatten-document-symbols symbols)))
      (remove-if-not #'lsp-ch:document-symbol-is-callable-p all-symbols))))

(defun collect-call-hierarchy-for-symbol (buffer doc-symbol seen-nodes)
  "Collect call hierarchy data for a single DocumentSymbol.

BUFFER is the buffer containing the symbol.
DOC-SYMBOL is the DocumentSymbol to analyze.
SEEN-NODES is a hash table of already-seen node IDs to avoid duplicates.

Returns (values nodes edges) where:
  - nodes is a list of node alists
  - edges is a list of edge alists"
  (let ((nodes '())
        (edges '())
        (point (lsp-ch:document-symbol-to-position doc-symbol buffer)))
    (when-let ((items (lsp-ch:prepare-call-hierarchy buffer point)))
      (let* ((item (elt items 0))
             (node-id (lsp-ch:make-node-id-from-item item)))
        ;; Add this node if not seen
        (unless (gethash node-id seen-nodes)
          (setf (gethash node-id seen-nodes) t)
          (push (lsp-ch:call-hierarchy-item-to-node-alist item) nodes))
        ;; Get incoming calls (callers)
        (when-let ((incoming (lsp-ch:get-incoming-calls item)))
          (dolist (call (coerce incoming 'list))
            (let* ((from-item (lem-lsp-base/protocol-3-17:call-hierarchy-incoming-call-from call))
                   (from-id (lsp-ch:make-node-id-from-item from-item)))
              ;; Add caller node if not seen
              (unless (gethash from-id seen-nodes)
                (setf (gethash from-id seen-nodes) t)
                (push (lsp-ch:call-hierarchy-item-to-node-alist from-item) nodes))
              ;; Add edge
              (push (lsp-ch:incoming-call-to-edge-alist call node-id) edges))))
        ;; Get outgoing calls (callees)
        (when-let ((outgoing (lsp-ch:get-outgoing-calls item)))
          (dolist (call (coerce outgoing 'list))
            (let* ((to-item (lem-lsp-base/protocol-3-17:call-hierarchy-outgoing-call-to call))
                   (to-id (lsp-ch:make-node-id-from-item to-item)))
              ;; Add callee node if not seen
              (unless (gethash to-id seen-nodes)
                (setf (gethash to-id seen-nodes) t)
                (push (lsp-ch:call-hierarchy-item-to-node-alist to-item) nodes))
              ;; Add edge
              (push (lsp-ch:outgoing-call-to-edge-alist call node-id) edges))))))
    (values nodes edges)))

(defun collect-file-call-hierarchy (buffer)
  "Collect call hierarchy for all callable symbols in BUFFER.

Returns a call-graph structure containing all functions and their call relationships.
This function uses the LSP Call Hierarchy API."
  (unless (lsp-ch:buffer-has-call-hierarchy-p buffer)
    (return-from collect-file-call-hierarchy (make-call-graph)))
  (let ((seen-nodes (make-hash-table :test #'equal))
        (all-nodes '())
        (all-edges '())
        (seen-edges (make-hash-table :test #'equal)))
    ;; Collect all callable symbols
    (dolist (doc-symbol (collect-callable-symbols buffer))
      (multiple-value-bind (nodes edges)
          (collect-call-hierarchy-for-symbol buffer doc-symbol seen-nodes)
        (setf all-nodes (nconc nodes all-nodes))
        ;; Deduplicate edges
        (dolist (edge edges)
          (let ((edge-key (format nil "~A->~A"
                                  (cdr (assoc "source" edge :test #'string=))
                                  (cdr (assoc "target" edge :test #'string=)))))
            (unless (gethash edge-key seen-edges)
              (setf (gethash edge-key seen-edges) t)
              (push edge all-edges))))))
    ;; Convert to call-graph structure
    (let ((graph (make-call-graph)))
      (dolist (node-alist all-nodes)
        (add-node graph (alist-to-graph-node node-alist)))
      (dolist (edge-alist all-edges)
        (add-edge graph (alist-to-graph-edge edge-alist)))
      graph)))

;;; ============================================================
;;; Task C-2: LSP Provider Class
;;; ============================================================

(defclass lsp-call-hierarchy-provider (call-graph-provider)
  ()
  (:documentation "Call graph provider using LSP Call Hierarchy API.
Works with any language server that supports the Call Hierarchy protocol."))

(defmethod provider-name ((provider lsp-call-hierarchy-provider))
  :lsp-call-hierarchy)

(defmethod provider-priority ((provider lsp-call-hierarchy-provider))
  20)  ; Higher priority than micros provider (10)

(defmethod provider-supports-p ((provider lsp-call-hierarchy-provider) source)
  "Return T if SOURCE is a buffer with LSP call hierarchy support."
  (and (typep source 'lem:buffer)
       (lsp-ch:buffer-has-call-hierarchy-p source)))

(defmethod provider-analyze ((provider lsp-call-hierarchy-provider) source &key)
  "Analyze SOURCE buffer using LSP Call Hierarchy API.
Returns a call-graph structure."
  (if (typep source 'lem:buffer)
      (collect-file-call-hierarchy source)
      (make-call-graph)))
