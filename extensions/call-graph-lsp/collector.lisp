(in-package #:call-graph-lsp)

;;; Call Graph Collection from LSP Call Hierarchy
;;;
;;; This module provides functions to build call-graph structures from
;;; LSP Call Hierarchy data. The LSP request implementation is abstracted
;;; via callback functions, making this module Lem-independent.

;;; ============================================================
;;; Graph building from CallHierarchyItems
;;; ============================================================

(defun build-call-graph-from-hierarchy (items incoming-calls-fn outgoing-calls-fn
                                        &key (include-incoming nil)
                                             (include-outgoing t)
                                             (progress-fn nil))
  "Build a call-graph from a list of CallHierarchyItems.

ITEMS - List of CallHierarchyItem objects (from prepareCallHierarchy)
INCOMING-CALLS-FN - Function (item) -> list of CallHierarchyIncomingCall or nil
OUTGOING-CALLS-FN - Function (item) -> list of CallHierarchyOutgoingCall or nil
INCLUDE-INCOMING - If T, fetch and include incoming calls (callers)
INCLUDE-OUTGOING - If T, fetch and include outgoing calls (callees)
PROGRESS-FN - Optional function (current total) for progress reporting

The callback functions allow callers to implement their own LSP request
mechanism (with error handling, timeouts, etc.).

Returns a call-graph structure."
  (let ((seen-nodes (make-hash-table :test #'equal))
        (seen-edges (make-hash-table :test #'equal))
        (all-nodes '())
        (all-edges '())
        (total (length items))
        (current 0))
    ;; Process each item
    (dolist (item items)
      (incf current)
      (when progress-fn
        (funcall progress-fn current total))
      (multiple-value-bind (nodes edges)
          (collect-hierarchy-for-item item
                                      incoming-calls-fn
                                      outgoing-calls-fn
                                      seen-nodes
                                      :include-incoming include-incoming
                                      :include-outgoing include-outgoing)
        ;; Add nodes
        (setf all-nodes (nconc nodes all-nodes))
        ;; Deduplicate and add edges
        (dolist (edge edges)
          (let ((edge-key (format nil "~A->~A"
                                  (cdr (assoc "source" edge :test #'string=))
                                  (cdr (assoc "target" edge :test #'string=)))))
            (unless (gethash edge-key seen-edges)
              (setf (gethash edge-key seen-edges) t)
              (push edge all-edges))))))
    ;; Convert to call-graph structure
    (let ((graph (call-graph:make-call-graph)))
      (dolist (node-alist all-nodes)
        (call-graph:add-node graph (call-graph:alist-to-graph-node node-alist)))
      (dolist (edge-alist all-edges)
        (call-graph:add-edge graph (call-graph:alist-to-graph-edge edge-alist)))
      graph)))

(defun collect-hierarchy-for-item (item incoming-calls-fn outgoing-calls-fn seen-nodes
                                   &key (include-incoming nil)
                                        (include-outgoing t))
  "Collect call hierarchy data for a single CallHierarchyItem.

ITEM - The CallHierarchyItem to analyze
INCOMING-CALLS-FN - Function to fetch incoming calls (or nil to skip)
OUTGOING-CALLS-FN - Function to fetch outgoing calls (or nil to skip)
SEEN-NODES - Hash table of already-seen node IDs

Returns (values nodes edges) where:
  - nodes is a list of node alists
  - edges is a list of edge alists"
  (let ((nodes '())
        (edges '())
        (node-id (make-node-id-from-item item)))
    ;; Add this node if not seen
    (unless (gethash node-id seen-nodes)
      (setf (gethash node-id seen-nodes) t)
      (push (call-hierarchy-item-to-node-alist item) nodes))
    ;; Get incoming calls (callers)
    (when (and include-incoming incoming-calls-fn)
      (let ((incoming (funcall incoming-calls-fn item)))
        (when incoming
          (dolist (call (coerce incoming 'list))
            (let* ((from-item (lsp:call-hierarchy-incoming-call-from call))
                   (from-id (make-node-id-from-item from-item)))
              ;; Add caller node if not seen
              (unless (gethash from-id seen-nodes)
                (setf (gethash from-id seen-nodes) t)
                (push (call-hierarchy-item-to-node-alist from-item) nodes))
              ;; Add edge
              (push (incoming-call-to-edge-alist call node-id) edges))))))
    ;; Get outgoing calls (callees)
    (when (and include-outgoing outgoing-calls-fn)
      (let ((outgoing (funcall outgoing-calls-fn item)))
        (when outgoing
          (dolist (call (coerce outgoing 'list))
            (let* ((to-item (lsp:call-hierarchy-outgoing-call-to call))
                   (to-id (make-node-id-from-item to-item)))
              ;; Add callee node if not seen
              (unless (gethash to-id seen-nodes)
                (setf (gethash to-id seen-nodes) t)
                (push (call-hierarchy-item-to-node-alist to-item) nodes))
              ;; Add edge
              (push (outgoing-call-to-edge-alist call node-id) edges))))))
    (values nodes edges)))
