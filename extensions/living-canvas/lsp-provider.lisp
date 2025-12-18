(defpackage #:lem-living-canvas/lsp-provider
  (:use #:cl #:call-graph)
  (:import-from #:alexandria #:when-let)
  (:local-nicknames (:lsp-ch :lem-living-canvas/lsp-call-hierarchy)
                    (:cg-lsp :call-graph-lsp))
  (:export #:lsp-call-hierarchy-provider
           #:collect-file-call-hierarchy))
(in-package #:lem-living-canvas/lsp-provider)

;;; LSP Call Hierarchy Provider
;;;
;;; This provider uses the LSP Call Hierarchy API to analyze code.
;;; It works with any language server that supports the Call Hierarchy protocol.
;;; The core conversion logic is delegated to call-graph-lsp package.

;;; ============================================================
;;; Lem-specific helper functions
;;; ============================================================

(defun collect-callable-symbols (buffer)
  "Get all callable symbols (functions, methods, constructors) from BUFFER.

Uses textDocument/documentSymbol to get all symbols, then filters to callables.
Returns a list of DocumentSymbol objects."
  (when-let ((symbols (lsp-ch:get-document-symbols buffer)))
    (let ((all-symbols (lsp-ch:flatten-document-symbols symbols)))
      (remove-if-not #'lsp-ch:document-symbol-is-callable-p all-symbols))))

(defun prepare-call-hierarchy-items (buffer)
  "Collect CallHierarchyItem objects for all callable symbols in BUFFER.

Returns a list of CallHierarchyItem objects suitable for use with
call-graph-lsp:build-call-graph-from-hierarchy."
  (let* ((items '())
         (callable-symbols (collect-callable-symbols buffer))
         (total (length callable-symbols))
         (current 0))
    (lem:message "Preparing ~D symbols..." total)
    (dolist (doc-symbol callable-symbols)
      (incf current)
      (when (zerop (mod current 5))
        (lem:message "Preparing symbols... ~D/~D" current total)
        (lem:redraw-display))
      (handler-case
          (let ((point (lsp-ch:document-symbol-to-position doc-symbol buffer)))
            (when-let ((prepared (lsp-ch:prepare-call-hierarchy buffer point)))
              (push (elt prepared 0) items)))
        (error () nil)))
    (nreverse items)))

(defun make-safe-incoming-calls-fn ()
  "Create a callback function for fetching incoming calls with error handling."
  (lambda (item)
    (handler-case
        (lsp-ch:get-incoming-calls item)
      (error () nil))))

(defun make-safe-outgoing-calls-fn ()
  "Create a callback function for fetching outgoing calls with error handling."
  (lambda (item)
    (handler-case
        (lsp-ch:get-outgoing-calls item)
      (error () nil))))

;;; ============================================================
;;; Collect File Call Hierarchy
;;; ============================================================

(defun collect-file-call-hierarchy (buffer &key (include-incoming nil)
                                                (include-outgoing t))
  "Collect call hierarchy for all callable symbols in BUFFER.

INCLUDE-INCOMING - if T, fetch incoming calls (callers). Default: NIL
INCLUDE-OUTGOING - if T, fetch outgoing calls (callees). Default: T

Returns a call-graph structure containing all functions and their call relationships.
This function uses the LSP Call Hierarchy API via call-graph-lsp."
  (unless (lsp-ch:buffer-has-call-hierarchy-p buffer)
    (return-from collect-file-call-hierarchy (make-call-graph)))
  (let ((items (prepare-call-hierarchy-items buffer))
        (total 0))
    (setf total (length items))
    (when (zerop total)
      (return-from collect-file-call-hierarchy (make-call-graph)))
    (lem:message "Analyzing ~D symbols..." total)
    ;; Use call-graph-lsp to build the graph
    (cg-lsp:build-call-graph-from-hierarchy
     items
     (make-safe-incoming-calls-fn)
     (make-safe-outgoing-calls-fn)
     :include-incoming include-incoming
     :include-outgoing include-outgoing
     :progress-fn (lambda (current total)
                    (when (zerop (mod current 5))
                      (lem:message "Analyzing calls... ~D/~D" current total)
                      (lem:redraw-display))))))

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
