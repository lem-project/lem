(in-package #:call-graph-lsp)

;;; LSP Call Hierarchy → alist Conversion
;;;
;;; This module provides pure conversion functions from LSP Call Hierarchy
;;; protocol objects to alist format compatible with call-graph JSON format.
;;; These functions have no Lem dependencies beyond LSP protocol definitions.

;;; ============================================================
;;; URI and ID utilities
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
  (let ((uri (lsp:call-hierarchy-item-uri item))
        (name (lsp:call-hierarchy-item-name item))
        (range (lsp:call-hierarchy-item-selection-range item)))
    (format nil "~A:~A:~A"
            uri
            name
            (lsp:position-line (lsp:range-start range)))))

;;; ============================================================
;;; SymbolKind conversion
;;; ============================================================

(defun symbol-kind-to-type-keyword (kind)
  "Convert LSP SymbolKind to a type keyword for call-graph."
  (case kind
    (#.lsp:symbol-kind-function :function)
    (#.lsp:symbol-kind-method :method)
    (#.lsp:symbol-kind-constructor :constructor)
    (#.lsp:symbol-kind-class :class)
    (#.lsp:symbol-kind-interface :interface)
    (#.lsp:symbol-kind-module :module)
    (#.lsp:symbol-kind-namespace :namespace)
    (#.lsp:symbol-kind-property :property)
    (#.lsp:symbol-kind-field :field)
    (#.lsp:symbol-kind-variable :variable)
    (#.lsp:symbol-kind-constant :constant)
    (t :function)))

;;; ============================================================
;;; CallHierarchyItem → node alist
;;; ============================================================

(defun call-hierarchy-item-to-node-alist (item)
  "Convert a CallHierarchyItem to a node alist for call-graph JSON format.

Returns an alist with keys: id, name, package, type, docstring, arglist,
sourceFile, sourceLine."
  (let* ((name (lsp:call-hierarchy-item-name item))
         (kind (lsp:call-hierarchy-item-kind item))
         (uri (lsp:call-hierarchy-item-uri item))
         (detail (handler-case
                     (lsp:call-hierarchy-item-detail item)
                   (unbound-slot () nil)))
         (range (lsp:call-hierarchy-item-selection-range item))
         (line (1+ (lsp:position-line (lsp:range-start range))))
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

;;; ============================================================
;;; IncomingCall/OutgoingCall → edge alist
;;; ============================================================

(defun incoming-call-to-edge-alist (incoming-call target-id)
  "Convert a CallHierarchyIncomingCall to an edge alist.

INCOMING-CALL is the LSP response object.
TARGET-ID is the node ID of the target (the function being called).

Returns an alist with keys: source, target, callType."
  (let* ((from-item (lsp:call-hierarchy-incoming-call-from incoming-call))
         (source-id (make-node-id-from-item from-item)))
    (list (cons "source" source-id)
          (cons "target" target-id)
          (cons "callType" "direct"))))

(defun outgoing-call-to-edge-alist (outgoing-call source-id)
  "Convert a CallHierarchyOutgoingCall to an edge alist.

OUTGOING-CALL is the LSP response object.
SOURCE-ID is the node ID of the source (the function making the call).

Returns an alist with keys: source, target, callType."
  (let* ((to-item (lsp:call-hierarchy-outgoing-call-to outgoing-call))
         (target-id (make-node-id-from-item to-item)))
    (list (cons "source" source-id)
          (cons "target" target-id)
          (cons "callType" "direct"))))
