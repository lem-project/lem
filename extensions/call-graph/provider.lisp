(in-package :call-graph)

;;; Provider Protocol
;;;
;;; This protocol defines the interface for call graph providers.
;;; Different providers can implement analysis for different languages:
;;; - Common Lisp (using sb-introspect)
;;; - LSP-based languages (using Call Hierarchy API)
;;; - etc.

(defclass call-graph-provider ()
  ()
  (:documentation "Base class for call graph providers.

Subclasses should implement:
  - provider-name: Return a keyword identifying the provider
  - provider-supports-p: Check if this provider can analyze a given source
  - provider-analyze: Perform analysis and return a call-graph"))

(defgeneric provider-name (provider)
  (:documentation "Return the provider name as a keyword (e.g., :common-lisp, :lsp)."))

(defgeneric provider-supports-p (provider source)
  (:documentation "Return T if PROVIDER can analyze SOURCE.
SOURCE can be a buffer, file path, or other source designator."))

(defgeneric provider-analyze (provider source &key)
  (:documentation "Analyze SOURCE and return a call-graph structure.
SOURCE can be a buffer, file path, package designator, or system designator
depending on what the provider supports."))

(defgeneric provider-priority (provider)
  (:documentation "Return the priority of this provider (higher = tried first).
Default is 0. LSP providers should return higher values to be preferred
over language-specific providers when LSP is available.")
  (:method ((provider call-graph-provider))
    0))
