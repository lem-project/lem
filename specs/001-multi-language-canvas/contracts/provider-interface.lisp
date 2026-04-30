;;;; Provider Interface Contract
;;;; Multi-Language Living Canvas Feature
;;;;
;;;; This file defines the formal API contract for call graph providers.
;;;; Providers implementing this interface integrate with Living Canvas
;;;; to provide call graph visualization for any programming language.
;;;;
;;;; Status: Specification (not loadable code)
;;;; Date: 2025-12-23

;;; ============================================================================
;;; Package Definition
;;; ============================================================================

(defpackage :lem-call-graph/provider
  (:use :cl)
  (:export
   ;; Protocol class
   #:call-graph-provider

   ;; Required generic functions
   #:provider-name
   #:provider-supports-p
   #:provider-analyze

   ;; Optional generic functions
   #:provider-priority
   #:provider-languages
   #:provider-capabilities

   ;; Registration
   #:register-provider
   #:unregister-provider
   #:find-provider
   #:list-providers

   ;; Utilities
   #:with-provider
   #:define-call-graph-provider))

;;; ============================================================================
;;; Protocol Class
;;; ============================================================================

(defclass call-graph-provider ()
  ()
  (:documentation
   "Base class for language-specific call graph analysis providers.

Providers extract function definitions and call relationships from source code
and return them as call-graph structures for visualization in Living Canvas.

To implement a new provider:
1. Subclass call-graph-provider
2. Implement provider-name (returns keyword identifier)
3. Implement provider-supports-p (returns T if source can be analyzed)
4. Implement provider-analyze (returns call-graph structure)
5. Optionally override provider-priority and provider-capabilities
6. Register with (register-provider *registry* provider '(:language ...))

Example:
  (defclass my-python-provider (call-graph-provider) ())

  (defmethod provider-name ((p my-python-provider)) :python)

  (defmethod provider-supports-p ((p my-python-provider) source)
    (or (python-buffer-p source)
        (python-file-p source)))

  (defmethod provider-analyze ((p my-python-provider) source &key type)
    (analyze-python-ast source type))"))

;;; ============================================================================
;;; Required Generic Functions
;;; ============================================================================

(defgeneric provider-name (provider)
  (:documentation
   "Return a keyword uniquely identifying this provider.

Arguments:
  PROVIDER -- A call-graph-provider instance

Returns:
  KEYWORD -- Unique provider identifier (e.g., :python, :javascript, :micros-cl)

Contract:
  - MUST return a keyword symbol
  - MUST be unique across all registered providers
  - SHOULD be descriptive of the language or analysis method

Example:
  (defmethod provider-name ((p tree-sitter-python-provider))
    :tree-sitter-python)"))

(defgeneric provider-supports-p (provider source)
  (:documentation
   "Return T if this provider can analyze SOURCE.

Arguments:
  PROVIDER -- A call-graph-provider instance
  SOURCE   -- The source to potentially analyze. Can be:
              - lem:buffer -- A Lem buffer object
              - pathname   -- A file path
              - string     -- Source code text (with optional metadata)

Returns:
  BOOLEAN -- T if this provider can analyze SOURCE, NIL otherwise

Contract:
  - MUST return T or NIL (no other values)
  - MUST NOT modify SOURCE or have side effects
  - SHOULD be fast (used for provider selection)
  - SHOULD check file extensions, major modes, or content signatures

Example:
  (defmethod provider-supports-p ((p python-provider) source)
    (typecase source
      (buffer (eq (buffer-major-mode source) 'python-mode))
      (pathname (member (pathname-type source) '(\"py\" \"pyw\") :test #'string-equal))
      (string (starts-with-p source \"#!/usr/bin/python\"))
      (t nil)))"))

(defgeneric provider-analyze (provider source &key type)
  (:documentation
   "Analyze SOURCE and return a call-graph structure.

Arguments:
  PROVIDER -- A call-graph-provider instance
  SOURCE   -- The source to analyze (buffer, pathname, or string)
  TYPE     -- (keyword) Analysis scope:
              :file    -- Single file analysis (default)
              :buffer  -- Analyze buffer contents
              :package -- Analyze package/module
              :project -- Analyze entire project
              :system  -- Analyze system/package definition

Returns:
  CALL-GRAPH -- A call-graph structure containing:
                - nodes: Hash table of graph-node structs
                - edges: List of graph-edge structs
                - root-package: Analysis scope identifier

Contract:
  - MUST return a valid call-graph structure (never NIL for success)
  - MUST signal provider-error on failure with descriptive message
  - MUST populate source-location for all nodes where available
  - SHOULD handle syntax errors gracefully (partial results)
  - SHOULD respect TYPE parameter for analysis scope
  - MAY cache results for performance

Error Conditions:
  provider-error        -- Analysis failed (with message)
  provider-unavailable  -- Required dependencies missing
  source-not-found      -- File/buffer doesn't exist

Example:
  (defmethod provider-analyze ((p python-provider) source &key (type :file))
    (let ((text (source-to-string source))
          (filepath (source-to-pathname source)))
      (multiple-value-bind (nodes edges) (parse-python-ast text filepath)
        (make-call-graph :nodes nodes
                         :edges edges
                         :root-package (pathname-name filepath)))))"))

;;; ============================================================================
;;; Optional Generic Functions (with defaults)
;;; ============================================================================

(defgeneric provider-priority (provider)
  (:documentation
   "Return integer priority for provider selection. Higher = tried first.

Arguments:
  PROVIDER -- A call-graph-provider instance

Returns:
  INTEGER -- Priority value (default: 0)

Contract:
  - MUST return a non-negative integer
  - Higher values indicate preference
  - Providers with same priority are tried in registration order

Typical Priority Values:
  0  -- Basic/fallback providers
  3  -- Regex-based or heuristic providers
  5  -- Tree-sitter based providers
  10 -- LSP-based or runtime introspection providers

Example:
  (defmethod provider-priority ((p lsp-python-provider)) 10)
  (defmethod provider-priority ((p tree-sitter-python-provider)) 5)")
  (:method ((provider call-graph-provider)) 0))

(defgeneric provider-languages (provider)
  (:documentation
   "Return list of language keywords this provider supports.

Arguments:
  PROVIDER -- A call-graph-provider instance

Returns:
  LIST -- List of language keywords (e.g., '(:python :python3))

Contract:
  - MUST return a list of keywords
  - Empty list means provider relies solely on provider-supports-p
  - Used for indexing in provider registry

Example:
  (defmethod provider-languages ((p js-provider))
    '(:javascript :ecmascript :jsx))")
  (:method ((provider call-graph-provider)) nil))

(defgeneric provider-capabilities (provider)
  (:documentation
   "Return plist of capabilities this provider supports.

Arguments:
  PROVIDER -- A call-graph-provider instance

Returns:
  PLIST -- Property list of capabilities

Known Capability Keys:
  :cross-file       -- Can resolve cross-file calls
  :type-info        -- Provides type information
  :docstrings       -- Extracts documentation
  :incremental      -- Supports incremental updates
  :async            -- Analysis can run asynchronously
  :project-scope    -- Can analyze entire projects
  :execution-trace  -- Can trace runtime execution

Example:
  (defmethod provider-capabilities ((p lsp-provider))
    '(:cross-file t :type-info t :docstrings t))")
  (:method ((provider call-graph-provider)) nil))

;;; ============================================================================
;;; Registry Interface
;;; ============================================================================

(defgeneric register-provider (registry provider languages)
  (:documentation
   "Register PROVIDER in REGISTRY for LANGUAGES.

Arguments:
  REGISTRY  -- Provider registry instance
  PROVIDER  -- A call-graph-provider instance
  LANGUAGES -- List of language keywords to associate with provider

Side Effects:
  - Adds provider to registry
  - Indexes provider by languages
  - May override existing providers with same name

Returns:
  PROVIDER -- The registered provider

Contract:
  - MUST add provider to registry
  - MUST index by all specified languages
  - SHOULD warn if overwriting existing provider

Example:
  (register-provider *registry* (make-instance 'python-provider) '(:python))"))

(defgeneric unregister-provider (registry provider-name)
  (:documentation
   "Remove provider with PROVIDER-NAME from REGISTRY.

Arguments:
  REGISTRY      -- Provider registry instance
  PROVIDER-NAME -- Keyword name of provider to remove

Returns:
  BOOLEAN -- T if provider was found and removed, NIL otherwise"))

(defgeneric find-provider (registry language &optional source)
  (:documentation
   "Find best provider for LANGUAGE in REGISTRY.

Arguments:
  REGISTRY -- Provider registry instance
  LANGUAGE -- Language keyword (e.g., :python)
  SOURCE   -- Optional source to check against provider-supports-p

Returns:
  PROVIDER or NIL -- Best matching provider, or NIL if none found

Contract:
  - MUST return highest priority provider that supports source
  - MUST return NIL if no suitable provider found
  - SHOULD be fast (called on every analysis request)"))

(defgeneric list-providers (registry &optional language)
  (:documentation
   "List all providers in REGISTRY, optionally filtered by LANGUAGE.

Arguments:
  REGISTRY -- Provider registry instance
  LANGUAGE -- Optional language keyword to filter by

Returns:
  LIST -- List of provider instances"))

;;; ============================================================================
;;; Utility Macros
;;; ============================================================================

(defmacro with-provider ((var provider) &body body)
  "Execute BODY with VAR bound to PROVIDER, ensuring cleanup on error."
  `(let ((,var ,provider))
     (unwind-protect
         (progn ,@body)
       (when (slot-boundp ,var 'cleanup-hook)
         (funcall (slot-value ,var 'cleanup-hook))))))

(defmacro define-call-graph-provider (name &key
                                           (superclasses '(call-graph-provider))
                                           slots
                                           provider-name
                                           languages
                                           priority
                                           supports
                                           analyze
                                           capabilities
                                           documentation)
  "Define a new call-graph provider class with methods.

Arguments:
  NAME         -- Symbol naming the provider class
  SUPERCLASSES -- List of superclasses (default: (call-graph-provider))
  SLOTS        -- Additional slot definitions
  PROVIDER-NAME -- Keyword identifier (default: derived from NAME)
  LANGUAGES    -- List of language keywords
  PRIORITY     -- Integer priority (default: 0)
  SUPPORTS     -- Function (provider source) -> boolean
  ANALYZE      -- Function (provider source &key type) -> call-graph
  CAPABILITIES -- Plist of capabilities
  DOCUMENTATION -- Class documentation string

Example:
  (define-call-graph-provider my-python-provider
    :provider-name :python
    :languages (:python)
    :priority 5
    :supports (lambda (provider source)
                (declare (ignore provider))
                (python-source-p source))
    :analyze (lambda (provider source &key type)
               (declare (ignore provider type))
               (analyze-with-tree-sitter source))
    :documentation \"Tree-sitter based Python provider.\")"
  (let ((provider-name-keyword (or provider-name
                                    (intern (symbol-name name) :keyword))))
    `(progn
       (defclass ,name ,superclasses
         ,slots
         ,@(when documentation `((:documentation ,documentation))))

       (defmethod provider-name ((p ,name))
         ,provider-name-keyword)

       ,@(when languages
           `((defmethod provider-languages ((p ,name))
               ',languages)))

       ,@(when priority
           `((defmethod provider-priority ((p ,name))
               ,priority)))

       ,@(when supports
           `((defmethod provider-supports-p ((p ,name) source)
               (funcall ,supports p source))))

       ,@(when analyze
           `((defmethod provider-analyze ((p ,name) source &key (type :file))
               (funcall ,analyze p source :type type))))

       ,@(when capabilities
           `((defmethod provider-capabilities ((p ,name))
               ',capabilities)))

       ',name)))

;;; ============================================================================
;;; Error Conditions
;;; ============================================================================

(define-condition provider-error (error)
  ((provider :initarg :provider :reader provider-error-provider)
   (message :initarg :message :reader provider-error-message))
  (:report (lambda (c stream)
             (format stream "Provider ~A error: ~A"
                     (provider-name (provider-error-provider c))
                     (provider-error-message c))))
  (:documentation "Signaled when provider analysis fails."))

(define-condition provider-unavailable (provider-error)
  ((reason :initarg :reason :reader provider-unavailable-reason))
  (:report (lambda (c stream)
             (format stream "Provider ~A unavailable: ~A"
                     (provider-name (provider-error-provider c))
                     (provider-unavailable-reason c))))
  (:documentation "Signaled when provider dependencies are missing."))

(define-condition source-not-found (provider-error)
  ((source :initarg :source :reader source-not-found-source))
  (:report (lambda (c stream)
             (format stream "Source not found: ~A"
                     (source-not-found-source c))))
  (:documentation "Signaled when analysis source doesn't exist."))

;;; ============================================================================
;;; Sample Implementation Skeleton
;;; ============================================================================

#|
;; Example: Minimal Python provider implementation

(defclass tree-sitter-python-provider (call-graph-provider)
  ((parser :accessor provider-parser :initform nil)
   (queries :accessor provider-queries :initform nil))
  (:documentation "Python call graph provider using tree-sitter."))

(defmethod provider-name ((p tree-sitter-python-provider))
  :tree-sitter-python)

(defmethod provider-languages ((p tree-sitter-python-provider))
  '(:python))

(defmethod provider-priority ((p tree-sitter-python-provider))
  5)

(defmethod provider-supports-p ((p tree-sitter-python-provider) source)
  (typecase source
    (lem:buffer
     (eq (lem:buffer-major-mode source) 'lem-python-mode:python-mode))
    (pathname
     (string-equal "py" (pathname-type source)))
    (t nil)))

(defmethod provider-analyze ((p tree-sitter-python-provider) source &key (type :file))
  (declare (ignore type))
  (let* ((text (source-to-string source))
         (tree (ts:with-parser (parser "python")
                 (ts:parser-parse-string parser text)))
         (root (ts:tree-root-node tree)))
    (multiple-value-bind (nodes edges)
        (extract-python-graph root text (source-to-pathname source))
      (make-call-graph
       :nodes (nodes-to-hash-table nodes)
       :edges edges
       :root-package (pathname-name (source-to-pathname source))))))

(defmethod provider-capabilities ((p tree-sitter-python-provider))
  '(:docstrings t :incremental nil :cross-file nil))

;; Register the provider
(register-provider *provider-registry*
                   (make-instance 'tree-sitter-python-provider)
                   '(:python))
|#

;;; ============================================================================
;;; End of Provider Interface Contract
;;; ============================================================================
