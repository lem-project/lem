(in-package #:call-graph)

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

(defgeneric provider-languages (provider)
  (:documentation "Return list of language keywords this provider supports.
For example, '(:python) or '(:javascript :typescript).
Used for automatic registration and provider discovery.")
  (:method ((provider call-graph-provider))
    nil))

;;; Provider Conditions

;; Reader function declarations for exported condition accessors
(defgeneric provider-error-provider (condition)
  (:documentation "Return the provider that encountered the error."))

(defgeneric provider-error-message (condition)
  (:documentation "Return the error message, or NIL if not specified."))

(defgeneric provider-unavailable-reason (condition)
  (:documentation "Return the reason why the provider is unavailable, or NIL."))

(define-condition provider-error (error)
  ((provider :initarg :provider
             :reader provider-error-provider
             :documentation "The provider that encountered the error")
   (message :initarg :message
            :initform nil
            :reader provider-error-message
            :documentation "Optional error message"))
  (:report (lambda (condition stream)
             (format stream "Provider error~@[ (~A)~]: ~A"
                     (when (slot-boundp condition 'provider)
                       (provider-name (provider-error-provider condition)))
                     (or (provider-error-message condition)
                         "unspecified error"))))
  (:documentation "Base condition for provider errors.

Use this to signal errors during provider analysis that should be
handled by the caller (e.g., to try a different provider)."))

(define-condition provider-unavailable (provider-error)
  ((reason :initarg :reason
           :initform nil
           :reader provider-unavailable-reason
           :documentation "Reason why the provider is unavailable"))
  (:report (lambda (condition stream)
             (format stream "Provider ~A is unavailable~@[: ~A~]"
                     (when (slot-boundp condition 'provider)
                       (provider-name (provider-error-provider condition)))
                     (provider-unavailable-reason condition))))
  (:documentation "Signaled when a provider cannot perform analysis.

Reasons may include: missing dependencies, language grammar not loaded,
required runtime not connected, etc."))

;;; Provider Definition Macro

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun %parse-provider-options (options)
    "Parse provider definition options at macro-expansion time.
Returns values: (provider-name priority languages class-options).
Signals an error if :provider-name is not specified."
    (let ((provider-name (cadr (assoc :provider-name options)))
          (priority (or (cadr (assoc :priority options)) 5))
          (languages (cadr (assoc :languages options)))
          (class-options (remove-if (lambda (opt)
                                      (member (car opt)
                                              '(:provider-name :priority :languages)))
                                    options)))
      (unless provider-name
        (error "define-call-graph-provider requires :provider-name option"))
      (values provider-name priority languages class-options))))

(defmacro define-call-graph-provider (name (&rest superclasses) slots &body options)
  "Define a new call graph provider class with common boilerplate.

NAME is the class name (e.g., 'my-python-provider).
SUPERCLASSES defaults to (call-graph-provider) if empty.
SLOTS are slot definitions as in DEFCLASS.
OPTIONS are class options plus special provider options:

  :provider-name    - Keyword name for the provider (required)
  :priority         - Provider priority (default 5)
  :languages        - List of supported language keywords

To register the provider, call register-provider explicitly after load:
  (register-provider *provider-registry*
                     (make-instance 'my-python-provider)
                     '(:python))

Example:
  (define-call-graph-provider my-python-provider ()
    ((parser :accessor my-provider-parser))
    (:provider-name :my-python)
    (:priority 5)
    (:languages (:python)))"
  (multiple-value-bind (provider-name priority languages class-options)
      (%parse-provider-options options)
    (let ((superclasses (or superclasses '(call-graph-provider))))
      `(progn
         (defclass ,name ,superclasses
           ,slots
           ,@class-options)
         (defmethod provider-name ((provider ,name))
           ,provider-name)
         (defmethod provider-priority ((provider ,name))
           ,priority)
         ,@(when languages
             `((defmethod provider-languages ((provider ,name))
                 ',languages)))))))

