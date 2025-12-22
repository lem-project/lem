(in-package #:call-graph)

;;; Provider Registry
;;;
;;; This module provides a central registry for call graph providers.
;;; Providers are registered with language keywords and selected by priority.

(defclass provider-registry ()
  ((providers
    :initform (make-hash-table :test 'eq)
    :accessor registry-providers
    :documentation "Hash table mapping provider-name keyword to provider instance.")
   (language-map
    :initform (make-hash-table :test 'eq)
    :accessor registry-language-map
    :documentation "Hash table mapping language keyword to list of providers."))
  (:documentation "Central registry for call graph providers.

Manages provider registration, lookup, and selection based on language
and priority. Providers are indexed both by name and by supported languages."))

(defvar *provider-registry* (make-instance 'provider-registry)
  "Global provider registry instance.

This registry is used by living-canvas to find appropriate providers
for different programming languages.")

(defun register-provider (registry provider languages)
  "Register PROVIDER in REGISTRY for LANGUAGES.

Arguments:
  REGISTRY  - Provider registry instance
  PROVIDER  - A call-graph-provider instance
  LANGUAGES - List of language keywords (e.g., '(:python :python3))

Returns:
  PROVIDER - The registered provider

The provider is indexed by its name (from provider-name) and by each
language in LANGUAGES. If a provider with the same name already exists,
it is replaced."
  (let ((name (provider-name provider)))
    ;; Store by provider name
    (setf (gethash name (registry-providers registry)) provider)
    ;; Index by each language
    (dolist (lang languages)
      (push provider (gethash lang (registry-language-map registry)))))
  provider)

(defun unregister-provider (registry provider-name)
  "Remove provider with PROVIDER-NAME from REGISTRY.

Arguments:
  REGISTRY      - Provider registry instance
  PROVIDER-NAME - Keyword name of the provider to remove

Returns:
  T if provider was found and removed, NIL otherwise"
  (let ((provider (gethash provider-name (registry-providers registry))))
    (when provider
      ;; Remove from providers hash
      (remhash provider-name (registry-providers registry))
      ;; Remove from all language lists
      (maphash (lambda (lang providers)
                 (setf (gethash lang (registry-language-map registry))
                       (remove provider providers)))
               (registry-language-map registry))
      t)))

(defun find-provider (registry language &optional source)
  "Find the best provider for LANGUAGE in REGISTRY.

Arguments:
  REGISTRY - Provider registry instance
  LANGUAGE - Language keyword (e.g., :python)
  SOURCE   - Optional source to check against provider-supports-p

Returns:
  The highest-priority provider that:
  1. Is registered for LANGUAGE
  2. If SOURCE is provided, returns T from provider-supports-p

Returns NIL if no suitable provider is found."
  (let ((providers (gethash language (registry-language-map registry))))
    (when providers
      ;; Sort by priority (highest first)
      (let ((sorted (sort (copy-list providers) #'> :key #'provider-priority)))
        (if source
            ;; Filter by source support
            (find-if (lambda (p) (provider-supports-p p source)) sorted)
            ;; Return highest priority
            (first sorted))))))

(defun list-providers (registry &optional language)
  "List all providers in REGISTRY, optionally filtered by LANGUAGE.

Arguments:
  REGISTRY - Provider registry instance
  LANGUAGE - Optional language keyword to filter by

Returns:
  List of provider instances. If LANGUAGE is provided, returns only
  providers registered for that language. Otherwise returns all providers."
  (if language
      (gethash language (registry-language-map registry))
      ;; Return all unique providers
      (let ((all-providers nil))
        (maphash (lambda (name provider)
                   (declare (ignore name))
                   (pushnew provider all-providers))
                 (registry-providers registry))
        all-providers)))
