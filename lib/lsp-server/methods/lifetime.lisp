(cl-lsp/defpackage:defpackage :cl-lsp/methods/lifetime
  (:use :cl)
  (:import-from :cl-lsp/server
                :define-method
                :this-server
                :set-client-capabilities)
  (:import-from :cl-lsp/swank
                :swank-init)
  (:local-nicknames (:protocol :lem-lsp-utils/protocol)
                    (:json :lem-lsp-utils/json))
  (:export :register-initialized-hook))
(in-package :cl-lsp/methods/lifetime)

(defvar *initialized-hooks* '())

(defun register-initialized-hook (function)
  (pushnew function *initialized-hooks*))

(defun make-server-capabilities ()
  (make-instance
   'protocol:server-capabilities
   :text-document-sync (make-instance
                        'protocol:text-document-sync-options
                        :open-close (json:json-false)
                        :change protocol:text-document-sync-kind.incremental
                        :will-save (json:json-false)
                        :will-save-wait-until (json:json-false)
                        :save (json:json-false))
   :completion-provider (make-instance
                         'protocol:completion-options
                         :work-done-progress (json:json-false)
                         :trigger-characters (loop :for code
                                                   :from (char-code #\a)
                                                   :to (char-code #\z)
                                                   :collect (string (code-char code)))
                         :all-commit-characters (json:json-array) ;TODO
                         :resolve-provider (json:json-false)) ;TODO
   :hover-provider (make-instance 'protocol:hover-options
                                  :work-done-progress (json:json-false))
   :signature-help-provider (make-instance 'protocol:signature-help-options
                                           :work-done-progress (json:json-false)
                                           :trigger-characters (json:json-array (string #\space)) ;TODO
                                           :retrigger-characters (json:json-array)) ;TODO
   :declaration-provider (json:json-false)
   :definition-provider (make-instance 'protocol:definition-options
                                       :work-done-progress (json:json-false))
   :type-definition-provider (json:json-false)
   :implementation-provider (json:json-false)
   :references-provider (make-instance 'protocol:reference-options
                                       :work-done-progress (json:json-false))
   :document-highlight-provider (make-instance 'protocol:document-highlight-options
                                               :work-done-progress (json:json-false))
   :code-action-provider (json:json-false)
   ;; :code-lens-provider
   ;; :document-link-provider
   :color-provider (json:json-false)
   :document-formatting-provider (make-instance 'protocol:document-formatting-options
                                                :work-done-progress (json:json-false))
   :document-range-formatting-provider (make-instance 'protocol:document-range-formatting-options
                                                      :work-done-progress (json:json-false))
   :document-on-type-formatting-provider (make-instance 'protocol:document-on-type-formatting-options
                                                        :first-trigger-character ")"
                                                        #|:more-trigger-character|#)
   :rename-provider (make-instance 'protocol:rename-options
                                   :work-done-progress (json:json-false)
                                   :prepare-provider (json:json-false))
   :folding-range-provider (json:json-false)
   ;; :execute-command-provider
   :selection-range-provider (json:json-false)
   :workspace-symbol-provider (json:json-false)
   ;:workspace
   ))

(define-method "initialize" (params protocol:initialize-params) ()
  (set-client-capabilities params (this-server))
  (json:object-to-json
   (make-instance 'protocol:initialize-result
                  :capabilities (make-server-capabilities)
                  :server-info (json:make-json :name "cl-lsp"
                                               #|:version "0.0.1"|#))))

(define-method "initialized" () ()
  (swank-init)
  (mapc #'funcall *initialized-hooks*)
  nil)

(define-method "shutdown" () ()
  t)

(define-method "exit" () ()
  (values))
