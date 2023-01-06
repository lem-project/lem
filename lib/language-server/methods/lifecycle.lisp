(in-package :lem-language-server)

(define-request (initialize "initialize") (params lsp:initialize-params)
  (log:info "initialize")
  (run-backend)
  (setf (server-client-capabilities *server*) params)
  (convert-to-json
   (make-instance
    'lsp:initialize-result
    :capabilities (make-instance
                   'lsp:server-capabilities
                   ;; :position-encoding
                   :text-document-sync (make-instance
                                        'lsp:text-document-sync-options
                                        :open-close t
                                        :change lsp:text-document-sync-kind-incremental
                                        :will-save nil
                                        :will-save-wait-until nil
                                        :save t)
                   ;; :notebook-document-sync
                   ;; :completion-provider
                   :hover-provider (make-instance 'lsp:hover-options :work-done-progress nil)
                   ;; :signature-help-provider
                   ;; :declaration-provider
                   :definition-provider (make-instance 'lsp:definition-options :work-done-progress nil)
                   ;; :type-definition-provider
                   ;; :implementation-provider
                   ;; :references-provider
                   :document-highlight-provider (make-instance 'lsp:document-highlight-options)
                   ;; :document-symbol-provider
                   ;; :code-action-provider
                   ;; :code-lens-provider
                   ;; :document-link-provider
                   ;; :color-provider
                   ;; :document-formatting-provider
                   ;; :document-range-formatting-provider
                   ;; :document-on-type-formatting-provider
                   ;; :rename-provider
                   ;; :folding-range-provider
                   ;; :execute-command-provider
                   ;; :selection-range-provider
                   ;; :linked-editing-range-provider
                   ;; :call-hierarchy-provider
                   ;; :semantic-tokens-provider
                   ;; :moniker-provider
                   ;; :type-hierarchy-provider
                   ;; :inline-value-provider
                   ;; :inlay-hint-provider
                   ;; :diagnostic-provider
                   ;; :workspace-symbol-provider
                   ;; :workspace
                   :experimental nil)
    :server-info (make-lsp-map "name" *language-server-name*
                               "version" *language-server-version*))))

(define-request (initialized "initialized") (params lsp:initialized-params)
  (declare (ignore params))
  (values))

#+TODO
(define-request (client-register-capability "client/registerCapability") (params lsp:registration-params)
  )

#+TODO
(define-request (client-unregister-capability "client/unregisterCapability") (params lsp:unregistration-params)
  )

#+TODO
(define-request (set-trace "$/setTrace") (params lsp:set-trace-params)
  )

#+TODO
(define-request (log-trace "$/logTrace") (params lsp:log-trace-params)
  )

(define-request (shutdown "shutdown") ()
  (setf (shutdown-request-received-p (current-server)) t)
  nil)

(define-request (exit "exit") ()
  (if (server-shutdown-request-received-p (current-server))
      (uiop:quit 0)
      (uiop:quit 1))
  (values))
