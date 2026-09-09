(defpackage :lem-language-server
  (:use :cl
        :alexandria
        :lem-lsp-base/type
        :lem-lsp-base/converter
        :lem-lsp-base/utils
        :lem-lsp-base/yason-utils)
  (:export :uninitialized-error
           :call-lsp-method
           :current-server
           :start-server
           :start-mock-server
           :start-tcp-server
           :start-stdio-server
           :mock-server-exit-status
           :server-shutdown-request-received-p
           :with-mock-server
           ;; mock-server testing support
           :mock-server-canned-responses
           :mock-server-response-history
           :set-mock-response
           :clear-mock-responses
           ;; text-document.lisp
           :text-document-uri
           :text-document-language-id
           :text-document-version
           :text-document-buffer
           :find-text-document
           ;; methods - lifecycle
           :initialize-request
           :shutdown-request
           :exit-request
           ;; methods - document synchronization
           :text-document-did-open-request
           :text-document-did-change-request
           :text-document-did-close-request
           ;; methods - language features
           :completion-request
           :hover-request
           :go-to-definition-request
           :find-references-request
           :document-highlight-request
           :signature-help-request
           :document-symbol-request
           :document-formatting-request
           :document-range-formatting-request
           :on-type-formatting-request)
  (:lock t))
