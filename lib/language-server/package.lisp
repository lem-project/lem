(defpackage :lem-language-server
  (:use :cl
        :alexandria
        :lem-language-server/protocol/lsp-type
        :lem-language-server/protocol/converter
        :lem-language-server/protocol/utils
        :lem-language-server/protocol/yason-utils)
  (:export :uninitialized-error
           :call-lsp-method
           :current-server
           :start-server
           :start-mock-server
           :start-tcp-server
           :start-stdio-server
           :mock-server-exit-status
           ;; text-document.lisp
           :text-document-uri
           :text-document-language-id
           :text-document-version
           :text-document-buffer
           :find-text-document
           ;; methods
           :initialize-request
           :shutdown-request
           :exit-request
           :text-document-did-open-request
           :text-document-did-change-request
           :text-document-did-close-request)
  (:lock t))
