(in-package :lem-language-server)

(define-lsp-command "cl-lsp.eval" (arguments)
  (let ((point
          (text-document-position-params-to-point
           (convert-from-json (elt arguments 0)
                              'lsp:text-document-position-params))))
    (let ((result (eval-previous-form point)))
      (notify-show-message lsp:message-type-info
                           result))
    :null))
