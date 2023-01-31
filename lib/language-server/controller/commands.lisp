(in-package :lem-language-server)

(define-lsp-command "cl-lsp.eval" (arguments)
  (let ((point
          (text-document-position-params-to-point
           (convert-from-json (elt arguments 0)
                              'lsp:text-document-position-params))))
    (multiple-value-bind (result errorp)
        (eval-previous-form point)
      (notify-show-message (if errorp
                               lsp:message-type-error
                               lsp:message-type-info)
                           result))
    :null))
