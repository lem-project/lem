(in-package :lem-language-server)

(define-lsp-command eval-command "cl-lsp.eval" (arguments)
  (let* ((point
           (text-document-position-params-to-point
            (convert-from-json (elt arguments 0)
                               'lsp:text-document-position-params)))
         (string (previous-form-string point))
         (server *server*)
         (connection jsonrpc/connection:*connection*))
    (micros/client:remote-eval
     (server-backend-connection *server*)
     `(micros/lsp-api:eval-for-language-server ,string)
     :package-name (scan-current-package point)
     :callback (lambda (value)
                 (with-error-handler ()
                   (let ((*server* server)
                         (jsonrpc/connection:*connection* connection))
                     (alexandria:destructuring-ecase value
                       ((:ok result)
                        (let ((value (or (micros/lsp-api:eval-result-error result)
                                         (micros/lsp-api:eval-result-value result)))
                              (errorp (not (null (micros/lsp-api:eval-result-error
                                                  result)))))
                          (notify-show-message (if errorp
                                                   lsp:message-type-error
                                                   lsp:message-type-info)
                                               value)))
                       ((:abort condition)
                        (notify-show-message lsp:message-type-error
                                             condition)))))))
    :null))
