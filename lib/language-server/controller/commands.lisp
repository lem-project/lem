(in-package :lem-language-server)

(define-lsp-command eval-command "cl-lsp.eval" (arguments)
  (let* ((point
           (text-document-position-params-to-point
            (convert-from-json (elt arguments 0)
                               'lsp:text-document-position-params)))
         (string (previous-form-string point)))
    (micros/client:remote-eval
     (server-backend-connection *server*)
     `(micros/lsp-api:eval-for-language-server ,string)
     :package-name (scan-current-package point)
     :callback (lambda (value)
                 (with-error-handler ()
                   (notify-eval-result value)))))
  :null)
