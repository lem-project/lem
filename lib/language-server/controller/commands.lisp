(in-package :lem-language-server)

(define-lsp-command eval-command "cl-lsp.eval" (arguments)
  (let* ((point
           (text-document-position-params-to-point
            (convert-from-json (elt arguments 0)
                               'lsp:text-document-position-params)))
         (string (previous-form-string point)))
    (remote-eval string (scan-current-package point)))
  :null)

(define-lsp-command interrupt-eval-command "cl-lsp.interrupt" (arguments)
  (declare (ignore arguments))
  (interrupt-eval)
  :null)
