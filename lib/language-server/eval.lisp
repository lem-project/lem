(in-package :lem-language-server)

(defun eval-previous-form (point)
  (let* ((string (previous-form-string point))
         (result (remote-eval-sync *server*
                                   `(micros/lsp-api:eval-for-language-server ,string)
                                   (scan-current-package point))))
    (values (or (micros/lsp-api:eval-result-error result)
                (micros/lsp-api:eval-result-value result))
            (not (null (micros/lsp-api:eval-result-error result))))))
