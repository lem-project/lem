(in-package :lem-language-server)

(defun convert-eval-result (value)
  (alexandria:destructuring-ecase value
    ((:ok result)
     (let ((value (or (micros/lsp-api:eval-result-error result)
                      (micros/lsp-api:eval-result-value result)))
           (errorp (not (null (micros/lsp-api:eval-result-error
                               result)))))
       (values value
               (if errorp
                   lsp:message-type-error
                   lsp:message-type-info))))
    ((:abort condition)
     (values condition
             lsp:message-type-error))))

(defun notify-eval-result (value)
  (multiple-value-bind (message type)
      (convert-eval-result value)
    (notify-show-message type message)))
