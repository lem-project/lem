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

(defun remote-eval (string package-name)
  (micros/client:remote-eval
   (server-backend-connection *server*)
   `(micros/lsp-api:eval-for-language-server ,string)
   :package-name package-name
   :callback (lambda (value)
               (with-error-handler ()
                 (notify-eval-result value)))
   :thread :repl-thread))

(defun interrupt-eval ()
  (micros/client:interrupt (server-backend-connection *server*) :repl-thread))

(defun micros-write-string (string target info)
  (declare (ignore target))
  (with-error-handler ()
    (let ((info (ecase info
                  (:log lsp:message-type-log)
                  (:error lsp:message-type-error))))
      (let ((jsonrpc/connection:*connection* (server-jsonrpc-connection *server*)))
        (notify-log-message info string)))))
