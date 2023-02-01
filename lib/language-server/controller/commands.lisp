(in-package :lem-language-server)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar +server-variable+ '#:serve)
  (defvar +connection-variable+ '#:connection))

(defmacro with-save-server-context (() &body body)
  `(let ((,+server-variable+ *server*)
         (,+connection-variable+ jsonrpc/connection:*connection*))
     ,@body))

(defmacro with-restore-server-context (() &body body)
  `(let ((*server* ,+server-variable+)
         (jsonrpc/connection:*connection* ,+connection-variable+))
     ,@body))

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

(define-lsp-command eval-command "cl-lsp.eval" (arguments)
  (let* ((point
           (text-document-position-params-to-point
            (convert-from-json (elt arguments 0)
                               'lsp:text-document-position-params)))
         (string (previous-form-string point)))
    (with-save-server-context ()
      (micros/client:remote-eval
       (server-backend-connection *server*)
       `(micros/lsp-api:eval-for-language-server ,string)
       :package-name (scan-current-package point)
       :callback (lambda (value)
                   (with-error-handler ()
                     (with-restore-server-context ()
                       (notify-eval-result value)))))))
  :null)
