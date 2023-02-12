(in-package :lem-language-server)

(lem-lsp-base/type:define-class show-eval-result-params ()
  ((message :type lem-lsp-base/type:lsp-string
            :initarg :message
            :accessor show-eval-result-params-message)
   (type :type lsp:message-type
         :initarg :type
         :accessor show-eval-result-params-type)
   (id :initarg :id
       :accessor show-eval-result-params-id)
   (range :initarg :range
          :type lsp:range
          :accessor show-eval-result-params-range)))

(lem-lsp-base/type:define-notification-message lisp/show-eval-result ()
  :message-direction "serverToClient"
  :method "lisp/showEvalResult"
  :params 'show-eval-result-params)

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

(defun notify-eval-result (value range)
  (multiple-value-bind (message type)
      (convert-eval-result value)
    (notify-log-message type message)
    (notify-to-client (make-instance 'lisp/show-eval-result)
                      (make-instance 'show-eval-result-params
                                     :id 0
                                     :type type
                                     :message message
                                     :range range))))

(defun remote-eval (string package-name &optional continue)
  (micros/client:remote-eval
   (server-backend-connection *server*)
   `(micros/lsp-api:eval-for-language-server ,string)
   :package-name package-name
   :callback (lambda (value)
               (with-error-handler ()
                 (funcall continue value)))
   :thread :repl-thread))

(defun interrupt-eval ()
  (micros/client:interrupt (server-backend-connection *server*) :repl-thread))

(defun eval-last-expression (point)
  (lem:with-point ((start point :left-inserting)
                   (end point :left-inserting))
    (when (lem:form-offset start -1) ; TODO: nilの場合を考慮する
      (let ((string (lem:points-to-string start end))
            (range (points-to-lsp-range start end)))
        (remote-eval string
                     (scan-current-package point)
                     (lambda (value)
                       (notify-eval-result value range)))))))

(defun micros-write-string (string target info)
  (declare (ignore target))
  (with-error-handler ()
    (let ((info (ecase info
                  (:log lsp:message-type-log)
                  (:error lsp:message-type-error))))
      (let ((jsonrpc/connection:*connection* (server-jsonrpc-connection *server*)))
        (notify-log-message info string)))))
