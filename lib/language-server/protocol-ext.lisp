(in-package :lem-language-server)

(lem-lsp-base/type:define-class eval-result-params ()
  ((message :type lem-lsp-base/type:lsp-string
            :initarg :message
            :accessor eval-result-params-message)
   (id :initarg :id
       :accessor eval-result-params-id)))

(lem-lsp-base/type:define-notification-message micros/eval-result ()
  :message-direction "serverToClient"
  :method "micros/evalResult"
  :params 'eval-result-params)
