(in-package :lem-language-server)

(defun notify-to-client (notification params)
  (assert (typep params (notification-message-params notification)))
  (jsonrpc:notify (server-jsonrpc-server *server*)
                  (notification-message-method notification)
                  (convert-to-json params)))

(defun notify-show-message (type message)
  (notify-to-client (make-instance 'lsp:window/show-message)
                    (make-instance 'lsp:show-message-params
                                   :type type
                                   :message message)))
