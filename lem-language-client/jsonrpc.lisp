(in-package :lem-language-client)

(defun pretty-json (params)
  (with-output-to-string (stream)
    (yason:encode params (yason:make-json-output-stream stream))))

(defun jsonrpc-notify (connection method params)
  (lem:message "notify: ~A ~A" method (pretty-json params))
  (jsonrpc:notify connection method params))

(defun jsonrpc-call (connection method params)
  (lem:message "call: ~A ~A" method (pretty-json params))
  (jsonrpc:call connection method params))
