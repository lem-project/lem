(in-package :jsonrpc/transport/stdio)
(defmethod send-message-using-transport ((transport stdio-transport) connection message)
  (let ((json (trivial-utf-8:string-to-utf-8-bytes
               (with-output-to-string (s)
                 (yason:encode message s))))
        (stream (connection-socket connection)))
    (format stream "Content-Length: ~A~C~C~:*~:*~C~C"
            (length json)
            #\Return
            #\Newline)
    (write-sequence json stream)
    (force-output stream)))
