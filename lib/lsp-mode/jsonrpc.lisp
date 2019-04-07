(in-package #:jsonrpc/transport/tcp)

(defmethod send-message-using-transport
    ((transport tcp-transport) connection message)
  (let ((json (with-output-to-string (s)
                (yason:encode message s)))
        (stream (connection-socket connection)))
    (write-sequence
     (string-to-utf-8-bytes
      (format nil
              "Content-Length: ~A~C~C~:*~:*~C~C~A"
              (length (string-to-utf-8-bytes json))
              #\Return
              #\Newline
              json))
     stream)
    (force-output stream)))
