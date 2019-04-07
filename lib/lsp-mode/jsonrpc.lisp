(in-package #:jsonrpc/transport/tcp)

(defmethod send-message-using-transport
    ((transport tcp-transport) connection message)
  (write-sequence
   (string-to-utf-8-bytes
    (format nil
            "Content-Length: ~A~C~C~:*~:*~C~C~A"
            (length (babel:string-to-octets json))
            #\Return
            #\Newline
            json))
   stream)
  (force-output stream))
