(in-package :jsonrpc/transport/stdio)

(defmethod send-message-using-transport ((transport stdio-transport) connection message)
  (let ((json (babel:string-to-octets
               (with-output-to-string (s)
                 (yason:encode message s))))
        (stream (connection-socket connection)))
    (format stream "Content-Length: ~A~C~C~:*~:*~C~C"
            (length json)
            #\Return
            #\Newline)
    (write-sequence json stream)
    (force-output stream)))

(defun character-size-in-octets (char)
  (babel:string-size-in-octets
   (string char)
   :encoding
   #+(and sbcl win32)
   sb-impl::*default-external-format*
   #-(and sbcl win32)
   :utf-8))

(defmethod receive-message-using-transport ((transport stdio-transport) connection)
  (let* ((stream (connection-socket connection))
         (headers (read-headers stream))
         (length (ignore-errors (parse-integer (gethash "content-length" headers)))))
    (when length
      (let ((body
              (with-output-to-string (out)
                (loop
                  :for c := (read-char stream)
                  :do (write-char c out)
                      (decf length (character-size-in-octets c))
                      (when (<= length 0)
                        (return))))))
        (parse-message body)))))
