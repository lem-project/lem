(in-package :lem-language-client)

(defclass lem-stdio-transport (jsonrpc:transport)
  ((stream
    :reader lem-stdio-transport-stream)
   (process
    :initarg :process
    :initform (error ":process missing")
    :reader lem-stdio-transport-process)))

(defmethod initialize-instance :around ((transport lem-stdio-transport) &rest initargs &key process)
  (setf (slot-value transport 'stream)
        (lem-process:make-process-stream process))
  (apply #'call-next-method transport initargs))

(defun find-mode-class (mode)
  (when (eq mode :lem-stdio)
    (find-class 'lem-stdio-transport)))

(pushnew 'find-mode-class jsonrpc:*find-mode-class-functions*)

(defmethod jsonrpc/transport/interface:start-client ((transport lem-stdio-transport))
  (let* ((stream (lem-stdio-transport-stream transport))
         (connection (make-instance 'jsonrpc/connection:connection
                                    :socket stream
                                    :request-callback (jsonrpc/transport/interface:transport-message-callback
                                                       transport))))
    (setf (jsonrpc/transport/interface:transport-connection transport) connection)
    (setf (jsonrpc/transport/interface:transport-threads transport)
          (list
           (bt:make-thread
            (lambda ()
              (jsonrpc/transport/interface:run-processing-loop transport connection))
            :name "jsonrpc/transport/lem-stdio processing")
           (bt:make-thread
            (lambda ()
              (jsonrpc/transport/interface:run-reading-loop transport connection))
            :name "jsonrpc/transport/lem-stdio reading")))
    connection))

(defmethod jsonrpc/transport/interface:send-message-using-transport
    ((transport lem-stdio-transport) connection message)
  (let ((json (with-output-to-string (s)
                (yason:encode message s)))
        (stream (jsonrpc/connection:connection-socket connection)))
    (format stream "Content-Length: ~A~C~C~:*~:*~C~C~A"
            (length json)
            #\Return
            #\Newline
            json)
    (force-output stream)))

(defmethod jsonrpc/transport/interface:receive-message-using-transport
    ((transport lem-stdio-transport) connection)
  (let* ((stream (jsonrpc/connection:connection-socket connection))
         (headers (read-headers stream))
         (length (ignore-errors (parse-integer (gethash "content-length" headers)))))
    (when length
      (let ((body (make-string length)))
        (read-sequence body (lem-stdio-transport-stream transport))
        (jsonrpc/request-response:parse-message body)))))

;; character stream
(defun read-headers (stream)
  (let ((headers (make-hash-table :test 'equal)))
    (loop for line = (read-line stream)
          until (equal (string-trim '(#\Return #\Newline) line) "")
          do (let* ((colon-pos (position #\: line))
                    (field (string-downcase (subseq line 0 colon-pos)))
                    (value (string-trim '(#\Return #\Space #\Tab) (subseq line (1+ colon-pos)))))
               (setf (gethash field headers) value)))
    headers))
