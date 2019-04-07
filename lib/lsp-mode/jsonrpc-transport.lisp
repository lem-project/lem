(in-package :lem-lsp-mode)

#|
このファイルで使っているjsonrpcは次のフォークされたブランチに依存する
https://github.com/cxxxr/jsonrpc/tree/hijack-find-mode-class
|#

(defclass lem-stdio-transport (jsonrpc:transport)
  ((stream
    :reader lem-stdio-transport-stream)
   (program
    :initarg :program
    :initform (error ":program missing")
    :reader lem-stdio-transport-program)
   (arguments
    :initarg :arguments
    :initform (error ":arguments missing")
    :reader lem-stdio-transport-arguments)
   (process
    :reader lem-stdio-transport-process)
   (event-queue
    :initform (lem::make-event-queue)
    :reader lem-stdio-transport-event-queue)))

;; TODO: デストラクタで(delete-process process), (close stream)をする

(defmethod initialize-instance :around ((transport lem-stdio-transport) &rest initargs &key program arguments)
  (let ((process (lem-process:run-process (cons program arguments)
                                          :output-callback (lambda (string)
                                                             (process-output-callback transport string)))))
    (setf (slot-value transport 'process) process)
    (setf (slot-value transport 'stream) (lem-process:make-process-stream process)))
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
    (let ((string (format nil "Content-Length: ~A~C~C~:*~:*~C~C~A"
                          (length json)
                          #\Return
                          #\Newline
                          json)))
      (write-string string stream))
    (force-output stream)))

(defmethod jsonrpc/transport/interface:receive-message-using-transport
    ((transport lem-stdio-transport) connection)
  (let* ((stream (jsonrpc/connection:connection-socket connection))
         (headers (read-headers transport stream))
         (length (ignore-errors (parse-integer (gethash "content-length" headers)))))
    (when length
      (let ((body (make-string length)))
        (read-sequence body (lem-stdio-transport-stream transport))
        (jsonrpc/request-response:parse-message body)))))

(defun read-headers (transport stream)
  (let ((headers (make-hash-table :test 'equal)))
    (loop for line = (read-line* transport stream)
          until (equal (string-trim '(#\Return #\Newline) line) "")
          do (let* ((colon-pos (position #\: line))
                    (field (string-downcase (subseq line 0 colon-pos)))
                    (value (string-trim '(#\Return #\Space #\Tab) (subseq line (1+ colon-pos)))))
               (setf (gethash field headers) value)))
    headers))

(defun read-line* (transport stream)
  (cond ((listen stream)
         (read-line stream))
        ((lem-process:process-alive-p (lem-stdio-transport-process transport))
         (lem::dequeue-event nil (lem-stdio-transport-event-queue transport))
         (read-line stream))
        (t
         (error 'end-of-file :stream stream))))

(defun process-output-callback (transport string)
  (declare (ignore string))
  (lem:send-event t (lem-stdio-transport-event-queue transport)))
