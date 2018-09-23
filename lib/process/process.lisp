(in-package :lem-process)

(defstruct process
  pointer
  name
  buffer-stream
  read-thread
  output-callback)

(defun run-process (program args &key name output-callback)
  (let ((buffer-stream (make-string-output-stream)))
    (let* ((pointer (async-process:create-process (cons program args) :nonblock nil))
           (thread (bt:make-thread
                    (lambda ()
                      (loop
                        (alexandria:when-let
                            (string (async-process:process-receive-output pointer))
                          (send-event (lambda ()
                                        (write-to-buffer buffer-stream string output-callback))))))
                    :name (format nil "run-process ~A ~A" program args))))
      (make-process :pointer pointer
                    :name name
                    :buffer-stream buffer-stream
                    :read-thread thread
                    :output-callback output-callback))))

(defun write-to-buffer (buffer-stream string output-callback)
  (write-string string buffer-stream)
  (when output-callback
    (funcall output-callback string)))

(defun delete-process (process)
  (bt:destroy-thread (process-read-thread process))
  (async-process:delete-process (process-pointer process)))

(defun process-alive-p (process)
  (async-process:process-alive-p (process-pointer process)))

(defun process-send-input (process string)
  (async-process:process-send-input (process-pointer process) string))

(defun make-process-stream (process)
  (make-instance 'process-io-stream :process process))
