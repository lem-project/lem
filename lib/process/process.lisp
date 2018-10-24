(in-package :lem-process)

(defstruct process
  pointer
  name
  buffer-stream
  read-thread
  output-callback)

(defun run-process (command &key name output-callback)
  (setf command (uiop:ensure-list command))
  (let ((buffer-stream (make-string-output-stream)))
    (let* ((pointer (async-process:create-process command :nonblock nil))
           (thread (bt:make-thread
                    (lambda ()
                      (loop
                        (unless (async-process:process-alive-p pointer)
                          (return))
                        (alexandria:when-let
                            (string (async-process:process-receive-output pointer))
                          (send-event (lambda ()
                                        (write-to-buffer buffer-stream string output-callback))))))
                    :name (format nil "run-process ~{~A~^ ~}" command))))
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
