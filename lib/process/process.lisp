(in-package :lem-process)

(defclass process ()
  ((pointer
    :initarg :pointer
    :reader process-pointer)
   (name
    :initarg :name
    :reader process-name)
   (command
    :initarg :command
    :reader process-command)
   (buffer-stream
    :initarg :buffer-stream
    :reader process-buffer-stream)
   (read-thread
    :initarg :read-thread
    :reader process-read-thread)
   (output-callback
    :initarg :output-callback
    :reader process-output-callback)))

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
      (make-instance 'process :pointer pointer
                     :name name
                     :command command
                     :buffer-stream buffer-stream
                     :read-thread thread
                     :output-callback output-callback))))

(defun write-to-buffer (buffer-stream string output-callback)
  (write-string string buffer-stream)
  (when output-callback
    (funcall output-callback string)))

(defun delete-process (process)
  (when (bt:thread-alive-p (process-read-thread process))
    (bt:destroy-thread (process-read-thread process)))
  (async-process:delete-process (process-pointer process)))

(defun process-alive-p (process)
  (async-process:process-alive-p (process-pointer process)))

(defun process-send-input (process string)
  (async-process:process-send-input (process-pointer process) string))
