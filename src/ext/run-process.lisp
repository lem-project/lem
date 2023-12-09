(defpackage :lem/run-process
  (:use :cl :lem :ip-management/impl)
  (:export #:run-process)
  #+sbcl
  (:lock t))

(in-package :lem/run-process)

(defun run-process (command &key name output-callback output-callback-type directory)
  (declare (ignorable directory))
  (let* ((buffer-stream (make-string-output-stream))
         (process
           (make-process command
                         :name name
                         :stdin :stream
                         :stdout :stream
                         :stderr :stream
                         :buffer-stream buffer-stream
                         :output-callback output-callback
                         :output-callback-type output-callback-type))
         (thread (bt:make-thread
                  (lambda ()
                    (loop
                      (unless (process-alive-p process)
                        (return))
                      (alexandria:when-let ((string
                                             (process-get-last-output process)))
                        (send-event (lambda ()
                                      (write-to-buffer process string))))))
                  :name (format nil "run-process ~A }" command))))
    (set-process-read-thread thread process)
    process))

(defun write-to-buffer (process string)
  (let ((buffer-stream (ipm/impl::process-buffer-stream process))
        (output-callback (ipm/impl::process-output-callback process))
        (output-callback-type (ipm/impl::process-output-callback-type process)))
    (write-string string buffer-stream)
    (when output-callback
      (case output-callback-type
        (:process-input
         (funcall output-callback process string))
        (otherwise
         (funcall output-callback string))))))
