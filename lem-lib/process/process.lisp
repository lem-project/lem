(defpackage :lem-process
  (:use :cl :lem)
  (:export :run-process
           :delete-process
           :process-send-input))
(in-package :lem-process)

(defstruct process
  pointer
  name
  buffer
  read-thread)

(defun run-process (program args &key name buffer)
  (let* ((buffer (get-buffer buffer))
         (pointer (async-process:create-process (cons program args) :nonblock nil))
         (thread (bt:make-thread
                  (lambda ()
                    (loop
                      (alexandria:when-let
                          (string (async-process:process-receive-output pointer))
                        (send-event `(write-to-buffer ,buffer ,string))))))))
    (make-process :pointer pointer
                  :name name
                  :buffer buffer
                  :read-thread thread)))

(defun write-to-buffer (buffer string)
  (insert-string (buffer-end-point buffer) string))

(defun delete-process (process)
  (bt:destroy-thread (process-read-thread process))
  (async-process:delete-process (process-pointer process)))

(defun process-send-input (process string)
  (async-process:process-send-input (process-pointer process) string))
