(in-package :lem)

(export '(show-message
          clear-message
          message
          message-without-log
          message-buffer))

(defparameter *message-timeout* 1)

(defgeneric show-message (string &key timeout))
(defgeneric clear-message ())

(defun log-message (string args)
  (when string
    (let ((msg (apply #'format nil string args)))
      (let ((buffer (make-buffer "*Messages*")))
        (with-open-stream (stream (make-buffer-output-stream
                                   (buffer-end-point buffer)))
          (fresh-line stream)
          (princ msg stream))))))

(defun message-without-log (string &rest args)
  (if (null string)
      (clear-message)
      (show-message (apply #'format nil string args)
                    :timeout *message-timeout*)))

(defun message (string &rest args)
  (log-message string args)
  (apply #'message-without-log string args)
  t)

(defun message-buffer (buffer)
  (show-message buffer))
