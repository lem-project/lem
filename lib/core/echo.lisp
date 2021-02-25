(in-package :lem)

(defgeneric show-message (string))
(defgeneric show-message-buffer (buffer))

(defun log-message (string args)
  (when string
    (let ((msg (apply #'format nil string args)))
      (let ((buffer (make-buffer "*Messages*")))
        (with-open-stream (stream (make-buffer-output-stream
                                   (buffer-end-point buffer)))
          (fresh-line stream)
          (princ msg stream))))))

(defun message-without-log (string &rest args)
  (show-message (if string
                    (apply #'format nil string args)
                    nil)))

(defun message (string &rest args)
  (log-message string args)
  (apply #'message-without-log string args)
  t)

(defun message-buffer (buffer)
  (show-message-buffer buffer))
