(in-package :lem-core)

(defparameter *message-timeout* 2)

(defvar *inhibit-message* nil
  "When true, `message` and `message-without-log` do nothing.

`process-each-cursors` binds this around the fake-cursor passes, so a command
that runs once per cursor reports its message once, from the real cursor.")

(defgeneric show-message (string &key timeout style &allow-other-keys))
(defgeneric clear-message ())

(defun log-message (string args)
  "Print a message.

This function creates the `*Messages*` buffer before it writes.

The first argument is a format control string, and the rest are data to be
formatted under control of the string."
  (when string
    (let ((msg (apply #'format nil string args))
          (buffer (make-buffer "*Messages*")))
      (with-open-stream (stream (make-buffer-output-stream
                                 (buffer-end-point buffer)))
        (fresh-line stream)
        (princ msg stream)))))

(defun message-without-log (string &rest args)
  "Print a message.

This function does not write into the `*Messages*` buffer.

The first argument is a format control string, and the rest are data to be
formatted under control of the string."
  (cond (*inhibit-message*)
        ((null string)
         (clear-message))
        (t
         (show-message (apply #'format nil string args)
                       :timeout *message-timeout*))))

(defun message (string &rest args)
  "Print a message.

The message goes into the `*Messages*` buffer and shows besides cursor.
Return t if success.

The first argument is a format control string, and the rest are data to be
formatted under control of the string."
  (unless *inhibit-message*
    (log-message string args)
    (apply #'message-without-log string args))
  (values))

(defun message-buffer (buffer)
  "Message BUFFER object."
  (show-message buffer))
