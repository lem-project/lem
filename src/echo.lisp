(in-package :lem-core)

(defparameter *message-timeout* 2)

(defvar *cursor-messages* nil
  "Messages collected while a command runs once per cursor.

Bound by `call-with-cursor-messages' to a cons whose car holds the deferred
reports from fake-cursor passes, as (key . function) newest first, and whose
cdr holds the keys reported by the real cursor's pass.  NIL outside such a
command.  Like `*current-buffer*', this is dynamic state: `message' cannot
take it as an argument, and a binding stays local to the editor thread.")

(defvar *fake-cursor-pass-p* nil
  "True while a command runs for a fake cursor rather than the real one.")

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

(defun report-once (key function)
  "Call FUNCTION to report KEY, at most once per command over multiple cursors.

Outside a multiple-cursor command, call FUNCTION right away.  During a
fake-cursor pass, defer it.  During the real cursor's pass, call it and record
KEY so `call-with-cursor-messages' does not report the same KEY again."
  (let ((state *cursor-messages*))
    (cond ((null state)
           (funcall function))
          (*fake-cursor-pass-p*
           (unless (assoc key (car state) :test #'equal)
             (push (cons key function) (car state)))
           nil)
          (t
           (push key (cdr state))
           (funcall function)))))

(defun call-with-cursor-messages (function)
  "Call FUNCTION, which runs a command once per cursor, and report each message once.

Messages from the real cursor's pass are shown as usual.  Messages that only
fake cursors produced are shown after FUNCTION returns, so they are not lost.
A nested call joins the outer one."
  (if *cursor-messages*
      (funcall function)
      (let ((state (cons '() '())))
        (multiple-value-prog1 (let ((*cursor-messages* state))
                                (funcall function))
          (loop :for (key . report) :in (reverse (car state))
                :unless (member key (cdr state) :test #'equal)
                  :do (funcall report))))))

(defun call-as-fake-cursor-pass (function)
  "Call FUNCTION as the pass of a command for a fake cursor."
  (let ((*fake-cursor-pass-p* t))
    (funcall function)))

(defun message-without-log (string &rest args)
  "Print a message.

This function does not write into the `*Messages*` buffer.

The first argument is a format control string, and the rest are data to be
formatted under control of the string.

When a command runs once per cursor, the message is shown once."
  (if (null string)
      (report-once '(:clear) #'clear-message)
      (let ((text (apply #'format nil string args)))
        (report-once (list :message text)
                     (lambda ()
                       (show-message text :timeout *message-timeout*))))))

(defun message (string &rest args)
  "Print a message.

The message goes into the `*Messages*` buffer and shows besides cursor.
Return t if success.

The first argument is a format control string, and the rest are data to be
formatted under control of the string.

When a command runs once per cursor, the message is shown and logged once."
  (if (null string)
      (message-without-log nil)
      (let ((text (apply #'format nil string args)))
        (report-once (list :message text)
                     (lambda ()
                       (log-message "~A" (list text))
                       (show-message text :timeout *message-timeout*)))))
  (values))

(defun message-buffer (buffer)
  "Message BUFFER object.

When a command runs once per cursor, BUFFER is shown once."
  (report-once (list :buffer buffer)
               (lambda () (show-message buffer))))
