(defpackage :lem-language-server/internal-rpc/server
  (:use :cl :lem-language-server/internal-rpc/rpc)
  (:export :start-server))
(in-package :lem-language-server/internal-rpc/server)

(defparameter *logging-pathname* (merge-pathnames "internal-rpc.log" (user-homedir-pathname)))

(defvar *logging-mutex* (sb-thread:make-mutex))

(defun do-log (control-string &rest format-arguments)
  (sb-thread:with-mutex (*logging-mutex*)
    (let ((string (apply #'format nil control-string format-arguments)))
      (with-open-file (out *logging-pathname*
                           :direction :output
                           :if-exists :append
                           :if-does-not-exist :create)
        (write-line string out)))))

;;;
(define-condition server-error (error) ())

(define-condition unexpected-message (server-error)
  ((message :initarg :message))
  (:report (lambda (c s)
             (with-slots (message) c
               (format s "Unexpected message: ~S" message)))))

(defvar *message-dispatcher* (make-hash-table :test 'eq))

(defmacro define-message (name (&rest parameters) &body body)
  (let ((message (gensym "MESSAGE")))
    `(setf (gethash ,name *message-dispatcher*)
           (lambda (,message)
             (destructuring-bind (,@parameters) (rest ,message)
               ,@body)))))

(defun dispatch-message (message)
  (check-type message cons)
  (let ((dispatcher (gethash (first message) *message-dispatcher*)))
    (when dispatcher
      (funcall dispatcher message))))

(defmacro with-io-streams (() &body body)
  `(call-with-io-streams (lambda () ,@body)))

(defun call-with-io-streams (function)
  (let* ((output (make-broadcast-stream))
         (input (make-broadcast-stream))
         ;; (io (make-two-way-stream input output))
         )
    (let ((*standard-output* output)
          (*standard-input* input)
          (*error-output* output)
          (*trace-output* output)
          ;; (*debug-io* io)
          ;; (*query-io* io)
          ;; (*terminal-io* io)
          )
      (funcall function))))

(defun read-loop (mailbox)
  (loop :for message := (read-message)
        :do (do-log "read message: ~S" message)
            (sb-concurrency:send-message mailbox message)))

(defun dispatch-loop (mailbox)
  (loop :for message := (sb-concurrency:receive-message mailbox)
        :do (dispatch-message message)))

(defvar *send-mutex* (sb-thread:make-mutex))

(defun send-message (message)
  (do-log "send message: ~S" message)
  (sb-thread:with-mutex (*send-mutex*)
    (write-message message)))

(defun start-server ()
  (do-log "start-server")
  (let* ((mailbox (sb-concurrency:make-mailbox :name "internal-rpc read message"))
         (read-loop-thread
           (sb-thread:make-thread (lambda ()
                                    (read-loop mailbox)))))
    (unwind-protect (dispatch-loop mailbox)
      (sb-thread:terminate-thread read-loop-thread))))

;;;
(define-message :eval (form request-id)
  (multiple-value-bind (value ok)
      ;; TODO: package, readtable
      (handler-case (values (with-io-streams ()
                              (eval form))
                            t)
        (error (condition)
          (values condition nil)))
    (let ((message
            (if ok
                `(:eval-return (:ok ,(prin1-to-string value)) ,request-id)
                `(:eval-return (:error ,(prin1-to-string value)) ,request-id))))
      (send-message message))))

;;;
(defun describe-symbol (symbol-name package-name)
  (let ((package (find-package package-name)))
    (when package
      (let* ((symbol (read-from-string-for-rpc symbol-name package))
             (text (with-output-to-string (output)
                     (describe symbol output))))
        text))))
