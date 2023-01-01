(defpackage :lem-language-server/internal-rpc/server
  (:use :cl :lem-language-server/internal-rpc/rpc)
  (:export :start-server))
(in-package :lem-language-server/internal-rpc/server)

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
    (unless dispatcher
      (error 'unexpected-message :message message))
    (funcall dispatcher message)))

(defun read-loop (mailbox)
  (loop :for message := (read-message)
        :do (sb-concurrency:send-message mailbox message)))

(defun dispatch-loop (mailbox)
  (loop :for message := (sb-concurrency:receive-message mailbox)
        :do (dispatch-message message)))

(defvar *send-mutex* (sb-thread:make-mutex))

(defun send-message (message)
  (sb-thread:with-mutex (*send-mutex*)
    (write-message message)))

(defun start-server ()
  (let* ((mailbox (sb-concurrency:make-mailbox :name "internal-rpc read message"))
         (read-loop-thread
           (sb-thread:make-thread (lambda ()
                                    (read-loop mailbox)))))
    (unwind-protect (dispatch-loop mailbox)
      (sb-thread:terminate-thread read-loop-thread))))

;;;
(define-message :eval (form request-id)
  (let ((value (eval form)))
    (send-message `(:eval-return ,value ,request-id))))

;;;
(defun describe-symbol (symbol-name package-name)
  (let ((package (find-package package-name)))
    (when package
      (let* ((symbol (read-from-string-for-rpc symbol-name package))
             (text (with-output-to-string (output)
                     (describe symbol output))))
        text))))
