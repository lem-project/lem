(defpackage :lem-language-server/internal-rpc/client
  (:use :cl
        :alexandria
        :lem-language-server/internal-rpc/rpc)
  (:export :run-server
           :stop-server
           :remote-eval))
(in-package :lem-language-server/internal-rpc/client)

(define-condition timeout-error (error) () (:report "Timeout"))

;;;
(defvar *request-counter* 0)

(defun generate-request-id ()
  (incf *request-counter*))

(defun generate-eval-message (form)
  (let ((request-id (generate-request-id)))
    (values (write-message-to-string `(:eval ,form ,request-id))
            request-id)))

;;;
(defvar *connection* nil)

(defclass connection ()
  ((terminated
    :initform nil
    :accessor connection-terminated-p)
   (process
    :accessor connection-process)
   (reader-thread
    :accessor connection-reader-thread)
   (request-continuations
    :initform '()
    :accessor connection-request-continuations
    :documentation "This slot is an ALIST with request-id and continuation function as elements.")
   (request-continuations-lock
    :initform (sb-thread:make-mutex)
    :reader connection-request-continuations-lock)))

(defun add-continuation (connection request-id continuation)
  (sb-thread:with-mutex ((connection-request-continuations-lock connection))
    (setf (connection-request-continuations connection)
          (acons request-id
                 continuation
                 (connection-request-continuations connection)))))

(defun get-and-remove-continuation (connection request-id)
  (sb-thread:with-mutex ((connection-request-continuations-lock connection))
    (let ((elt (assoc request-id (connection-request-continuations connection) :test #'equal)))
      (when elt
        (removef (connection-request-continuations connection)
                 request-id
                 :key #'car
                 :test #'equal)
        (cdr elt)))))

(defun run-server ()
  (let* ((connection (make-instance 'connection))
         (process
           (async-process:create-process
            '("ros"
              "run"
              "-s"
              "lem-language-server/internal-rpc/server"
              "-e"
              "(lem-language-server/internal-rpc/server::start-server)"))))
    ;; read-loop内でconnection-processを参照するので先に束縛が保証されている必要がある
    (setf (connection-process connection) process)
    (setf (connection-reader-thread connection)
          (sb-thread:make-thread
           (lambda () (read-loop connection))
           :name "internal-rpc read loop"))
    connection))

(defun stop-server (connection)
  (unless (connection-terminated-p connection)
    (async-process:delete-process (connection-process connection))
    (setf (connection-terminated-p connection) t)))

(defun receive-output (connection)
  (let ((string (async-process:process-receive-output (connection-process connection))))
    (log:info string)
    string))

(defun read-loop (connection)
  (loop :for payload := (receive-output connection)
        :for message := (read-message-from-string payload)
        :do (dispatch-message connection message)))

(defun dispatch-message (connection message)
  (destructuring-case message
    ((:eval-return result request-id)
     (let ((continuation (get-and-remove-continuation connection request-id)))
       (assert continuation)
       (funcall continuation result)))))

(defun remote-eval (connection form &key timeout)
  (let ((mailbox (sb-concurrency:make-mailbox)))
    (multiple-value-bind (message request-id) (generate-eval-message form)
      (add-continuation connection
                        request-id
                        (lambda (value)
                          (sb-concurrency:send-message mailbox value)))
      (async-process:process-send-input (connection-process connection)
                                        message)
      (multiple-value-bind (result timeout-p)
          (unwind-protect (sb-concurrency:receive-message mailbox :timeout timeout)
            (get-and-remove-continuation connection request-id))
        (unless timeout-p
          (error 'timeout-error))
        result))))
