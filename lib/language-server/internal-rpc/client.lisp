(defpackage :lem-language-server/internal-rpc/client
  (:use :cl
        :lem-language-server/internal-rpc/rpc))
(in-package :lem-language-server/internal-rpc/client)

(defvar *process* nil)

(defun run-server ()
  (setf *process*
        (async-process:create-process '("ros"
                                        "run"
                                        "-s"
                                        "lem-language-server/internal-rpc/server"
                                        "-e"
                                        "(lem-language-server/internal-rpc/server::start-server)"))))

(defun stop-server ()
  (async-process:delete-process *process*)
  (setf *process* nil))

(defvar *request-counter* 0)

(defun generate-request-id ()
  (incf *request-counter*))

(defun generate-eval-message (form)
  (let ((request-id (generate-request-id)))
    (with-output-to-string (out)
      (write-message `(:eval ,form ,request-id) out))))

(defun remote-eval (form)
  (let ((message (generate-eval-message form)))
    (async-process:process-send-input *process* message)))
