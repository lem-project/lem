(defpackage :lem-lisp-mode/v2/lsp-config
  (:use :cl
        :lem
        :lem-lsp-mode
        :lem-lsp-base/converter)
  (:import-from :lem-lisp-mode/v2/eval
                :register-eval-methods))
(in-package :lem-lisp-mode/v2/lsp-config)

(defvar *self-connection* nil)

(define-language-spec (lisp-spec lem-lisp-mode:lisp-mode)
  :language-id "lisp"
  :root-uri-patterns '(".asd")
  :command (lambda (port)
             (assert (not *self-connection*))
             `("lem-language-server"
               "--tcp"
               "--port" ,(princ-to-string port)
               "--log-file" ,(namestring
                              (merge-pathnames "language-server.log"
                                               (lem:lem-logdir-pathname)))))
  :connection-mode :tcp)

(defmethod lem-lsp-mode::initialized-workspace ((mode lem-lisp-mode:lisp-mode) workspace)
  (unless *self-connection*
    (let ((swank-port (gethash "swankPort" (lem-lsp-mode::workspace-server-info workspace))))
      (lem-lisp-mode:connect-to-swank "127.0.0.1" swank-port)))
  (register-eval-methods workspace))

(defun start-micros-server (port)
  (setf (lem-language-server::config :backend-port) port)
  (let* ((output (make-broadcast-stream))
         (*standard-output* output)
         (*error-output* output))
    (micros:create-server :port port)))

(defun start-language-server (port)
  (bt:make-thread (lambda ()
                    (lem-language-server:start-tcp-server port))))

(defmethod lem-lsp-mode::run-server ((spec lisp-spec))
  (if (not *self-connection*)
      (call-next-method)
      (let* ((lsp-port (lem-socket-utils:random-available-port))
             (micros-port (lem-socket-utils:random-available-port lsp-port)))
        (start-language-server lsp-port)
        (start-micros-server micros-port)
        (make-instance 'lem-lsp-mode/client:tcp-client :port lsp-port))))

;; override lisp-mode autodoc
(defmethod lem:execute :after ((mode lem-lisp-mode:lisp-mode) (command lem:self-insert) argument)
  )
