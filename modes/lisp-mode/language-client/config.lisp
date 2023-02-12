(defpackage :lem-lisp-mode/language-client/config
  (:use :cl
        :lem
        :lem-lsp-mode
        :lem-lsp-base/converter))
(in-package :lem-lisp-mode/language-client/config)

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
  :mode :tcp)

(defmethod lem-lsp-mode::initialized-workspace ((mode lem-lisp-mode:lisp-mode) workspace)
  (unless *self-connection*
    (let ((swank-port (gethash "swankPort" (lem-lsp-mode::workspace-server-info workspace))))
      (lem-lisp-mode:slime-connect "localhost" swank-port nil)))
  (lem-lsp-mode::register-lsp-method workspace
                                     "lisp/showEvalResult"
                                     'lem-lisp-mode/language-client/eval::show-eval-result)
  (lem-lsp-mode::register-lsp-method workspace
                                     "lisp/startEval"
                                     'lem-lisp-mode/language-client/eval::start-eval))

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
        (lem-lsp-mode::make-server-info :port lsp-port))))

;; override lisp-mode autodoc
(defmethod lem:execute :after ((mode lem-lisp-mode:lisp-mode) (command lem:self-insert) argument)
  )
