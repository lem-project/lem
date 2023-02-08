(defpackage :lem-lisp-mode/language-client/lsp-config
  (:use :cl
        :lem
        :lem-lsp-mode
        :lem-lsp-base/converter))
(in-package :lem-lisp-mode/language-client/lsp-config)

(defvar *self-connection* nil)

(define-language-spec (micros-spec lem-lisp-mode:lisp-mode)
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
                                     "micros/evalResult"
                                     'micros/eval-result))

(defun start-micros-server (port)
  (setf (lem-language-server::config :backend-port) port)
  (micros:create-server :port port))

(defun start-language-server (port)
  (bt:make-thread (lambda ()
                    (lem-language-server:start-tcp-server port))))

(defmethod lem-lsp-mode::run-server ((spec micros-spec))
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

(define-command lisp-evaluate () ()
  (if (buffer-mark-p (current-buffer))
      (let ((start (region-beginning))
            (end (region-end)))
        (declare (ignore start end))
        #+(or)
        (lem-language-client/request::execute-command
         (lem-lsp-mode::workspace-client (lem-lsp-mode::buffer-workspace (current-buffer)))
         "cl-lsp.eval-range"
         ...TODO))
      (lem-language-client/request::execute-command
       (lem-lsp-mode::workspace-client (lem-lsp-mode::buffer-workspace (current-buffer)))
       "cl-lsp.eval-previous-form"
       (lem-lsp-mode::make-text-document-position-params (current-point)))))

(defun micros/eval-result (params)
  (let* ((params (convert-from-json params 'lem-language-server::eval-result-params))
         (text (lem-language-server::eval-result-params-message params)))
    (send-event (lambda ()
                  (display-popup-message text
                                         :style '(:gravity :cursor
                                                  :use-border nil
                                                  :background-color "dark cyan"))))))
