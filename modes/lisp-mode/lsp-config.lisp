(defpackage :lem-lisp-mode/language-client/lsp-config
  (:use :cl :lem-lsp-mode))
(in-package :lem-lisp-mode/language-client/lsp-config)

(define-language-spec (micros-spec lem-lisp-mode:lisp-mode)
  :language-id "lisp"
  :root-uri-patterns '(".asd")
  :command (lambda (port)
             `("lem-language-server"
               "--tcp"
               "--port" ,(princ-to-string port)
               "--log-file" ,(namestring
                              (merge-pathnames "language-server.log"
                                               (lem:lem-logdir-pathname)))))
  :mode :tcp)

(defmethod lem-lsp-mode::initialized-workspace ((mode lem-lisp-mode:lisp-mode) workspace)
  (let ((swank-port (gethash "swankPort" (lem-lsp-mode::workspace-server-info workspace))))
    (lem-lisp-mode:slime-connect "localhost" swank-port nil)))

;; override lisp-mode autodoc
(defmethod lem:execute :after ((mode lem-lisp-mode:lisp-mode) (command lem:self-insert) argument)
  )
