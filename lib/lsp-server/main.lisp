(cl-lsp/defpackage:defpackage :cl-lsp/main
  (:use :cl
        :alexandria
        :cl-lsp/methods
        :cl-lsp/logger
        ;; :cl-lsp/eval
        )
  (:import-from :cl-lsp/server
                :tcp-server
                :stdio-server
                :server-listen)
  (:import-from :cl-lsp/config
                :with-environment
                :config)
  (:export :run-tcp-mode
           :run-stdio-mode))
(in-package :cl-lsp/main)

(defun start-swank-if-enabled ()
  (when-let ((port (config :swank :port)))
    (swank:create-server :dont-close t :port port)))

(defun run-tcp-mode (&key (port 10003))
  (unless (find-package :jsonrpc/transport/tcp)
    ;; XXX: https://github.com/cxxxr/jsonrpc/issues/27
    (ql:quickload :jsonrpc/transport/tcp :silent t))
  (with-environment :tcp
    (start-swank-if-enabled)
    (with-log-stream (*error-output*)
      (log-format "server-listen~%mode:tcp~%port:~D~%" port)
      (server-listen (make-instance 'tcp-server :port port)))))

(defun run-stdio-mode ()
  (with-environment :stdio
    (start-swank-if-enabled)
    (with-log-file ((config :log-pathname))
      (log-format "server-listen~%mode:stdio~%")
      (server-listen (make-instance 'stdio-server)))))
