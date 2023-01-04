(in-package :lem-language-server)

(defun run-backend ()
  (unless (server-backend-connection *server*)
    (setf (server-backend-connection *server*)
          (micros/client:start-server-and-connect))))

(defun describe-symbol (symbol-name package-name)
  (micros/client:remote-eval-sync (server-backend-connection *server*)
                                  `(micros/lsp-api:hover-symbol ,symbol-name)
                                  :package-name package-name))
