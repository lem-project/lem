(in-package :lem-language-server)

(defun run-backend ()
  (unless (server-backend-connection *server*)
    (setf (server-backend-connection *server*)
          (micros/client:start-server-and-connect))))
