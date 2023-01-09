(in-package :lem-language-server)

(defvar *server*)

(defun current-server () *server*)

(defgeneric start-server (server))

(defmethod start-server :before (server)
  (expose-all-methods *server*))

(defclass server ()
  ((jsonrpc-server :initform (jsonrpc:make-server)
                   :reader server-jsonrpc-server)
   (client-capabilities :accessor server-client-capabilities)
   (shutdown-request-received :initform nil
                              :accessor server-shutdown-request-received-p)
   (backend-connection :initform nil
                       :accessor server-backend-connection)))

(defclass tcp-server (server)
  ((port :initarg :port
         :reader tcp-server-port)))

(defclass stdio-server (server)
  ())

(defmethod start-server ((server tcp-server))
  (with-yason-bindings ()
    (jsonrpc:server-listen (server-jsonrpc-server server)
                           :mode :tcp
                           :port (tcp-server-port server))))

(defmethod start-server ((server stdio-server))
  (with-yason-bindings ()
    (jsonrpc:server-listen (server-jsonrpc-server server)
                           :mode :stdio)))

(defmethod expose-all-methods ((server server))
  (loop :for class :in *method-classes*
        :for instance := (make-instance class)
        :do (jsonrpc:expose (server-jsonrpc-server server)
                            (lsp-method-name instance)
                            (curry #'call instance))))

(defun start-tcp-server (port)
  (setf *server* (make-instance 'tcp-server :port port))
  (start-server *server*))

(defun start-stdio-server ()
  (setf *server* (make-instance 'stdio-server))
  (start-server *server*))

(defun run-backend ()
  (unless (server-backend-connection *server*)
    (setf (server-backend-connection *server*)
          (micros/client:start-server-and-connect))))
