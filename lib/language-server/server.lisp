(in-package :lem-language-server)

(defvar *server*)

(defun current-server () *server*)

(defgeneric start-server (server))
(defgeneric run-backend (server))
(defgeneric swank-port (server))

(defmethod start-server :before (server)
  (setf *server* server)
  (expose-all-methods server)
  (unless (server-backend-connection server)
    (setf (server-backend-connection server)
          (run-backend server))))

(defclass server ()
  ((jsonrpc-server :initform (jsonrpc:make-server)
                   :reader server-jsonrpc-server)
   (client-capabilities :accessor server-client-capabilities)
   (shutdown-request-received :initform nil
                              :accessor server-shutdown-request-received-p)
   (backend-connection :initform nil
                       :accessor server-backend-connection)))

(defclass mock-server (server)
  ())

(defclass tcp-server (server)
  ((port :initarg :port
         :reader tcp-server-port)))

(defclass stdio-server (server)
  ())

(defmethod start-server ((server mock-server))
  nil)

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
                            (curry #'call-lsp-method instance))))

(defun start-tcp-server (port)
  (start-server (make-instance 'tcp-server :port port)))

(defun start-stdio-server ()
  (start-server (make-instance 'stdio-server)))

(defun run-backend-internal (deny-port)
  (let ((hostname (config :backend-hostname))
        (port (config :backend-port)))
    (if (and hostname port)
        (micros/client:connect hostname port)
        (micros/client:start-server-and-connect deny-port))))

(defmethod run-backend ((server tcp-server))
  (run-backend-internal (tcp-server-port server)))

(defmethod run-backend ((server stdio-server))
  (run-backend-internal nil))

(defmethod run-backend ((server mock-server))
  nil)

(defmethod swank-port ((server tcp-server))
  (micros/client:connection-swank-port
   (server-backend-connection server)))

(defmethod swank-port ((server stdio-server))
  (micros/client:connection-swank-port
   (server-backend-connection server)))

(defmethod swank-port ((server mock-server))
  nil)
