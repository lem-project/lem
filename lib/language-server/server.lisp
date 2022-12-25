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
                              :accessor shutdown-request-received-p)))

(defclass tcp-server (server)
  ((port :initarg :port
         :reader tcp-server-port)))

(defmethod start-server ((server tcp-server))
  (jsonrpc:server-listen (server-jsonrpc-server server)
                         :mode :tcp
                         :port (tcp-server-port server)))

(defmethod expose-all-methods ((server server))
  (loop :for class :in *method-classes*
        :for instance := (make-instance class)
        :do (jsonrpc:expose (server-jsonrpc-server server)
                            (lsp-method-name instance)
                            (curry #'call instance))))

(defun start-tcp-server ()
  (setf *server* (make-instance 'tcp-server :port 10003))
  (bt:make-thread (lambda ()
                    (start-server *server*))
                  :initial-bindings `((*standard-output* . ,*standard-output*)
                                      (*error-output* . ,*error-output*))))
