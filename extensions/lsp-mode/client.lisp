(defpackage :lem-lsp-mode/client
  (:use :cl)
  (:import-from :jsonrpc)
  (:import-from :lem-lsp-mode/lem-stdio-transport
                :lem-stdio-transport)
  (:export :dispose
           :tcp-client
           :tcp-client-process
           :stdio-client
           :stdio-client-process
           :stdio-client-pid
           :stdio-client-alive-p)
  #+sbcl
  (:lock t))
(in-package :lem-lsp-mode/client)

(defgeneric dispose (client))

(defclass tcp-client (lem-language-client/client:client)
  ((port
    :initarg :port
    :reader tcp-client-port)
   (process
    :initform nil
    :initarg :process
    :reader tcp-client-process)))

(defmethod lem-language-client/client:jsonrpc-connect ((client tcp-client))
  (jsonrpc:client-connect (lem-language-client/client:client-connection client)
                          :mode :tcp
                          :port (tcp-client-port client)))

(defmethod dispose ((client tcp-client))
  (when (tcp-client-process client)
    (lem-process:delete-process (tcp-client-process client))))

(defclass stdio-client (lem-language-client/client:client)
  ((process :initarg :process
            :reader stdio-client-process)
   (pid :initform nil
        :reader stdio-client-pid)
   (disposed :initform nil
             :accessor stdio-client-disposed-p)
   (lock :initform (bt2:make-lock :name "stdio-client")
         :reader stdio-client-lock)))

(defmethod initialize-instance :after ((client stdio-client) &key)
  ;; async-process defines process-pid but does not export it.
  (setf (slot-value client 'pid)
        (async-process::process-pid (stdio-client-process client))))

(defmethod lem-language-client/client:jsonrpc-connect ((client stdio-client))
  ;; Connecting starts the transport threads; holding the lock keeps dispose
  ;; from freeing the process while they are being created.
  (bt2:with-lock-held ((stdio-client-lock client))
    (when (stdio-client-disposed-p client)
      (error "The language server client has been disposed"))
    (jsonrpc/client:client-connect-using-class (lem-language-client/client:client-connection client)
                                               'lem-stdio-transport
                                               :process (stdio-client-process client))))

(defun stdio-client-alive-p (client)
  "True while the server process runs. Never touches the process after dispose."
  (and (not (stdio-client-disposed-p client))
       (async-process:process-alive-p (stdio-client-process client))))

(defun wait-for-threads (threads timeout)
  (loop :with deadline := (+ (get-internal-real-time) (* timeout internal-time-units-per-second))
        :while (and (some #'bt2:thread-alive-p threads)
                    (< (get-internal-real-time) deadline))
        :do (sleep 0.01)))

(defmethod dispose ((client stdio-client))
  ;; delete-process frees the process structure that the transport's reading
  ;; thread may be blocked reading into; freeing it first corrupted the heap.
  ;; Kill the server so that read returns, stop and wait for the transport
  ;; threads, reap the child (kill(pid, 0) also succeeds on zombies), then free.
  (bt2:with-lock-held ((stdio-client-lock client))
   (unless (stdio-client-disposed-p client)
    (setf (stdio-client-disposed-p client) t)
    (let* ((pid (stdio-client-pid client))
           (connection (lem-language-client/client:client-connection client))
           (transport (ignore-errors (jsonrpc/base:jsonrpc-transport connection)))
           (threads (and transport
                         (copy-list (jsonrpc/transport/interface:transport-threads transport)))))
      (ignore-errors (sb-posix:kill pid sb-posix:sigkill))
      (when threads
        (ignore-errors (jsonrpc:client-disconnect connection))
        (wait-for-threads threads 2))
      (ignore-errors (sb-posix:waitpid pid 0))
      (async-process:delete-process (stdio-client-process client))))))
