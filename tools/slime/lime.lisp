(in-package :cl-user)
(defpackage lem-slime.lime
  (:use :cl)
  (:import-from :trivial-types
                :association-list
                :proper-list)
  (:import-from :lem-slime.swank-protocol
                :connect
                :connection-package)
  ;; Classes
  (:export :connection)
  ;; Accessors
  (:export :connection-package
           :connection-debug-level
           :connection-pid
           :connection-implementation-name
           :connection-implementation-version
           :connection-machine-type
           :connection-machine-version
           :connection-swank-version)
  ;; Functions and methods
  (:export :connect
           :debuggerp)
  (:local-nicknames (:swank-protocol :lem-slime.swank-protocol))
  (:documentation "A high-level Swank client."))
(in-package :lem-slime.lime)

;;; Classes

(defclass connection (swank-protocol:connection)
  ((debug-level :accessor connection-debug-level
                :initform 0
                :type integer
                :documentation "The depth at which the debugger is called.")
   (pid :accessor connection-pid
        :type integer
        :documentation "The PID of the Swank server process.")
   (implementation-name :accessor connection-implementation-name
                        :type string
                        :documentation "The name of the implementation running
 the Swank server.")
   (implementation-version :accessor connection-implementation-version
                           :type string
                           :documentation "The version string of the
 implementation running the Swank server.")
   (machine-type :accessor connection-machine-type
                 :type string
                 :documentation "The server machine's architecture.")
   (machine-version :accessor connection-machine-version
                    :type string
                    :documentation "The server machine's processor type.")
   (swank-version :accessor connection-swank-version
                  :type string
                  :documentation "The server's Swank version.")
   (info :accessor connection-info))
  (:documentation "A connection to a Swank server."))

;;; Functions and methods

(defmethod connect :after ((connection connection))
  "After connecting, query the Swank server for connection information and
create a REPL."
  ;; Issue every request
  (swank-protocol:request-connection-info connection)
  ;; Read the connection information message
  (let* ((info (swank-protocol:read-message connection))
         (data (getf (getf info :return) :ok))
         (impl (getf data :lisp-implementation))
         (machine (getf data :machine)))
    (setf (connection-info connection) info)
    (setf (connection-pid connection)
          (getf data :pid)

          (connection-implementation-name connection)
          (getf impl :name)

          (connection-implementation-version connection)
          (getf impl :version)

          (connection-machine-type connection)
          (getf machine :type)

          (connection-machine-version connection)
          (getf machine :version)

          (connection-swank-version connection)
          (getf data :version)))
  ;; Require some Swank modules
  (swank-protocol:request-swank-require
   connection
   '(swank-trace-dialog
     swank-package-fu
     swank-presentations
     swank-fuzzy
     swank-fancy-inspector
     swank-c-p-c
     swank-arglists
     swank-repl))
  (swank-protocol:read-message connection)
  ;; Start it up
  (swank-protocol:request-init-presentations connection)
  (swank-protocol:request-create-repl connection)
  ;; Wait for startup
  (swank-protocol:read-message connection)
  ;; Read all the other messages, dumping them
  (swank-protocol:read-all-messages connection))

(defun debuggerp (connection)
  "T if the connection is in the debugger, NIL otherwise."
  (> (connection-debug-level connection) 0))
