(defpackage :lem-utils/socket
  (:use :cl)
  (:import-from :usocket)
  (:import-from :lem-utils)
  (:export :port-available-p
           :random-available-port))
(in-package :lem-utils/socket)

#+sbcl
(sb-ext:lock-package :lem-utils/socket)

(defun port-available-p (port)
  (let (socket)
    (unwind-protect
         (handler-case (progn
                         (setq socket (usocket:socket-listen "127.0.0.1" port :reuse-address nil))
                         port)
           (usocket:address-in-use-error () nil)
           (usocket:socket-error (e)
             (warn "USOCKET:SOCKET-ERROR: ~A" e)
             nil)
           #+sbcl
           (sb-bsd-sockets:socket-error (e)
             (warn "SB-BSD-SOCKETS:SOCKET-ERROR: ~A" e)
             nil))
      (when socket
        (usocket:socket-close socket)
        port))))

(defun random-available-port ()
  (loop :for port := (lem-utils:random-range 49152 65535)
        :when (port-available-p port)
        :return port))
