(defpackage :lem-scheme-mode.util
  (:use :cl)
  (:import-from :lem
                :random-range)
  (:export :port-available-p
           :random-port))
(in-package :lem-scheme-mode.util)

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

(defun random-port ()
  (loop :for port := (random-range 49152 65535)
        :when (port-available-p port)
        :return port))
