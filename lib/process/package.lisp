(defpackage :lem-process
  (:use :cl :lem)
  (:export :process-io-stream
           :run-process
           :delete-process
           :process-alive-p
           :process-send-input)
  #+sbcl
  (:lock t))
