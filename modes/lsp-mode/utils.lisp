(defpackage :lem-lsp-mode/utils
  (:use :cl)
  (:export :get-pid))
(in-package :lem-lsp-mode/utils)

(defun get-pid ()
  #+sbcl
  (sb-posix:getpid)
  #+ccl
  (ccl::getpid)
  #+lispworks
  (progn
    #+win32 (win32:get-current-process-id)
    #-win32 (system::getpid)))
