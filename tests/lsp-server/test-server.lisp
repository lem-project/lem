(lem-lsp-server/defpackage:defpackage :lem-lsp-server/test/test-server
  (:use :cl
        :lem-lsp-server/server)
  (:local-nicknames (:protocol :lem-lsp-utils/protocol)
                    (:json :lem-lsp-utils/json))
  (:export :test-server
           :call-lsp-method))
(in-package :lem-lsp-server/test/test-server)

(defclass test-server (abstract-server)
  ((method-table
    :initform (make-hash-table :test 'equal)
    :reader server-method-table)))

(defmethod register-request ((server test-server) request)
  (setf (gethash (request-method-name request) (server-method-table server))
        request))

(defun call-lsp-method (server name params)
  (let ((request (gethash name (server-method-table server))))
    (unless request
      (error "~A is not defined" name))
    (with-server (server)
      (funcall request params))))
