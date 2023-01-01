(defpackage :lem-language-server/internal-rpc/rpc
  (:use :cl)
  (:export :prin1-to-string-for-rpc
           :read-from-string-for-rpc
           :write-message
           :read-message))
(in-package :lem-language-server/internal-rpc/rpc)

(defvar *io-package*
  (make-package :lem-language-server/internal-rpc/internal-package
                :use '()))

(defun prin1-to-string-for-rpc (message &optional (package *io-package*))
  (with-standard-io-syntax
    (let ((*print-case* :downcase)
          (*print-readably* nil)
          (*print-pretty* nil)
          (*package* package))
      (prin1-to-string message))))

(defun read-from-string-for-rpc (string &optional (package *io-package*))
  (with-standard-io-syntax
    (let ((*package* package)
          (*read-suppress* nil)
          (*read-eval* nil))
      (read-from-string string))))

(deftype message () 'cons)

(defun write-header (content-length stream)
  (format stream "~6,'0,X" content-length))

(defun write-message (message &optional (stream *standard-output*))
  (check-type message message)
  (let* ((string (prin1-to-string-for-rpc message))
         (length (length string)))
    (write-header length stream)
    (write-sequence string stream)
    (values)))

(defun read-header (stream)
  (let ((buffer (make-array 6 :element-type 'character)))
    (read-sequence buffer stream)
    (parse-integer buffer :radix 16)))

(defun read-message (&optional (stream *standard-input*))
  (let* ((length (read-header stream))
         (buffer (make-array length :element-type 'character)))
    (read-sequence buffer stream)
    (read-from-string-for-rpc buffer)))
