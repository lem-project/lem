(defpackage :lem-language-server/protocol/utils
  (:use :cl)
  (:import-from :quri)
  (:export :pathname-to-uri
           :uri-to-pathname))
(in-package :lem-language-server/protocol/utils)

(defun pathname-to-uri (pathname)
  (format nil "file://~A" (namestring pathname)))

(defun uri-to-pathname (uri)
  (pathname (quri:uri-path (quri:uri uri))))
