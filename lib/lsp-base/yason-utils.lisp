(defpackage :lem-lsp-base/yason-utils
  (:use :cl)
  (:import-from :bordeaux-threads
                :*default-special-bindings*)
  (:export :with-yason-bindings
           :update-jsonrpc-yason-parameters
           :parse-json))
(in-package :lem-lsp-base/yason-utils)

(defparameter *yason-bindings*
  '((yason:*parse-json-null-as-keyword* . t)
    (yason:*parse-json-arrays-as-vectors* . t)))

(defmacro with-yason-bindings (() &body body)
  `(call-with-yason-bindings (lambda () ,@body)))

(defun call-with-yason-bindings (function)
  (let ((*default-special-bindings*
          (append *yason-bindings*
                  *default-special-bindings*)))
    (progv (mapcar #'car *yason-bindings*)
        (mapcar #'cdr *yason-bindings*)
      (funcall function))))

(defun update-jsonrpc-yason-parameters ()
  (setf jsonrpc/yason:*parse-json-null-as-keyword* t
        jsonrpc/yason:*parse-json-arrays-as-vectors* t))

(defun parse-json (input)
  (with-yason-bindings ()
    (yason:parse input)))
