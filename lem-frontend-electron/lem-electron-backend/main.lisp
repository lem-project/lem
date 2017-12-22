(defpackage :lem-electron-backend
  (:use :cl :lem :lem-jsonrpc)
  (:export :js-eval
           :set-html-pane
           :set-font
           :delete-html-pane
           :import-electron-module))

(in-package :lem-electron-backend)

(defun js-eval (string)
  (notify "js-eval" (params "string" string)))

(defun set-html-pane (name html)
  (notify "set-pane"
          (params "name" name
                  "html" (babel:string-to-octets html))))

(defun set-font (name size)
  (notify "set-font"
          (params "name" name
                  "size" size)))

(define-command delete-html-pane () ()
  (notify "delete-pane" nil))

(defvar *electron-modules* '())

(define-command import-electron-module (name) ("sImport: ")
  (pushnew name *electron-modules*)
  (notify "import" (params "name" (namestring name))))
