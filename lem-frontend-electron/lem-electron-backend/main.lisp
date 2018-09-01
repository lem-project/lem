(defpackage :lem-electron-backend
  (:use :cl :lem :lem-jsonrpc)
  (:export :js-eval
           :set-html-pane
           :set-font
           :babel
           :delete-html-pane
           :input-string
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

(define-command input-string (chars) ("sInput String")
  (let ((str-list (mapcar #'(lambda (char-octets) 
                              (babel:octets-to-string (make-array (list (length char-octets)) 
                                                                  :element-type '(unsigned-byte 8) 
                                                                  :initial-contents char-octets)))
                          chars)))
    (insert-string (current-point) (format nil "~{~A~}" str-list))))

(defvar *electron-modules* '())

(define-command import-electron-module (name) ("sImport: ")
  (pushnew name *electron-modules*)
  (notify "import" (params "name" (namestring name))))

(pushnew :lem-electron *features*)
