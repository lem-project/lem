(uiop:define-package :lem-webview
  (:use :cl)
  (:export :lem-main
           :webview-main))
(in-package :lem-webview)

(defun run-webview (&key title url width height)
  (float-features:with-float-traps-masked t
    (let ((w (webview:webview-create 0 (cffi:null-pointer))))
      (unwind-protect
           (progn
             (webview:webview-set-title w title)
             (webview:webview-set-size w width height 0)
             (webview:webview-navigate w url)
             (webview:webview-run w))
        (webview:webview-destroy w)))))

(defun lem-main (&optional (args (uiop:command-line-arguments)))
  (let ((port (lem/common/socket:random-available-port)))
    (bt2:make-thread (lambda () (lem-server:run-websocket-server :port port :args args))
                     :name "lem-server")
    (run-webview :title "Lem"
                 :url (format nil "http://127.0.0.1:~D" port)
                 :width 1024
                 :height 768)
    (uiop:quit)))

(defun webview-main (&optional (args (uiop:command-line-arguments)))
  (let ((spec '((("url") :type string :optional nil :documentation "url")
                (("title") :type string :optional t :documentation "window's title")
                (("width") :type number :optional t :documentation "window's width")
                (("height") :type number :optional t :documentation "window's height")
                (("help" #\h) :type boolean :optional t :documentation "display help"))))
    (command-line-arguments:handle-command-line
     spec
     (lambda (&key url (title "lem-client") (width 800) (height 600))
       (unless url
         (command-line-arguments:show-option-help spec :sort-names t)
         (uiop:quit 1))
       (run-webview :url url :title title :width width :height height))
     :command-line args
     :name "lem-client")))
