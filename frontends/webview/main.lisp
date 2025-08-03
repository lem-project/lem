(uiop:define-package :lem-webview
  (:use :cl)
  (:export :main))
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

(defun main ()
  (let ((port (lem/common/socket:random-available-port)))
    (bt2:make-thread (lambda () (lem-server:run-websocket-server :port port))
                     :name "lem-server")
    (run-webview :title "Lem"
                 :url (format nil "http://localhost:~D" port)
                 :width 1024
                 :height 768)
    (uiop:quit)))
