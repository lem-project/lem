(uiop:define-package :lem-webview
  (:use :cl)
  (:export :main
           :webview-main
           :webview
           :set-frame-color
           :*webview-handle*))
(in-package :lem-webview)

(defclass webview (lem-server:jsonrpc lem-core:implementation)
  ()
  (:documentation "Webview frontend implementation.
Combines the JSON-RPC server protocol with a native webview window."))

(defvar *webview-handle* nil
  "The native webview handle, set during run-webview.
This must be a dynamic variable because it is set on the main thread
(inside run-webview) and read from the editor thread (via set-frame-color).
The webview pointer is written once at startup and cleared on shutdown.")

(defmethod lem-if:invoke ((implementation webview) function)
  (main)
  (call-next-method))

(defun run-webview (&key title url width height (frame-color :dark))
  "Run the webview window. FRAME-COLOR is :dark or :light (macOS only)."
  (float-features:with-float-traps-masked t
    (let ((w (webview:webview-create 0 (cffi:null-pointer))))
      (setf *webview-handle* w)
      (unwind-protect
           (progn
             (webview:webview-set-title w title)
             (webview:webview-set-size w width height 0)
             (set-window-appearance w frame-color)
             (webview:webview-navigate w url)
             (webview:webview-run w))
        (setf *webview-handle* nil)
        (webview:webview-destroy w)))))

;; The editor thread's *implementation* is a jsonrpc instance (not webview),
;; because run-websocket-server passes --interface JSONRPC.  We specialize
;; on jsonrpc and guard with *webview-handle* so plain server frontends
;; get a no-op.
(defmethod lem-if:set-frame-color ((implementation lem-server:jsonrpc) mode)
  "Set the macOS window frame to :dark or :light mode.
Can be called at any time while the webview is running."
  (when *webview-handle*
    (dispatch-set-window-appearance *webview-handle* mode)))

(defun main (&optional (args (uiop:command-line-arguments)))
  (let ((port (lem/common/socket:random-available-port)))
    (bt2:make-thread (lambda ()
                       (lem-server:run-websocket-server
                        :port port
                        :args args))
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
