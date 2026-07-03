(uiop:define-package :lem-webview
  (:use :cl)
  (:import-from :lem-webview/darwin
   :set-window-appearance
   :dispatch-set-window-appearance)
  (:export :main
           :webview-main
           :webview))
(in-package :lem-webview)

(defvar *webview-handle* nil
  "Native webview handle, bound while the window's event loop is running.")

(defclass webview (lem-server:jsonrpc lem-core:implementation)
  ()
  (:documentation "Webview frontend implementation.
Combines the JSON-RPC server protocol with a native webview window."))

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

;; webview_dispatch callback: runs on the main (event loop) thread, where
;; calling webview_terminate is always legal.
(cffi:defcallback %terminate-on-main-thread :void ((w :pointer) (arg :pointer))
  (declare (ignore arg))
  (webview:webview-terminate w))

(defun terminate-webview ()
  "Stop the webview event loop so the main thread returns from webview-run.
Termination is dispatched onto the event loop thread via webview_dispatch.
A watchdog thread hard-exits the process in case the loop fails to stop
(or the window does not exist yet)."
  (let ((w *webview-handle*))
    (log:info "terminate-webview: handle=~A" w)
    (when w
      (log:info "terminate-webview: dispatch result=~A"
                (webview:webview-dispatch w
                                          (cffi:callback %terminate-on-main-thread)
                                          (cffi:null-pointer))))
    (bt2:make-thread (lambda ()
                       (sleep 3)
                       (uiop:quit 0 nil))
                     :name "lem-webview exit watchdog")))

(defmethod lem-if:set-frame-color ((implementation lem-server:jsonrpc) mode)
  "Set the window frame to :dark or :light mode.
Currently implemented for macOS via darwin.lisp; on other platforms this
is a no-op. Can be called from any thread."
  (dispatch-set-window-appearance mode))

(defun main (&optional (args (uiop:command-line-arguments)))
  (let ((port (lem/common/socket:random-available-port)))
    ;; uiop:quit from the editor thread cannot unwind the main thread
    ;; while it is blocked in the native webview event loop; terminate
    ;; the loop instead and let MAIN's own uiop:quit end the process.
    (setf lem-server:*exit-function* 'terminate-webview)
    (bt2:make-thread (lambda ()
                       (lem-server:run-websocket-server
                        :port port
                        :args args))
                     :name "lem-server")
    (run-webview :title "Lem"
                 :url (format nil "http://127.0.0.1:~D" port)
                 :width 1024
                 :height 768)
    ;; Hard exit: a clean exit would wait to unwind the server/editor
    ;; threads, which may be blocked in foreign calls (socket accept).
    (uiop:quit 0 nil)))

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
