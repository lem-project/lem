(uiop:define-package :lem-webview/browser
  (:use :cl)
  (:import-from :lem
                :define-command
                :editor-error
                :message)
  (:import-from :lem-webview
                :*webview-handle*)
  (:documentation "Open web pages in native webview windows.

A window opened here is created on the webview event loop thread and
lives alongside the main editor window. It is not part of the editor
itself: it has no buffer, and its contents cannot be controlled from
Lisp.")
  (:export :*popup-width*
           :*popup-height*
           :open-url
           :browser-open-url))
(in-package :lem-webview/browser)

(defvar *popup-width* 1024
  "Width in pixels of the windows opened by BROWSER-OPEN-URL.")

(defvar *popup-height* 768
  "Height in pixels of the windows opened by BROWSER-OPEN-URL.")

(defvar *popup-windows* '()
  "Native webview handles of the windows opened by BROWSER-OPEN-URL.

The webview library has no window-closed callback, so the native object
of a window the user has closed stays allocated. Keeping the handles
here makes that state visible rather than silently losing track of it.")

(defun url-address-p (string)
  "Return T when STRING starts with a URI scheme followed by \"//\"."
  (let ((colon (position #\: string)))
    (and colon
         (< 0 colon (1- (length string)))
         (char= (char string (1+ colon)) #\/)
         (loop :for char :across (subseq string 0 colon)
               :always (or (alpha-char-p char)
                           (digit-char-p char)
                           (find char "+-."))))))

(defun normalize-url (string)
  "Return STRING as a URL that can be navigated to.

A string without a URI scheme is treated as a host name, and a path to
an existing file is turned into a \"file://\" URL."
  (let ((string (string-trim '(#\Space #\Tab #\Newline) string)))
    (cond ((url-address-p string) string)
          ((probe-file string)
           (format nil "file://~A" (uiop:native-namestring (truename string))))
          (t (format nil "https://~A" string)))))

(cffi:defcallback %make-window-on-main-thread :void ((handle :pointer) (arg :pointer))
  "Create a window for the URL that ARG points to, then free ARG.

Runs on the webview event loop thread. WEBVIEW-CREATE shows the window
by itself, and the event loop of the main window drives it, so
WEBVIEW-RUN is never called on the new window."
  (declare (ignore handle))
  (unwind-protect
       (let ((url (cffi:foreign-string-to-lisp arg)))
         ;; GTK and WebKit are about to run on this thread; mask the
         ;; float traps so their arithmetic cannot raise an error.
         (float-features:with-float-traps-masked t
           (let ((window (webview:webview-create 0 (cffi:null-pointer))))
             (push window *popup-windows*)
             (webview:webview-set-title window url)
             (webview:webview-set-size window *popup-width* *popup-height* 0)
             (webview:webview-navigate window url))))
    (cffi:foreign-string-free arg)))

(defun open-url (url)
  "Open URL in a new native webview window.

URL is normalized by NORMALIZE-URL first. Returns the URL that was
opened. Signals an EDITOR-ERROR if the webview frontend is not running
or if the window could not be created."
  (let ((handle *webview-handle*))
    (unless handle
      (editor-error "No webview window is running"))
    (let* ((url (normalize-url url))
           (argument (cffi:foreign-string-alloc url))
           (error-code (webview:webview-dispatch
                        handle
                        (cffi:callback %make-window-on-main-thread)
                        argument)))
      (if (zerop error-code)
          url
          (progn
            ;; The callback only runs when the dispatch succeeds, so the
            ;; argument is still ours to free.
            (cffi:foreign-string-free argument)
            (editor-error "Failed to create a window for ~A (error ~D)"
                          url error-code))))))

(define-command browser-open-url (url) ((:string "Open URL: "))
  "Open URL in a new native webview window.

A string without a URI scheme is treated as a host name, so
\"example.com\" opens https://example.com."
  (message "Opened ~A" (open-url url)))
