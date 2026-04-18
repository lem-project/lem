(in-package :lem-webview)

;;; macOS window appearance via Objective-C runtime
;;;
;;; Uses objc_msgSend to set NSAppearance on the NSWindow returned by
;;; webview_get_window. This controls the window frame (titlebar, traffic
;;; lights) color independent of the HTML content rendering.

#+darwin
(progn

(cffi:define-foreign-library libobjc
  (:os-macosx "libobjc.dylib"))

(cffi:use-foreign-library libobjc)

(cffi:defcfun ("objc_getClass" objc-get-class) :pointer
  (name :string))

(cffi:defcfun ("sel_registerName" sel-register-name) :pointer
  (name :string))

;; Fixed-arity objc_msgSend wrappers — avoids ARM64 variadic ABI issues.
;; objc_msgSend is not truly variadic; it uses the callee's ABI, so we
;; must declare the exact parameter types.

(cffi:defcfun ("objc_msgSend" objc-msg-send/ptr) :pointer
  (self :pointer)
  (sel :pointer)
  (arg :pointer))

(cffi:defcfun ("objc_msgSend" objc-msg-send/str) :pointer
  (self :pointer)
  (sel :pointer)
  (arg :string))

(defun %apply-window-appearance (webview-handle mode)
  "Apply appearance MODE (:dark or :light) to the window.
Must be called on the main thread."
  (let ((nswindow (webview:webview-get-window webview-handle)))
    (when (cffi:null-pointer-p nswindow)
      (return-from %apply-window-appearance nil))
    (let* ((appearance-name (ecase mode
                              (:dark "NSAppearanceNameDarkAqua")
                              (:light "NSAppearanceNameAqua")))
           (ns-appearance-class (objc-get-class "NSAppearance"))
           (ns-name (objc-msg-send/str
                     (objc-get-class "NSString")
                     (sel-register-name "stringWithUTF8String:")
                     appearance-name))
           (appearance (objc-msg-send/ptr
                        ns-appearance-class
                        (sel-register-name "appearanceNamed:")
                        ns-name)))
      (when (cffi:null-pointer-p appearance)
        (return-from %apply-window-appearance nil))
      (objc-msg-send/ptr nswindow
                         (sel-register-name "setAppearance:")
                         appearance)
      mode)))

(cffi:defcallback dispatch-set-dark-appearance :void
    ((webview-handle :pointer) (arg :pointer))
  (declare (ignore arg))
  (%apply-window-appearance webview-handle :dark))

(cffi:defcallback dispatch-set-light-appearance :void
    ((webview-handle :pointer) (arg :pointer))
  (declare (ignore arg))
  (%apply-window-appearance webview-handle :light))

(defun set-window-appearance (webview-handle mode)
  "Set the macOS window frame appearance for WEBVIEW-HANDLE.
MODE should be :dark or :light. Called before the event loop starts,
so it runs directly on the main thread."
  (%apply-window-appearance webview-handle mode))

(defun dispatch-set-window-appearance (webview-handle mode)
  "Set the macOS window frame appearance via webview_dispatch.
Use this when the event loop is already running to ensure the
Cocoa API calls execute on the main thread."
  (webview:webview-dispatch webview-handle
                            (ecase mode
                              (:dark (cffi:callback dispatch-set-dark-appearance))
                              (:light (cffi:callback dispatch-set-light-appearance)))
                            (cffi:null-pointer))
  mode)

) ; end #+darwin progn

#-darwin
(progn
(defun set-window-appearance (webview-handle mode)
  "No-op: window appearance is only supported on macOS."
  (declare (ignore webview-handle mode))
  nil)
(defun dispatch-set-window-appearance (webview-handle mode)
  "No-op: window appearance is only supported on macOS."
  (declare (ignore webview-handle mode))
  nil)
)
