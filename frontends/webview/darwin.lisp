(defpackage :lem-webview/darwin
  (:use :cl)
  (:export :set-window-appearance
           :dispatch-set-window-appearance))
(in-package :lem-webview/darwin)

;;; macOS window appearance via Objective-C runtime
;;;
;;; Uses objc_msgSend to set NSAppearance on the key window.
;;; set-window-appearance takes a webview handle (for pre-event-loop use).
;;; dispatch-set-window-appearance uses GCD + [NSApp keyWindow] so the
;;; editor thread can change appearance without holding a native handle.

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

(cffi:defcfun ("objc_msgSend" objc-msg-send/no-arg) :pointer
  (self :pointer)
  (sel :pointer))

(cffi:defcfun ("objc_msgSend" objc-msg-send/ptr) :pointer
  (self :pointer)
  (sel :pointer)
  (arg :pointer))

(cffi:defcfun ("objc_msgSend" objc-msg-send/str) :pointer
  (self :pointer)
  (sel :pointer)
  (arg :string))

;; Grand Central Dispatch — dispatch work to the main thread without
;; needing a webview handle.
(cffi:defcfun ("dispatch_async_f" dispatch-async-f) :void
  (queue :pointer)
  (context :pointer)
  (work :pointer))

(defun main-dispatch-queue ()
  "Return the main dispatch queue (dispatch_get_main_queue)."
  (cffi:foreign-symbol-pointer "_dispatch_main_q"))

(defun %get-key-window ()
  "Return the NSWindow that is the current key window, or a null pointer."
  (let* ((nsapp (objc-msg-send/no-arg
                 (objc-get-class "NSApplication")
                 (sel-register-name "sharedApplication")))
         (window (objc-msg-send/no-arg nsapp (sel-register-name "keyWindow"))))
    window))

(defun %set-nswindow-appearance (nswindow mode)
  "Set NSAppearance on NSWINDOW to MODE (:dark or :light).
Must be called on the main thread."
  (when (or (null nswindow) (cffi:null-pointer-p nswindow))
    (return-from %set-nswindow-appearance nil))
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
      (return-from %set-nswindow-appearance nil))
    (objc-msg-send/ptr nswindow
                       (sel-register-name "setAppearance:")
                       appearance)
    mode))

;; GCD callbacks — dispatch_async_f signature: void (*)(void *)
(cffi:defcallback gcd-set-dark-appearance :void ((context :pointer))
  (declare (ignore context))
  (%set-nswindow-appearance (%get-key-window) :dark))

(cffi:defcallback gcd-set-light-appearance :void ((context :pointer))
  (declare (ignore context))
  (%set-nswindow-appearance (%get-key-window) :light))

(defun set-window-appearance (webview-handle mode)
  "Set the macOS window frame appearance for WEBVIEW-HANDLE.
MODE should be :dark or :light. Called before the event loop starts,
so it runs directly on the main thread with the handle available."
  (%set-nswindow-appearance (webview:webview-get-window webview-handle) mode))

(defun dispatch-set-window-appearance (mode)
  "Set the macOS window frame appearance via GCD main queue dispatch.
Uses [NSApp keyWindow] to find the window, so no webview handle is needed.
Safe to call from any thread."
  (dispatch-async-f (main-dispatch-queue)
                    (cffi:null-pointer)
                    (ecase mode
                      (:dark (cffi:callback gcd-set-dark-appearance))
                      (:light (cffi:callback gcd-set-light-appearance))))
  mode)

) ; end #+darwin progn

#-darwin
(progn
(defun set-window-appearance (webview-handle mode)
  "No-op: window appearance is only supported on macOS."
  (declare (ignore webview-handle mode))
  nil)
(defun dispatch-set-window-appearance (mode)
  "No-op: window appearance is only supported on macOS."
  (declare (ignore mode))
  nil)
)
