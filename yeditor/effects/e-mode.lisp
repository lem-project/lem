(defpackage :yeditor-effects/e-mode
  (:use :cl :lem :yeditor-tx :yeditor-context)
  (:export :install-mode))
(in-package :yeditor-effects/e-mode)

(defun install-mode (comp buf mode)
  "Enable MODE on BUF. Returns an effect handle with undo that disables the mode.
   Mode toggling semantics are implementation-defined; here we simply call the mode function.
   The undo thunk does nothing (disabling a mode is not universally supported)."
  (let ((handle (install-effect comp :mode
                                 (lambda () (funcall mode buf))
                                 :undo-thunk (lambda () (declare (ignore))))))
    ;; store metadata
    (push (cons :mode mode) handle)
    (push (cons :buffer buf) handle)
    handle))
