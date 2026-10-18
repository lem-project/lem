(defpackage :yeditor-effects/e-variable
  (:use :cl :yeditor-tx)
  (:export :install-variable))
(in-package :yeditor-effects/e-variable)

(defun install-variable (comp var value)
  "Set dynamic global VAR to VALUE as a transaction effect (kind 6, Cap. 5.3).
   Captures the previous value (or :unbound) before install.
   Undo restores the previous value; if VAR was unbound when the effect was
   installed, undo calls makunbound.
   Re-override from multiple components uses LIFO order naturally (each
   effect captures the value it found; the last undo restores each layer)."
  (let* ((was-bound (boundp var))
         (prev (when was-bound (symbol-value var))))
    (let ((handle (install-effect comp :dynamic-global
                                   (lambda () (setf (symbol-value var) value))
                                   :undo-thunk
                                   (lambda ()
                                     (if was-bound
                                         (setf (symbol-value var) prev)
                                         (makunbound var))))))
      (push (cons :var var) handle)
      (push (cons :prev (if was-bound prev :unbound)) handle)
      handle)))
