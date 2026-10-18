(defpackage :yeditor-effects/e-hook
  (:use :cl :lem :yeditor-tx :yeditor-context)
  (:export :install-hook))
(in-package :yeditor-effects/e-hook)

(defun install-hook (comp hook fn)
  "Add FN to HOOK. Returns an effect handle; undo removes the hook."
  (let ((handle (install-effect comp :hook
                                 (lambda () (add-hook hook fn))
                                 :undo-thunk (lambda () (remove-hook hook fn)))))
    (push (cons :hook hook) handle)
    (push (cons :fn fn) handle)
    handle))
