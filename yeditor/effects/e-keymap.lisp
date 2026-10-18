(defpackage :yeditor-effects/e-keymap
  (:use :cl :lem :yeditor-tx)
  (:export :install-keybinding))
(in-package :yeditor-effects/e-keymap)

(defun %prev-binding (keymap keyspec)
  "Return the current command bound to KEYSPEC in KEYMAP, or nil if unbound.
   Uses Lem internal parse-keyspec + prefix-for-key."
  (let* ((keys (lem::parse-keyspec keyspec))
         (prefix (lem::prefix-for-key keymap (car (last keys)))))
    (when prefix (lem::prefix-suffix prefix))))

(defun install-keybinding (comp keymap keyspec command)
  "Bind KEYSPEC to COMMAND in KEYMAP as a transaction effect (kind 5, Cap. 5.3).
   Captures the previous binding; undo restores it or calls undefine-key.
   KEYMAP is a lem:keymap; KEYSPEC is a string; COMMAND is a symbol."
  (let ((prev (%prev-binding keymap keyspec)))
    (let ((handle (install-effect comp :keybinding
                                   (lambda () (lem:define-key keymap keyspec command))
                                   :undo-thunk
                                   (lambda ()
                                     (if prev
                                         (lem:define-key keymap keyspec prev)
                                         (lem:undefine-key keymap keyspec))))))
      (push (cons :keymap keymap) handle)
      (push (cons :keyspec keyspec) handle)
      (push (cons :command command) handle)
      (push (cons :prev prev) handle)
      handle)))
