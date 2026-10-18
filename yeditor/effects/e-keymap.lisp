;;;; Copyright (c) 2026 Yuri Flores Albuquerque.
;;;; SPDX-License-Identifier: FSL-1.1-MIT
;;;; Licensed under the Functional Source License, Version 1.1 (see LICENSE-FSL).
;;;; Competing commercial use is strictly prohibited.

(defpackage :yeditor-effects/e-keymap
  (:use :cl :lem :yeditor-tx)
  (:export :install-keybinding :install-key-removal))
(in-package :yeditor-effects/e-keymap)

(defun %prev-binding (keymap keyspec)
  "Return the command currently bound to KEYSPEC in KEYMAP, or nil."
  (let ((prefix (keymap-find keymap (parse-keyspec keyspec))))
    (when prefix (prefix-suffix prefix))))

(defun install-keybinding (comp keymap keyspec command)
  "Bind KEYSPEC to COMMAND in KEYMAP as a transaction effect (kind 5, Cap. 5.3).
   Captures the previous binding; undo restores it or calls undefine-key."
  (let ((prev (%prev-binding keymap keyspec)))
    (let ((handle (install-effect comp :keybinding
                                   (lambda () (define-key keymap keyspec command))
                                   :undo-thunk
                                   (lambda ()
                                     (if prev
                                         (define-key keymap keyspec prev)
                                         (undefine-key keymap keyspec))))))
      (push (cons :keymap keymap) handle)
      (push (cons :keyspec keyspec) handle)
      (push (cons :command command) handle)
      (push (cons :prev prev) handle)
      handle)))

(defun install-key-removal (comp keymap keyspec)
  "Remove the binding of KEYSPEC in KEYMAP as a transaction effect (kind 5).
   The binding found at commit time is restored on undo."
  (let ((prev nil))
    (let ((handle (install-effect comp :keybinding
                                  (lambda ()
                                    (setf prev (%prev-binding keymap keyspec))
                                    (when prev (undefine-key keymap keyspec)))
                                  :undo-thunk
                                  (lambda ()
                                    (when prev (define-key keymap keyspec prev))))))
      (push (cons :keyspec keyspec) handle)
      handle)))
