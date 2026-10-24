;;;; Copyright (c) 2026 Yuri Flores Albuquerque.
;;;; SPDX-License-Identifier: FSL-1.1-MIT
;;;; Licensed under the Functional Source License, Version 1.1 (see LICENSE-FSL).
;;;; Competing commercial use is strictly prohibited.

(defpackage :yeditor-effects/e-window
  (:use :cl :lem :yeditor-tx)
  (:export :install-side-window))
(in-package :yeditor-effects/e-window)

(defun install-side-window (comp buffer &key width)
  "Show BUFFER (a buffer, or a buffer name resolved at commit time) in a new window split to the right of the current one, as a
   transaction effect (kind 15 ui-state, Cap. 5.3). Focus stays in the original
   window. Undo deletes the side window and returns focus to the original window
   if it still exists."
  (let ((origin nil) (side nil))
    (let ((handle (install-effect comp :ui-state
                                  (lambda ()
                                    (setf origin (current-window))
                                    (let ((before (window-list)))
                                      (split-window-horizontally origin :width width)
                                      (setf side (find-if-not (lambda (w) (member w before))
                                                              (window-list))))
                                    (with-current-window side
                                      (switch-to-buffer (if (stringp buffer) (get-buffer buffer) buffer)
                                                        nil nil)))
                                  :undo-thunk
                                  (lambda ()
                                    (when (and side (not (deleted-window-p side)))
                                      (delete-window side))
                                    (when (and origin (not (deleted-window-p origin)))
                                      (setf (current-window) origin))
                                    (setf side nil)))))
      (push (cons :buffer buffer) handle)
      handle)))
