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

(defun binding-inverse (keymap keys)
  "Capture the prefix that define-key will change, including new prefix paths."
  (labels ((find-prefix (map key)
             (or (let ((prefix (find key (keymap-prefixes map)
                                        :key #'prefix-key :test #'equal)))
                   (when prefix (cons map prefix)))
                 (loop :for child :in (keymap-children map)
                       :thereis (find-prefix child key))))
           (capture (map remaining)
             (let* ((found (find-prefix map (first remaining)))
                    (prefix (cdr found)))
               (cond
                 ((null prefix)
                  ;; DEFINE-KEY creates this branch in MAP, even with a base map.
                  (let ((key (first remaining)))
                    (lambda ()
                      (setf (keymap-prefixes map)
                            (remove key (keymap-prefixes map)
                                    :key #'prefix-key :test #'equal)))))
                 ((and (rest remaining) (typep (prefix-suffix prefix) 'keymap))
                  (capture (prefix-suffix prefix) (rest remaining)))
                 (t
                  (let ((previous (prefix-suffix prefix))
                        (before (copy-list (keymap-prefixes (car found)))))
                    (lambda ()
                      (setf (keymap-prefixes (car found))
                            (append before
                                    (set-difference (keymap-prefixes (car found)) before))
                            (prefix-suffix prefix) previous))))))))
    (capture keymap keys)))

(defun install-keybinding (comp keymap keyspec command)
  "Bind string KEYSPEC to COMMAND as a reversible transaction effect.
   Each activation captures the binding and prefix structure it changes."
  (let ((inverse nil))
    (let ((handle (install-effect comp :keybinding
                                 (lambda ()
                                   (setf inverse
                                         (binding-inverse keymap (parse-keyspec keyspec)))
                                   (define-key keymap keyspec command))
                                 :undo-thunk
                                 (lambda ()
                                   (when inverse
                                     ;; Public removal also invalidates Lem's binding cache.
                                     (undefine-key keymap keyspec)
                                     (funcall inverse)
                                     (setf inverse nil))))))
      (push (cons :keymap keymap) handle)
      (push (cons :keyspec keyspec) handle)
      (push (cons :command command) handle)
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
