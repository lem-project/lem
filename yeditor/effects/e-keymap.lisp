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

(defun prune-created-prefixes (map keys)
  (let* ((prefix (find (first keys) (keymap-prefixes map) :key #'prefix-key :test #'equal))
         (child (and prefix (prefix-suffix prefix))))
    (when (typep child 'keymap)
      (when (rest keys) (prune-created-prefixes child (rest keys)))
      ;; DEFINE-KEY creates plain keymaps for intermediate prefixes. A custom
      ;; keymap* can carry command remappings/fallbacks even without prefixes.
      (when (and (not (typep child 'keymap*))
                 (null (keymap-prefixes child)) (null (keymap-children child))
                 (null (keymap-base child)) (null (keymap-properties child))
                 (null (keymap-description child)))
        (setf (keymap-prefixes map) (remove prefix (keymap-prefixes map) :test #'eq))))))

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
                  (lambda () (prune-created-prefixes map remaining)))
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
  "Remove KEYSPEC reversibly, retaining original prefix objects and ordering.
Undo preserves a binding supplied by the user after this removal."
  (let ((inverse nil))
    (let ((handle
            (install-effect
             comp :keybinding
             (lambda ()
               (setf inverse (and (%prev-binding keymap keyspec)
                                  (binding-inverse keymap (parse-keyspec keyspec))))
               (when inverse (undefine-key keymap keyspec)))
             :undo-thunk
             (lambda ()
               (when (and inverse (null (keymap-find keymap (parse-keyspec keyspec))))
                 ;; Invalidate lookup caches before restoring the captured objects.
                 (undefine-key keymap keyspec)
                 (funcall inverse))
               (setf inverse nil)))))
      (push (cons :keymap keymap) handle)
      (push (cons :keyspec keyspec) handle)
      handle)))
