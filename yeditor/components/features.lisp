;;;; Copyright (c) 2026 Yuri Flores Albuquerque.
;;;; SPDX-License-Identifier: FSL-1.1-MIT
;;;; Licensed under the Functional Source License, Version 1.1 (see LICENSE-FSL).
;;;; Competing commercial use is strictly prohibited.

(defpackage :yeditor-components/features
  (:use :cl :lem :yeditor-context :yeditor-effects)
  (:export :le-inherited-entry-points-off
           :le-git
           :le-project
           :le-tree
           :*feature-keys*
           :*feature-hooks*))
(in-package :yeditor-components/features)

;;;; Lem binds the entry points of Git (legit), projects and the file tree (filer)
;;;; globally at load time, outside any transaction. le-inherited-entry-points-off
;;;; (part of the basal state) removes them; le-git, le-project and le-tree each put
;;;; theirs back as effects, so each feature can be switched off with no residue.

(defparameter *feature-keys*
  (list (cons :git (list (cons "C-x g" (quote lem/legit:legit-status))))
        (cons :project (list (cons "C-x p f" (quote lem-core/commands/project:project-find-file))
                             (cons "C-x p d" (quote lem-core/commands/project:project-root-directory))
                             (cons "C-x p K" (quote lem-core/commands/project:project-kill-buffers))
                             (cons "C-x p p" (quote lem-core/commands/project:project-switch))
                             (cons "C-x p s" (quote lem-core/commands/project:project-save))
                             (cons "C-x p u" (quote lem-core/commands/project:project-unsave))))
        (cons :tree (list (cons "C-x d" (quote lem/filer:filer)))))
  "Global entry-point bindings per feature, as Lem defines them.")

(defparameter *feature-hooks*
  (list (cons :tree (list (cons (quote *switch-to-buffer-hook*)
                                (quote lem/filer:sync-filer-to-buffer-directory)))))
  "Global hook functions per feature, as Lem installs them.")

(defun feature-effects (comp feature)
  (append (loop :for (keyspec . command) :in (cdr (assoc feature *feature-keys*))
                :collect (install-keybinding comp *global-keymap* keyspec command))
          (loop :for (hook . fn) :in (cdr (assoc feature *feature-hooks*))
                :collect (install-hook comp hook fn))))

(define-component le-inherited-entry-points-off
  :version "0.1.0"
  :install (lambda (comp ctx)
             (declare (ignore ctx))
             (append (loop :for (nil . keys) :in *feature-keys*
                           :append (loop :for (keyspec) :in keys
                                         :collect (install-key-removal comp *global-keymap* keyspec)))
                     (loop :for (nil . hooks) :in *feature-hooks*
                           :append (loop :for (hook . fn) :in hooks
                                         :collect (install-hook-removal comp hook fn))))))

(define-component le-git
  :version "0.1.0"
  :install (lambda (comp ctx) (declare (ignore ctx)) (feature-effects comp :git)))

(define-component le-project
  :version "0.1.0"
  :install (lambda (comp ctx) (declare (ignore ctx)) (feature-effects comp :project)))

(define-component le-tree
  :version "0.1.0"
  :install (lambda (comp ctx) (declare (ignore ctx)) (feature-effects comp :tree)))
