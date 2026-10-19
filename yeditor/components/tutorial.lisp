;;;; Copyright (c) 2026 Yuri Flores Albuquerque.
;;;; SPDX-License-Identifier: FSL-1.1-MIT
;;;; Licensed under the Functional Source License, Version 1.1 (see LICENSE-FSL).
;;;; Competing commercial use is strictly prohibited.

(defpackage :yeditor-components/tutorial
  (:use :cl :lem :yeditor-context :yeditor-effects)
  (:export :le-tutorial
           :yeditor-tutorial-file
           :yeditor-tutorial
           :yeditor-tutorial-editing))
(in-package :yeditor-components/tutorial)

;;;; The Yeditor tutorial: what Yeditor adds (quiet screen, sidebar and mouse, terminal,
;;;; themes, Python, agent transactions, Director Mode). Lem's own lessons on editing
;;;; text stay available as yeditor-tutorial-editing. le-tutorial makes Lem's tutorial
;;;; command (and the start screen's Tutorial entry) open this one; uninstalling it
;;;; gives Lem's back.

(defun yeditor-tutorial-file ()
  (asdf:system-relative-pathname "yeditor-components" "tutorial/yeditor-tutorial.txt"))

(define-command yeditor-tutorial () ()
  "Learn Yeditor: the quiet screen, panels, themes, Python and agent transactions."
  (lem-tutor:open-tutorial (yeditor-tutorial-file)))

(define-command yeditor-tutorial-editing () ()
  "Lem's lessons on editing text: movement, selection, search, undo, macros."
  (lem-tutor:open-tutorial (lem-tutor:basics-tutorial-file)))

(define-component le-tutorial
  :version "0.1.0"
  :install (lambda (comp ctx)
             (declare (ignore ctx))
             (list (install-variable comp (quote lem-tutor:*tutorial-file*) (yeditor-tutorial-file)))))
