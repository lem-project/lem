;;;; Copyright (c) 2026 Yuri Flores Albuquerque.
;;;; SPDX-License-Identifier: FSL-1.1-MIT
;;;; Licensed under the Functional Source License, Version 1.1 (see LICENSE-FSL).
;;;; Competing commercial use is strictly prohibited.

(defpackage :yeditor-components/zero-chrome
  (:use :cl :lem :yeditor-context :yeditor-effects)
  (:export :le-zero-chrome :*minimal-modeline-format*))
(in-package :yeditor-components/zero-chrome)

(defparameter *minimal-modeline-format*
  (list " " (list (quote modeline-name)) (list (quote modeline-position) nil :right))
  "One-line modeline of the basal state: buffer name on the left, position on the right.")

(define-component le-zero-chrome
  :version "0.1.0"
  :install (lambda (comp ctx)
             (declare (ignore ctx))
             (list (install-editor-variable comp (quote lem/frame-multiplexer:frame-multiplexer) nil)
                   (install-editor-variable comp (quote modeline-format) *minimal-modeline-format*))))
