;;;; Copyright (c) 2026 Yuri Flores Albuquerque.
;;;; SPDX-License-Identifier: FSL-1.1-MIT
;;;; Licensed under the Functional Source License, Version 1.1 (see LICENSE-FSL).
;;;; Competing commercial use is strictly prohibited.

(defpackage :yeditor-components
  (:use :cl :yeditor-context)
  (:import-from :yeditor-context/reactivity #:define-coeffects)
  (:import-from :yeditor-components/zero-chrome #:le-zero-chrome)
  (:import-from :yeditor-components/palette #:le-palette)
  (:import-from :yeditor-components/hud #:le-hud)
  (:export :enable-ephemeral-ui :disable-ephemeral-ui :le-zero-chrome :le-palette :le-hud))
(in-package :yeditor-components)

(defun enable-ephemeral-ui ()
  "Enter the zero-chrome basal state, bind the palette (C-c p) and let the agent HUD
   mount and unmount on the :agent-tx-active coeffect."
  (define-coeffects (quote le-hud) (list :agent-tx-active t))
  (install-component (quote le-zero-chrome))
  (install-component (quote le-palette)))

(defun disable-ephemeral-ui ()
  "Restore the chrome and bindings that existed before enable-ephemeral-ui."
  (uninstall-component (quote le-hud))
  (uninstall-component (quote le-palette))
  (uninstall-component (quote le-zero-chrome)))
