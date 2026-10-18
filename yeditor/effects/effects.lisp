;;;; Copyright (c) 2026 Yuri Flores Albuquerque.
;;;; SPDX-License-Identifier: FSL-1.1-MIT
;;;; Licensed under the Functional Source License, Version 1.1 (see LICENSE-FSL).
;;;; Competing commercial use is strictly prohibited.

(defpackage :yeditor-effects
  (:use :cl)
  (:import-from :yeditor-effects/e-buffer    :install-buffer)
  (:import-from :yeditor-effects/e-mode      :install-mode)
  (:import-from :yeditor-effects/e-hook      :install-hook :install-hook-removal)
  (:import-from :yeditor-effects/e-keymap    :install-keybinding :install-key-removal)
  (:import-from :yeditor-effects/e-variable  :install-variable)
  (:import-from :yeditor-effects/e-command   :install-command)
  (:import-from :yeditor-effects/e-overlay   :install-overlay)
  (:import-from :yeditor-effects/e-timer       :install-timer)
  (:import-from :yeditor-effects/e-process     :install-process)
  (:import-from :yeditor-effects/e-server      :install-tcp-server)
  (:import-from :yeditor-effects/e-lsp         :install-lsp-session)
  (:import-from :yeditor-effects/e-fdefinition :install-fdefinition)
  (:import-from :yeditor-effects/e-editor-variable :install-editor-variable)
  (:import-from :yeditor-effects/e-window :install-side-window)
  (:import-from :yeditor-effects/e-theme :install-theme)
  (:import-from :yeditor-effects/e-treesitter :install-treesitter-grammar :ensure-grammar)
  (:export :install-buffer :install-mode :install-hook
           :install-keybinding :install-variable :install-command
           :install-overlay :install-timer
           :install-process :install-tcp-server :install-lsp-session
           :install-fdefinition :install-editor-variable :install-side-window :install-theme
           :install-key-removal :install-hook-removal
           :install-treesitter-grammar :ensure-grammar))
(in-package :yeditor-effects)
