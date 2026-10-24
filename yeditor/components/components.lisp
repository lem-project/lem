;;;; Copyright (c) 2026 Yuri Flores Albuquerque.
;;;; SPDX-License-Identifier: FSL-1.1-MIT
;;;; Licensed under the Functional Source License, Version 1.1 (see LICENSE-FSL).
;;;; Competing commercial use is strictly prohibited.

(defpackage :yeditor-components
  (:use :cl :yeditor-context)
  (:import-from :yeditor-context/reactivity #:define-coeffects)
  (:import-from :yeditor-components/zero-chrome #:le-zero-chrome)
  (:import-from :yeditor-components/palette #:le-palette)
  (:import-from :yeditor-components/dark-theme #:le-dark-theme)
  (:import-from :yeditor-components/mode-line
                #:le-mode-line-minimal #:le-mode-line-buffer #:le-mode-line-mode
                #:le-mode-line-git #:le-mode-line-diagnostics #:le-mode-line-position #:le-tabs)
  (:import-from :yeditor-components/navigation #:le-navigation)
  (:import-from :yeditor-components/hud #:le-hud)
  (:import-from :yeditor-components/lsp #:le-lsp)
  (:import-from :yeditor-components/treesitter #:le-treesitter-packs)
  (:import-from :yeditor-components/context-bridge #:le-context-bridge)
  (:import-from :yeditor-components/lang-python #:le-lang-python #:enable-python-support)
  (:import-from :yeditor-components/ai #:le-models #:le-ollama)
  (:import-from :yeditor-components/mcp #:le-mcp)
  (:import-from :yeditor-components/features
                #:le-inherited-entry-points-off #:le-git #:le-project #:le-tree)
  (:import-from :yeditor-components/sidebar #:le-sidebar)
  (:import-from :yeditor-components/terminal #:le-terminal)
  (:import-from :yeditor-components/workspace-layout #:le-workspace-layout)
  (:import-from :yeditor-components/director-cockpit #:le-director-cockpit
                #:yeditor-director-intent #:yeditor-director-approve #:yeditor-director-rollback)
  (:import-from :yeditor-components/tutorial #:le-tutorial #:yeditor-tutorial)
  (:import-from :yeditor-components/themes-ergonomic #:le-themes-ergonomic #:le-theme-extended)
  (:export :le-navigation :enable-ephemeral-ui :disable-ephemeral-ui :le-zero-chrome :le-palette :le-hud
           :le-themes-ergonomic :le-theme-extended :le-dark-theme :le-lsp
           :le-mode-line-minimal :le-mode-line-buffer :le-mode-line-mode
           :le-mode-line-git :le-mode-line-diagnostics :le-mode-line-position :le-tabs
           :le-inherited-entry-points-off :le-git :le-project :le-tree :le-sidebar :le-terminal :le-workspace-layout
           :le-director-cockpit :yeditor-director-intent :yeditor-director-approve :yeditor-director-rollback
           :le-treesitter-packs :le-context-bridge :le-lang-python
           :le-models :le-ollama :le-mcp :le-tutorial :yeditor-tutorial))
(in-package :yeditor-components)

(defun enable-ephemeral-ui ()
  "Enable visible project/file navigation over the minimal workspace with the ergonomic theme pack (default Gruvbox Soft),
   Git/project/tree entry points owned by toggleable components (le-git, le-project,
   le-tree), bind the palette (C-c p), let the agent HUD mount and unmount on the
   :agent-tx-active coeffect, and enable the modern workspace layout (sidebar, terminal)."
  (define-coeffects (quote le-hud) (list :agent-tx-active t))
  (enable-python-support)
  (install-component (quote le-context-bridge))
  (install-component (quote le-zero-chrome))
  (install-component 'le-mode-line-minimal)
  (install-component 'le-mode-line-buffer)
  (install-component 'le-mode-line-position)
  (install-component (quote le-inherited-entry-points-off))
  (dolist (feature (list (quote le-git) (quote le-project) (quote le-tree)
                         (quote le-sidebar) (quote le-terminal)
                         (quote le-workspace-layout)
                         (quote le-director-cockpit) (quote le-tutorial)))
    (install-component feature))
  (install-component (quote le-themes-ergonomic))
  (install-component (quote le-dark-theme))
  (install-component (quote le-palette))
  (install-component (quote le-navigation)))

(defun disable-ephemeral-ui ()
  "Restore the chrome and bindings that existed before enable-ephemeral-ui."
  (uninstall-component (quote le-navigation))
  (uninstall-component (quote le-hud))
  (uninstall-component (quote le-lang-python))
  (uninstall-component (quote le-context-bridge))
  (uninstall-component (quote le-palette))
  (uninstall-component (quote le-dark-theme))
  (uninstall-component (quote le-themes-ergonomic))
  (dolist (feature (list (quote le-tutorial) (quote le-director-cockpit)
                         (quote le-workspace-layout) (quote le-terminal) (quote le-sidebar)
                          (quote le-tree) (quote le-project) (quote le-git)))
    (uninstall-component feature))
  (uninstall-component (quote le-inherited-entry-points-off))
  (uninstall-component 'le-tabs)
  (dolist (segment '(le-mode-line-position le-mode-line-diagnostics le-mode-line-git
                     le-mode-line-mode le-mode-line-buffer le-mode-line-minimal))
    (uninstall-component segment))
  (uninstall-component (quote le-zero-chrome)))
