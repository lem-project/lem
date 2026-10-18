;;;; Copyright (c) 2026 Yuri Flores Albuquerque.
;;;; SPDX-License-Identifier: FSL-1.1-MIT
;;;; Licensed under the Functional Source License, Version 1.1 (see LICENSE-FSL).
;;;; Competing commercial use is strictly prohibited.

;;; Launch Yeditor in the terminal (ncurses) with the ephemeral UI enabled.
;;; Usage, from the repository root: sbcl --load scripts/run-yeditor-ncurses.lisp
;;;   C-c b    toggle project sidebar (left)
;;;   C-c j    toggle integrated terminal (bottom)
;;;   C-c h    toggle Hermes Agent chat panel (bottom)
;;;   C-c w    open modern workspace layout (sidebar + editor)
;;;   C-c p    theme palette with live preview (C-g restores)
;;;   M-x yeditor-demo-agent   open a demo agent transaction (HUD mounts)
;;;   C-c C-a / C-c C-k        commit / roll back the agent transaction (HUD unmounts)
;;;   M-x yeditor-lsp-start / yeditor-lsp-stop-all   language servers as transactions (le-lsp)
(load ".qlot/setup.lisp")
(asdf:load-system :lem-ncurses)
(asdf:load-system :yeditor-components)
(asdf:load-system :yeditor-enterprise)
;; negative weight: run after Lem turns the tab bar on in its own after-init hook
(lem:add-hook lem:*after-init-hook* (quote yeditor-components:enable-ephemeral-ui) -10)
(lem:add-hook lem:*after-init-hook*
             (lambda ()
               (yeditor-context:install-component (quote yeditor-components:le-lsp))
               (yeditor-context:install-component (quote yeditor-enterprise/hermes-chat:le-hermes-chat))) -20)
;; the "lem" system also loads the webview frontend; select the terminal one explicitly
(lem:lem "--interface" "ncurses")
(uiop:quit)
