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
;;;   F1       Director Mode: submit one intent (natural language) to the agent
;;;   C-c C-a / C-c C-k        approve (commit) / revert (rollback) the agent transaction
;;;   C-c p    theme palette with live preview (C-g restores)
;;;   M-x yeditor-demo-agent   open a demo agent transaction (HUD mounts)
;;;   C-c C-a / C-c C-k        commit / roll back the agent transaction (HUD unmounts)
;;;   M-x yeditor-lsp-start / yeditor-lsp-stop-all   language servers as transactions (le-lsp)
(load ".qlot/setup.lisp")
(asdf:load-system :lem-ncurses)
;; terminal mouse (SGR 1006): clicks open sidebar items, switch tabs, focus windows,
;; drag separators and scroll, through Lem's core mouse handling
(asdf:load-system :lem-mouse-sgr1006)
(asdf:load-system :yeditor-components)
(asdf:load-system :yeditor-enterprise)
;; negative weight: run after Lem turns the tab bar on in its own after-init hook
(lem:add-hook lem:*after-init-hook* (quote yeditor-components:enable-ephemeral-ui) -10)
;; YEDITOR_MCP_PORT lets a second editor run beside one already serving the default port
(let ((port (uiop:getenv "YEDITOR_MCP_PORT")))
  (when (and port (plusp (length port)))
    (setf yeditor-components/mcp:*mcp-port* (parse-integer port))))
(lem:add-hook lem:*after-init-hook*
             (lambda ()
               ;; one component failing (e.g. the MCP port is taken) must not stop the others
               (dolist (name (list (quote yeditor-components:le-lsp)
                                   (quote yeditor-components:le-mcp)
                                   (quote yeditor-enterprise/hermes-chat:le-hermes-chat)))
                 (handler-case (yeditor-context:install-component name)
                   (error (e) (lem:message "~(~A~) not installed: ~A" name e))))) -20)
;; the "lem" system also loads the webview frontend; select the terminal one explicitly
(lem:lem "--interface" "ncurses")
(uiop:quit)
