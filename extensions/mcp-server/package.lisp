(defpackage :lem-mcp-server
  (:use :cl)
  (:import-from :alexandria
                :when-let
                :if-let
                :hash-table-alist
                :with-gensyms)
  (:import-from :lem
                :buffer
                :buffer-list
                :buffer-name
                :buffer-filename
                :buffer-modified-p
                :buffer-text
                :buffer-point
                :buffer-nlines
                :buffer-read-only-p
                :get-buffer
                :make-buffer
                :delete-buffer
                :current-buffer
                :with-point
                :copy-point
                :delete-point
                :move-to-line
                :line-start
                :line-end
                :line-offset
                :character-offset
                :line-number-at-point
                :point-charpos
                :insert-string
                :delete-between-points
                :points-to-string
                :save-buffer
                :get-command
                :all-command-names
                :call-command
                :send-event
                :define-command
                :message
                :redraw-display
                :switch-to-buffer
                :pop-to-buffer)
  (:export :*mcp-server-auto-start*
           :*mcp-server-default-port*
           :current-mcp-server
           :start-mcp-server
           :stop-mcp-server
           :mcp-server-port
           :mcp-server-sessions
           :mcp-server-start
           :mcp-server-stop
           :mcp-server-status))
