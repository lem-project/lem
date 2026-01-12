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
                :pop-to-buffer
                ;; Window-related
                :window-list
                :window-x
                :window-y
                :window-width
                :window-height
                :window-buffer
                :window-view-point
                :window-use-modeline-p
                :floating-window-p
                :frame-floating-windows
                :frame-header-windows
                :current-frame
                :current-window
                :display-width
                :display-height
                ;; Cursor
                :window-cursor-x
                :window-cursor-y
                ;; Line content
                :line-string
                :end-buffer-p
                ;; Selection/Region
                :buffer-mark
                :mark-active-p
                ;; Mode
                :mode-name
                :buffer-major-mode
                :buffer-minor-modes
                ;; Prompt window
                :active-prompt-window
                :frame-prompt-window
                ;; Overlays (exported only)
                :overlay-start
                :overlay-end
                :overlay-attribute
                :overlay-buffer
                ;; Attributes
                :attribute-foreground
                :attribute-background
                :attribute-bold
                :attribute-underline
                :attribute-reverse
                ;; Header windows
                :header-window-p
                ;; Key input
                :execute-key-sequence
                :key-ctrl
                :key-meta
                :key-super
                :key-hyper
                :key-shift
                :key-sym
                ;; Errors
                :editor-error
                :editor-abort)
  (:export :*mcp-server-default-port*
           :*mcp-server-default-hostname*
           :current-mcp-server
           :start-mcp-server
           :stop-mcp-server
           :mcp-server-hostname
           :mcp-server-port
           :mcp-server-sessions
           :mcp-server-start
           :mcp-server-stop
           :mcp-server-status))
