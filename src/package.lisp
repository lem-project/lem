(uiop:define-package :lem-core
  (:use :cl
        :lem/common/killring
        :lem/common/timer
        :lem/common/command)
  (:use-reexport :lem-base)
  ;; reexport common/killring
  (:export
   :with-killring-context)
  ;; reexport lem/common/command.lisp
  (:export
   :primary-command
   :make-command-table
   :add-command
   :remove-command
   :find-command
   :exist-command-p)
  ;; killring.lisp
  (:export
   :current-killring
   :copy-to-clipboard-with-killring
   :yank-from-clipboard-or-killring)
  ;; quicklisp-utils.lisp
  (:export
   :maybe-quickload)
  ;; config.lisp
  (:export
   :lem-home
   :lem-logdir-pathname
   :config)
  ;; errors.lisp
  (:export
   :editor-abort)
  ;; system.lisp
  (:export
   :get-pid
   :exist-program-p
   :lem-relative-pathname)
  ;; key.lisp
  (:export
   :make-key
   :key-p
   :key-ctrl
   :key-meta
   :key-super
   :key-hypher
   :key-shift
   :key-sym
   :match-key
   :insertion-key-sym-p
   :key-to-char)
  ;; macros.lisp
  (:export
   :with-current-window
   :with-disable-killring
   :with-pop-up-typeout-window)
  ;; color.lisp
  (:export
   :make-color
   :color-red
   :color-green
   :color-blue
   :parse-color
   :rgb-to-hsv
   :hsv-to-rgb)
  ;; attribute.lisp
  (:export
   :make-attribute
   :attribute
   :attribute-p
   :ensure-attribute
   :merge-attribute
   :set-attribute
   :set-attribute-foreground
   :set-attribute-background
   :set-attribute-reverse
   :set-attribute-bold
   :set-attribute-underline
   :attribute-foreground
   :attribute-background
   :attribute-reverse
   :attribute-bold
   :attribute-underline
   :get-attribute-cache
   :define-attribute
   :cursor
   :region
   :modeline
   :modeline-inactive
   :truncate-attribute
   :compiler-note-attribute
   :syntax-warning-attribute
   :syntax-string-attribute
   :syntax-comment-attribute
   :syntax-keyword-attribute
   :syntax-constant-attribute
   :syntax-function-name-attribute
   :syntax-variable-attribute
   :syntax-type-attribute
   :syntax-builtin-attribute
   :completion-attribute
   :non-focus-completion-attribute)
  ;; clipboard.lisp
  (:export
   :wsl-p
   :enable-clipboard
   :disable-clipboard
   :enable-clipboard-p
   :copy-to-clipboard
   :get-clipboard-data)
  ;; file.lisp
  (:export
   :get-file-mode
   :define-file-type
   :define-file-associations
   :define-program-name-with-mode)
  ;; screen.lisp
  (:export
   :screen-clear
   :screen-view)
  ;; frame.lisp
  (:export
   :update-prompt-window
   :frame
   :make-frame
   :frame-current-window
   :frame-window-tree
   :frame-floating-windows
   :frame-header-windows
   :frame-floating-prompt-window
   :frame-prompt-window
   :frame-message-window
   :notify-frame-redisplay-required
   :map-frame
   :get-frame
   :current-frame
   :unmap-frame
   :setup-frame
   :teardown-frame
   :teardown-frames
   :get-frame-of-window
   :focus-window-position)
  ;; mouse.lisp
  (:export
   :mouse-button-down-functions
   :receive-mouse-button-down
   :receive-mouse-button-up
   :receive-mouse-motion
   :receive-mouse-wheel
   :set-hover-message
   :get-point-from-window-with-coordinates)
  ;; echo.lisp
  (:export
   :show-message
   :clear-message
   :message
   :message-without-log
   :message-buffer)
  ;; prompt.lisp
  (:export
   :*prompt-activate-hook*
   :*prompt-deactivate-hook*
   :*prompt-buffer-completion-function*
   :*prompt-file-completion-function*
   :caller-of-prompt-window
   :prompt-active-p
   :active-prompt-window
   :get-prompt-input-string
   :prompt-for-character
   :prompt-for-y-or-n-p
   :prompt-for-string
   :prompt-for-integer
   :prompt-for-buffer
   :prompt-for-file
   :prompt-for-directory)
  ;; window.lisp
  (:export
   :line-wrap
   :*window-sufficient-width*
   :*scroll-recenter-p*
   :*window-scroll-functions*
   :*window-size-change-functions*
   :*window-show-buffer-functions*
   :window-parent
   :window-view-point
   :window
   :windowp
   :window-id
   :window-x
   :window-y
   :window-width
   :window-height
   :window-buffer
   :window-screen
   :window-view
   :window-point
   :window-cursor-invisible-p
   :last-print-cursor-x
   :last-print-cursor-y
   :window-parameter
   :window-delete-hook
   :window-leave-hook
   :window-use-modeline-p
   :window-redraw
   :current-window
   :window-list
   :one-window-p
   :deleted-window-p
   :window-recenter
   :window-scroll
   :window-cursor-x
   :window-cursor-y
   :move-to-next-virtual-line
   :move-to-previous-virtual-line
   :point-virtual-line-column
   :move-to-virtual-line-column
   :window-see
   :split-window-vertically
   :split-window-horizontally
   :split-window-sensibly
   :get-next-window
   :delete-window
   :get-buffer-windows
   :other-buffer
   :not-switchable-buffer-p
   :switch-to-buffer
   :pop-to-buffer
   :quit-window
   :left-window
   :right-window
   :up-window
   :down-window
   :make-floating-window
   :floating-window
   :floating-window-border
   :floating-window-border-shape
   :floating-window-p
   :header-window
   :update-on-display-resized
   :covered-with-floating-window-p
   :redraw-display)
  ;; popup.lisp
  (:export
   :*default-popup-message-timeout*
   :display-popup-message
   :delete-popup-message
   :display-popup-menu
   :popup-menu-update
   :popup-menu-quit
   :popup-menu-down
   :popup-menu-up
   :popup-menu-first
   :popup-menu-last
   :popup-menu-select)
  ;; modeline.lisp
  (:export
   :modeline-format
   :modeline-add-status-list
   :modeline-remove-status-list
   :modeline-clear-status-list
   :modeline-write-info
   :modeline-name
   :modeline-mode-names
   :modeline-position
   :modeline-posline
   :convert-modeline-element)
  ;; command.lisp
  (:export
   :executing-command-command
   :handle-signal
   :before-executing-command
   :after-executing-command
   :this-command
   :execute
   :call-command)
  ;; defcommand.lisp
  (:export
   :define-command)
  ;; mode.lisp
  (:export
   :ensure-mode-object
   :major-mode
   :mode-name
   :mode-description
   :mode-keymap
   :mode-syntax-table
   :mode-hook
   :mode-hook-variable
   :mode-active-p
   :find-mode
   :toggle-minor-mode
   :define-major-mode
   :define-minor-mode
   :change-buffer-mode
   :define-global-mode
   :change-global-mode-keymap
   :enable-minor-mode
   :disable-minor-mode)
  ;; keymap.lisp
  (:export
   :*keymaps*
   :keymap
   :make-keymap
   :*global-keymap*
   :define-key
   :keyseq-to-string
   :find-keybind
   :insertion-key-p
   :lookup-keybind
   :abort-key-p
   :with-special-keymap)
  ;; reexport common/timer
  (:export
   :timer
   :timer-name
   :timer-expired-p
   :make-timer
   :make-idle-timer
   :start-timer
   :stop-timer)
  ;; event-queue.lisp
  (:export
   :receive-event
   :send-event
   :send-abort-event
   :event-queue-length)
  ;; interp.lisp
  (:export
   :*exit-editor-hook*
   :interactive-p
   :continue-flag
   :pop-up-backtrace
   :call-background-job
   :command-loop-counter
   :command-loop
   :do-command-loop)
  ;; input.lisp
  (:export
   :*input-hook*
   :last-read-key-sequence
   :start-record-key
   :stop-record-key
   :key-recording-p
   :read-event
   :read-key
   :unread-key
   :read-command
   :read-key-sequence
   :unread-key-sequence
   :execute-key-sequence
   :sit-for)
  ;; overlay.lisp
  (:export
   :overlay-start
   :overlay-end
   :overlay-attribute
   :set-overlay-attribute
   :overlay-buffer
   :make-overlay
   :delete-overlay
   :overlay-put
   :overlay-get
   :clear-overlays
   :point-overlays)
  ;; streams.lisp
  (:export
   :buffer-input-stream
   :make-buffer-input-stream
   :buffer-output-stream
   :make-buffer-output-stream
   :editor-input-stream
   :make-editor-input-stream
   :editor-output-stream
   :make-editor-output-stream
   :make-editor-io-stream)
  ;; comp.lisp
  (:export
   :*file-completion-ignore-case*
   :completion
   :completion-test
   :completion-hypheen
   :completion-file
   :completion-strings
   :completion-buffer)
  ;; cursors.lisp
  (:export
   :cursor-region-beginning
   :cursor-region-end
   :buffer-cursors
   :make-fake-cursor)
  ;; typeout.lisp
  (:export
   :*typeout-mode-keymap*
   :typeout-mode
   :pop-up-typeout-window)
  ;; lem.lisp
  (:export
   :*before-init-hook*
   :*after-init-hook*
   :*splash-function*
   :setup-first-frame
   :find-editor-thread
   :lem
   :main)
  ;; primitive-command.lisp
  (:export
   :movable-advice
   :editable-advice
   :*set-location-hook*
   :undefined-key
   :exit-lem
   :quick-exit
   :keyboard-quit
   :escape
   :nop-command
   :unmark-buffer
   :*read-only-function*
   :toggle-read-only
   :rename-buffer
   :quoted-insert
   :newline
   :open-line
   :delete-next-char
   :delete-previous-char
   :copy-region
   :copy-region-to-clipboard
   :kill-region
   :kill-region-to-clipboard
   :kill-line
   :yank
   :yank-pop
   :yank-pop-next
   :yank-to-clipboard
   :paste-from-clipboard
   :next-line
   :next-logical-line
   :previous-line
   :previous-logical-line
   :forward-char
   :backward-char
   :move-to-beginning-of-buffer
   :move-to-end-of-buffer
   :move-to-beginning-of-line
   :move-to-beginning-of-logical-line
   :move-to-end-of-line
   :move-to-end-of-logical-line
   :next-page
   :previous-page
   :entab-line
   :detab-line
   :next-page-char
   :previous-page-char
   :delete-blank-lines
   :just-one-space
   :delete-indentation
   :transpose-characters
   :undo
   :redo
   :mark-set
   :exchange-point-mark
   :goto-line
   :filter-buffer
   :pipe-command
   :delete-trailing-whitespace
   :load-library
   :show-context-menu)
  ;; self-insert-command.lisp
  (:export
   :get-self-insert-char
   :self-insert-before-hook
   :self-insert-after-hook
   :self-insert)
  ;; file-command.lisp
  (:export
   :*find-file-executor*
   :find-file-executor
   :execute-find-file
   :find-file
   :read-file
   :add-newline-at-eof-on-writing-file
   :save-buffer
   :save-current-buffer
   :changefile-name
   :write-file
   :write-region-file
   :insert-file
   :save-some-buffers
   :sync-buffer-with-file-content
   :revert-buffer
   :revert-buffer-function
   :change-directory)
  ;; window-command.lisp
  (:export
   :select-buffer
   :kill-buffer
   :previous-buffer
   :next-buffer
   :recenter
   :split-active-window-vertically
   :split-active-window-horizontally
   :other-window
   :window-move-up
   :window-move-down
   :window-move-right
   :window-move-left
   :delete-other-windows
   :delete-active-window
   :quit-active-window
   :grow-window
   :shrink-window
   :grow-window-horizontally
   :shrink-window-horizontally
   :display-buffer
   :scroll-down
   :scroll-up
   :find-file-other-window
   :read-file-other-window
   :select-buffer-other-window
   :switch-to-last-focused-window
   :compare-windows)
  ;; help-command.lisp
  (:export
   :describe-key
   :describe-bindings
   :execute-command
   :apropos-command
   :lem-version)
  ;; word-command.lisp
  (:export
   :forward-word
   :previous-word
   :delete-word
   :backward-delete-word
   :downcase-region
   :uppercase-region
   :capitalize-word
   :lowercase-word
   :uppercase-word
   :forward-paragraph
   :backward-paragraph
   :kill-paragraph
   :count-words)
  ;; sexp-command.lisp
  (:export
   :forward-sexp
   :backward-sexp
   :forward-list
   :backward-list
   :down-list
   :backward-up-list
   :mark-sexp
   :kill-sexp
   :transpose-sexps)
  ;; multiple-cursors-command.lisp
  (:export
   :do-each-cursors)
  ;; display.lisp
  (:export
   :highlight-line)
  ;; interface.lisp
  (:export
   :with-implementation
   :implementation
   :native-scroll-support
   :redraw-after-modifying-floating-window
   :support-floating-window
   :set-foreground
   :set-background
   :display-width
   :display-height)
  ;; color-theme.lisp
  (:export
   :color-theme-names
   :define-color-theme
   :load-theme))
(sb-ext:lock-package :lem-core)

(uiop:define-package :lem
  (:use :cl)
  (:use-reexport :lem-core))
(sb-ext:lock-package :lem)

(defpackage :lem-interface
  (:nicknames :lem-if)
  (:use)
  (:export
   :*background-color-of-drawing-window*
   :invoke
   :get-background-color
   :update-foreground
   :update-background
   :display-width
   :display-height
   :make-view
   :delete-view
   :clear
   :set-view-size
   :set-view-pos
   :print
   :print-modeline
   :clear-eol
   :clear-eob
   :redraw-view-before
   :redraw-view-after
   :will-update-display
   :update-display
   :scroll
   :set-first-view
   :split-window-horizontally
   :split-window-vertically
   :display-popup-menu
   :popup-menu-update
   :popup-menu-quit
   :popup-menu-down
   :popup-menu-up
   :popup-menu-first
   :popup-menu-last
   :popup-menu-select
   :display-popup-message
   :delete-popup-message
   :display-context-menu
   :clipboard-paste
   :clipboard-copy
   :increase-font-size
   :decrease-font-size
   :resize-display-before
   :get-font-list
   :get-mouse-position))

(defpackage :lem-user
  (:use :cl :lem))

(defpackage :lem-restart
  (:use)
  (:export :message
           :call-function)
  #+sbcl
  (:lock t))
