(defpackage :lem
  (:use :cl
        :lem-base
        :lem/common/killring
        :lem/common/timer
        :lem/common/command)
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
   :exist-program-p)
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
   :with-pop-up-typeout-window)
  ;; color.lisp
  (:export
   :make-color
   :color-red
   :color-green
   :color-blue
   :parse-color)
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
   :set-attribute-reverse-p
   :set-attribute-bold-p
   :set-attribute-underline-p
   :attribute-foreground
   :attribute-background
   :attribute-reverse-p
   :attribute-bold-p
   :attribute-underline-p
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
   :enable-clipboard-p
   :copy-to-clipboard
   :get-clipboard-data)
  ;; file-ext.lisp
  (:export
   :define-file-type)
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
   :teardown-frames)
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
   :windowp
   :window-id
   :window-x
   :window-y
   :window-width
   :window-height
   :window-buffer
   :window-view
   :last-print-cursor-x
   :last-print-cursor-y
   :window-parameter
   :window-delete-hook
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
   :floating-window-p
   :header-window
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
   :mode-active-p
   :toggle-minor-mode
   :define-major-mode
   :define-minor-mode
   :change-buffer-mode
   :define-global-mode
   :change-global-mode-keymap)
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
   :lookup-keybind)
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
   :send-event
   :send-abort-event)
  ;; interp.lisp
  (:export
   :*exit-editor-hook*
   :interactive-p
   :continue-flag
   :pop-up-backtrace
   :call-background-job)
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
   :back-to-indentation-command
   :undo
   :redo
   :mark-set
   :exchange-point-mark
   :goto-line
   :filter-buffer
   :pipe-command
   :delete-trailing-whitespace
   :load-library)
  ;; self-insert-command.lisp
  (:export
   :self-insert-before-hook
   :self-insert-after-hook
   :self-insert)
  ;; file-command.lisp
  (:export
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
   :load-theme)
  (:export . #.(loop :for sym :being :the :external-symbols :of (find-package :lem-base)
                     :collect (make-symbol (string sym))))
  #+sbcl
  (:lock t))

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
   :redraw-view-after
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
   :display-menu
   :update-menu
   :delete-popup-message
   :clipboard-paste
   :clipboard-copy))

(defpackage :lem-user
  (:use :cl :lem))

(defpackage :lem-restart
  (:use)
  (:export :message
           :call-function)
  #+sbcl
  (:lock t))
