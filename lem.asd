(defsystem "lem"
  :version "1.10.0"
  :depends-on ("alexandria"
               "trivial-gray-streams"
               "trivial-types"
               "cl-ppcre"
               "inquisitor"
               "babel"
               "bordeaux-threads"
               "yason"
               "log4cl"
               "split-sequence"
               "lem-base"
               "lem-encodings"
               "sb-concurrency")
  :pathname "src"
  :serial t
  :components ((:module "common"
                :components ((:file "ring")
                             (:file "killring")
                             (:file "history")
                             (:file "timer")
                             (:file "command")))
               (:file "package")
               (:file "quicklisp-utils")
               (:file "config")
               (:file "errors")
               (:file "system")
               (:file "signal-handler")
               (:file "key")
               (:file "macros")
               (:file "color")
               (:file "attribute")
               (:file "clipboard")
               (:file "killring")
               (:file "file")
               (:file "screen")
               (:file "frame")
               (:file "echo")
               (:file "prompt")
               (:file "window-tree")
               (:file "window")
               (:file "popup")
               (:file "modeline")
               (:file "command")
               (:file "defcommand")
               (:file "mode")
               (:file "keymap")
               (:file "event-queue")
               (:file "interp")
               (:file "input")
               (:file "overlay")
               (:file "streams")
               (:file "fundamental-mode")
               (:file "completion")
               (:file "typeout")
               (:file "cursors")
               (:file "lem")

               (:file "primitive-command")
               (:file "self-insert-command")
               (:file "file-command")
               (:file "window-command")
               (:file "help-command")
               (:file "word-command")
               (:file "sexp-command")
               (:file "multiple-cursors-command")

               (:file "default-keymap")

               (:file "interface")
               (:file "display")

               (:file "color-theme")
               (:file "site-init")

               (:module "ext"
                :serial t
                :components ((:file "popup-window")
                             (:file "popup-message")
                             (:file "popup-menu")
                             (:file "multi-column-list")
                             (:file "context-menu")
                             (:file "list-buffers")
                             (:file "completion-mode")
                             (:file "prompt-window")
                             (:file "tmlanguage")
                             (:file "button")
                             (:file "loading-spinner")
                             (:file "listener-mode")
                             (:file "universal-argument")
                             (:file "kbdmacro")
                             (:file "isearch")
                             (:file "showparen")
                             (:file "line-numbers")
                             (:file "sourcelist")
                             (:file "peek-source")
                             (:file "grep")
                             (:file "go-back")
                             (:file "language-mode")
                             (:file "language-mode-tools")
                             (:file "gtags")
                             (:file "directory-mode")
                             (:file "abbrev")
                             (:file "rectangle")
                             (:file "auto-save")
                             (:file "tabbar")
                             (:file "frame-multiplexer")))))

(defsystem "lem/extensions"
  :depends-on ("lem-lsp-mode"
               "lem-vi-mode"
               "lem-lisp-mode"
               "lem-go-mode"
               "lem-c-mode"
               "lem-xml-mode"
               "lem-html-mode"
               "lem-python-mode"
               "lem-posix-shell-mode"
               "lem-markdown-mode"
               "lem-js-mode"
               "lem-json-mode"
               "lem-css-mode"
               "lem-rust-mode"
               "lem-paredit-mode"
               "lem-nim-mode"
               "lem-scheme-mode"
               "lem-patch-mode"
               "lem-yaml-mode"
               "lem-review-mode"
               "lem-asciidoc-mode"
               "lem-dart-mode"
               "lem-scala-mode"
               "lem-dot-mode"
               "lem-java-mode"
               "lem-haskell-mode"
               "lem-ocaml-mode"
               "lem-asm-mode"
               "lem-makefile-mode"
               "lem-shell-mode"
               "lem-sql-mode"))

(defsystem "lem/executable"
  :build-operation program-op
  :build-pathname "lem"
  :entry-point "lem:main"
  :depends-on ("lem-ncurses"))
