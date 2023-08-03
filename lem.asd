(defsystem "lem"
  :version "2.1.0"
  :depends-on ("alexandria"
               "trivial-gray-streams"
               "trivial-types"
               "cl-ppcre"
	       "micros"
               "inquisitor"
               "babel"
               "bordeaux-threads"
               "yason"
               "log4cl"
               "split-sequence"
               "str"
               "dexador"
               "lem-base"
               "lem-encodings"
	       #+sbcl
	       sb-concurrency
	       "lem-mailbox")
  :pathname "src"
  :serial t
  :components ((:module "common"
                :components ((:file "ring")
                             (:file "killring")
                             (:file "history")
                             (:file "timer")
                             (:file "command")))
               (:file "internal-packages")
               (:file "quicklisp-utils")
               (:file "version")
               (:file "config")
               (:file "errors")
               (:file "system")
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
               (:file "mouse")
               (:file "context-menu")
               (:file "input")
               (:file "overlay")
               (:file "streams")
               (:file "fundamental-mode")
               (:file "completion")
               (:file "typeout")
               (:file "cursors")
               (:file "command-advices")
               (:file "interface")
               (:file "display")
               (:file "site-init")
               (:file "lem")

               (:file "color-theme")

               (:module "commands"
                :serial t
                :components ((:file "move")
                             (:file "edit" :depends-on ("move"))
                             (:file "mark")
                             (:file "word" :depends-on ("edit"))
                             (:file "s-expression" :depends-on ("edit"))
                             (:file "file" :depends-on ("edit"))
                             (:file "project" :depends-on ("file"))
                             (:file "buffer")
                             (:file "window" :depends-on ("move"))
                             (:file "multiple-cursors")
                             (:file "process")
                             (:file "help")
                             (:file "font")
                             (:file "other" :depends-on ("file"))
                             (:file "frame")))

               (:file "external-packages")

               (:module "ext"
                :serial t
                :components ((:file "popup-window")
                             (:file "popup-message")
                             (:file "popup-menu")
                             (:file "markdown-buffer")
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
                             (:file "peek-source")
                             (:file "grep")
                             (:file "go-back")
                             (:file "hover")
                             (:file "language-mode")
                             (:file "language-mode-tools")
                             (:file "thingatp")
                             (:file "gtags")
                             (:file "directory-mode")
                             (:file "abbrev")
                             (:file "rectangle")
                             (:file "auto-save")
                             (:file "tabbar")
                             (:file "frame-multiplexer")
                             (:file "filer")
                             (:file "deepl")
                             (:file "themes")
                             (:file "detective")))))

(defsystem "lem/extensions"
  :depends-on (#+sbcl
               "lem-lsp-mode"
               "lem-vi-mode"
               #+sbcl
               "lem-lisp-mode"
               #+sbcl
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
               #-clasp
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
               "lem-sql-mode"
               "lem-base16-themes"
               #+sbcl
               "lem-elixir-mode"
               "lem-documentation-mode"
               "lem-elisp-mode"))

(defsystem "lem/legit"
  :serial t
  :depends-on ("lem")
  :pathname "src"
  :components ((:module "ext/legit"
                :components ((:file "porcelain")
                             (:file "peek-legit")
                             (:file "legit")
                             (:file "legit-rebase")))
               (:module "scripts"
                :components ((:static-file "dumbrebaseeditor.sh")))))

(defsystem "lem/executable"
  :build-operation program-op
  :build-pathname "lem"
  :entry-point "lem:main"
  :depends-on ("lem-ncurses"))
