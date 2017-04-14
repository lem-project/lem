(defsystem "lem-core"
  :depends-on ("uiop"
               "cl-charms"
               "cl-fad"
               "alexandria"
               "swank"
               "trivial-gray-streams"
               "cl-ppcre"
               "inquisitor"
               "babel"
               "lem-base"
               "lem-lisp-syntax")
  :serial t
  :components ((:file "history")
               (:file "package")
               (:file "errors")
               (:file "key")
               (:file "macros")
               (:file "attribute")
               (:file "kill")
               (:file "file-ext")
               (:file "window")
               (:file "modeline")
               (:file "defcommand")
               (:file "mode")
               (:file "keymap")
               (:file "timer")
               (:file "event")
               (:file "interp")
               (:file "input")
               (:file "overlay")
               (:file "streams")
               (:file "fundamental-mode")
               (:file "comp")
               (:file "minibuf")
               (:file "typeout")
               (:file "lem")

               (:file "command")
               (:file "file-command")
               (:file "window-command")
               (:file "help-command")
               (:file "word-command")
               (:file "sexp-command")

               (:file "listener-mode")
               (:file "kbdmacro")
               (:file "isearch")
               (:file "showparen")
               (:file "list-buffers")
               (:file "line-numbers")
               (:file "sourcelist")
               (:file "grep")
               (:file "language-mode")
               (:file "go-mode")
               (:file "dired")
               (:file "abbrev")

               (:file "init")

               (:file "interface")

               (:file "term")
               (:file "ncurses")

               (:file "color-theme")))
