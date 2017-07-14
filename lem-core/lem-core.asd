(defsystem "lem-core"
  :depends-on ("uiop"
               "cl-charms"
               "alexandria"
               "swank"
               "trivial-gray-streams"
               "cl-ppcre"
               "inquisitor"
               "babel"
               "bordeaux-threads"
               "trivial-clipboard"
               "yason"
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

               (:file "tmlanguage")
               (:file "button")
               (:file "tabbar")
               (:file "listener-mode")
               (:file "kbdmacro")
               (:file "isearch")
               (:file "showparen")
               (:file "menu-mode")
               (:file "list-buffers")
               (:file "line-numbers")
               (:file "sourcelist")
               (:file "grep")
               (:file "go-back")
               (:file "language-mode")
               (:file "gtags")
               (:file "dired")
               (:file "abbrev")
               (:file "less-mode")
               (:file "rectangle")

               (:file "init")

               (:file "interface")

               (:file "term")
               (:file "ncurses")

               (:file "color-theme")))
