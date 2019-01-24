(defsystem "lem-core"
  :author "cxxxr"
  :license "MIT"
  :description "the core, higher level parts of the Lem editor"
  :depends-on ("uiop"
               "alexandria"
               "swank"
               "trivial-gray-streams"
               "cl-ppcre"
               "inquisitor"
               "babel"
               "bordeaux-threads"
               "yason"
               "cl-fad"
               "lem-base"
               "lem-encodings"
               "lem-lisp-syntax")
  :serial t
  :components ((:file "history")
               (:file "package")
               (:file "errors")
               (:file "key")
               (:file "macros")
               (:file "color")
               (:file "attribute")
               (:file "kill")
               (:file "file-ext")
               (:file "window")
               (:file "modeline")
               (:file "defcommand")
               (:file "mode")
               (:file "keymap")
               (:file "timer")
               (:file "event-queue")
               (:file "interp")
               (:file "input")
               (:file "overlay")
               (:file "streams")
               (:file "fundamental-mode")
               (:file "comp")
               (:file "minibuffer")
               (:file "typeout")
               (:file "lem")

               (:file "command")
               (:file "file-command")
               (:file "window-command")
               (:file "help-command")
               (:file "word-command")
               (:file "sexp-command")

               (:file "init")
               (:file "interface")
               (:file "popup-window")

               (:file "completion-mode")
               (:file "tmlanguage")
               (:file "button")
               (:file "listener-mode")
               (:file "universal-argument")
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
               (:file "directory-mode")
               (:file "abbrev")
               (:file "rectangle")
               (:file "auto-save")

               (:file "color-theme")
               (:file "site-init")))
