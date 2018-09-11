(defsystem "lem"
  :version "1.4"
  :depends-on ("lem-core"
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
               "lem-css-mode"
               "lem-selection-mode"
               "lem-rust-mode"
               "lem-calc-mode"
               "lem-paredit-mode"
               "lem-nim-mode"))

(defsystem "lem/executable"
  :build-operation program-op
  :build-pathname "lem"
  :entry-point "lem:main"
  :depends-on ("lem-ncurses"))

(defsystem "lem-jsonrpc/executable"
  :build-operation program-op
  :build-pathname "lem-rpc"
  :entry-point "lem:main"
  :depends-on ("lem-jsonrpc" "lem-electron-backend"))
