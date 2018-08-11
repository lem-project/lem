(defsystem "lem"
  :version "1.3"
  :depends-on ("lem-core"
               "lem-vi-mode"
               "lem-lisp-mode"
               "lem-go-mode"
               "lem-c-mode"
               "lem-xml-mode"
               "lem-python-mode"
               "lem-posix-shell-mode"
               "lem-markdown-mode"
               "lem-js-mode"
               "lem-css-mode"
               "lem-selection-mode"))

(defsystem "lem/executable"
  :build-operation program-op
  :build-pathname "lem"
  :entry-point "lem:main"
  :depends-on ("lem-ncurses"))
