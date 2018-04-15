(defsystem "lem"
  :version "1.2"
  :depends-on ("lem-core"
               "lem-vi-mode"
               "lem-lisp-mode"
               "lem-go-mode"
               "lem-c-mode"
               "lem-xml-mode"))

(defsystem "lem/executable"
  :build-operation program-op
  :build-pathname "lem"
  :entry-point "lem:lem"
  :depends-on ("lem-ncurses"))
