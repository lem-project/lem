(asdf:defsystem "lem"
  :version "1.1"

  ;; Build an executable.
  :build-operation "program-op"
  :build-pathname "lem"
  :entry-point "lem:lem"

  :depends-on ("lem-core"
               "lem-vi-mode"
               "lem-lisp-mode"
               "lem-go-mode"
               "lem-c-mode"))
