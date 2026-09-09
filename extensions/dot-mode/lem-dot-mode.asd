(defsystem "lem-dot-mode"
  :depends-on ("lem/core"
               "cl-ppcre")
  :serial t
  :components ((:file "dot-mode")))
