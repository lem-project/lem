(defsystem "lem-patch-mode"
  :depends-on ("lem/core"
               "cl-ppcre")
  :serial t
  :components ((:file "patch-mode")))
