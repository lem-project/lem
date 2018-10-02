(defsystem "lem-diff-mode"
  :depends-on ("lem-core"
               "cl-ppcre")
  :serial t
  :components ((:file "diff-mode")))
