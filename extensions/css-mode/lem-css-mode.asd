(defsystem "lem-css-mode"
  :depends-on ("lem/core"
               "cl-ppcre")
  :serial t
  :components ((:file "css-mode")))
