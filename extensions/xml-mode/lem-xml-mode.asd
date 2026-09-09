(defsystem "lem-xml-mode"
  :depends-on ("lem/core"
               "cl-ppcre")
  :serial t
  :components ((:file "xml-mode")))
