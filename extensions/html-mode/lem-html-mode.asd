(defsystem "lem-html-mode"
  :depends-on ("lem/core"
               "lem-xml-mode"
               "cl-ppcre")
  :serial t
  :components ((:file "html-mode")))
