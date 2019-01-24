(defsystem "lem-html-mode"
  :author "Eitaro Fukamachi"
  :license "MIT"
  :description "html mode for the Lem editor"
  :depends-on ("lem-core"
               "lem-xml-mode"
               "cl-ppcre")
  :serial t
  :components ((:file "html-mode")))
