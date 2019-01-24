(defsystem "lem-js-mode"
  :author "Eitaro Fukamachi"
  :license "MIT"
  :description "javascript mode for the Lem editor"
  :depends-on ("lem-core"
               "lem-xml-mode")
  :serial t
  :components ((:file "js-mode")))
