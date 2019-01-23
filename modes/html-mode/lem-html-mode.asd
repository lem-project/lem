(defsystem "lem-html-mode"
  :author "Eitaro Fukamachi"
  :license "MIT"
  :depends-on ("lem-core"
               "lem-xml-mode"
               "cl-ppcre")
  :serial t
  :components ((:file "html-mode")))
