(defsystem "lem-xml-mode"
  :author "gos-k <mag4.elan@gmail.com>"
  :license "MIT"
  :description "xml mode for the Lem editor"
  :depends-on ("lem-core"
               "cl-ppcre")
  :serial t
  :components ((:file "xml-mode")))
