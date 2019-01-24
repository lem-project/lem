(defsystem "lem-css-mode"
  :author "Satoaki Miyao"
  :license "MIT"
  :description "css mode for the Lem editor"
  :depends-on ("lem-core"
               "cl-ppcre")
  :serial t
  :components ((:file "css-mode")))
