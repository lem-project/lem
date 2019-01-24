(defsystem "lem-markdown"
  :author "cxxxr"
  :license "MIT"
  :description "markdown viewer for the Lem editor"
  :depends-on ("lem-electron-backend"
               "parenscript")
  :serial t
  :components ((:file "markdown")))
