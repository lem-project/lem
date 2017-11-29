(defsystem "lem-markdown"
  :depends-on ("lem-electron-backend"
               "parenscript")
  :serial t
  :components ((:file "markdown")))
