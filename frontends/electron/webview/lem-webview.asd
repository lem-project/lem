(defsystem "lem-webview"
  :author "cxxxr"
  :license "MIT"
  :description "web viewer for the Lem editor"
  :depends-on ("lem-electron-backend"
               "parenscript"
               "plump"
               "clss")
  :serial t
  :components ((:file "lem-webview")
               (:file "clhs")))

