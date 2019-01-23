(defsystem "lem-webview"
  :author "cxxxr"
  :license "MIT"
  :depends-on ("lem-electron-backend"
               "parenscript"
               "plump"
               "clss")
  :serial t
  :components ((:file "lem-webview")
               (:file "clhs")))

