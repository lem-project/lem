(defsystem "lem-webview"
  :depends-on ("lem-electron-backend"
               "plump"
               "clss")
  :serial t
  :components ((:file "lem-webview")
               (:file "clhs")))

