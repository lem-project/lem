(defsystem "lem-webview"
  :depends-on ("webview"
               "float-features"
               "lem-server")
  :serial t
  :components ((:file "main")))
