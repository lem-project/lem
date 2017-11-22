(defsystem "lem-webview"
  :depends-on ("plump"
               "clss")
  :serial t
  :components ((:file "webview")
               (:file "clhs")))

