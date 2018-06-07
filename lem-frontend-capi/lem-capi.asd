(defsystem "lem-capi"
  :depends-on ("lem")
  :serial t
  :components ((:file "package")
               (:file "util")
               (:file "lem-pane")
               (:file "main")
               (:file "popup-menu")))
