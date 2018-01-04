(defsystem "lem-capi"
  :depends-on ("lem")
  :serial t
  :components ((:file "util")
               (:file "lem-pane")
               (:file "main")))
