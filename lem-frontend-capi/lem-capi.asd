(defsystem "lem-capi"
  :depends-on ("lem")
  :serial t
  :components ((:file "package")
               (:file "variables")
               (:file "util")
               (:file "input")
               (:file "editor-pane")
               (:file "directory-view")
               (:file "lem-panel")
               (:file "main")
               (:file "popup-menu")))
