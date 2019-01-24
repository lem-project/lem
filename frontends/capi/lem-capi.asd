(defsystem "lem-capi"
  :author "cxxxr"
  :license "MIT"
  :description "CAPI frontend for the Lem editor"
  :depends-on ("lem")
  :serial t
  :components ((:file "package")
               (:file "util")
               (:file "variables")
               (:file "input")
               (:file "directory-view")
               (:file "window-pane")
               (:file "window-panel")
               (:file "lem-panel")
               (:file "main")
               (:file "popup-window")
               (:file "menu")
               (:file "commands")))
