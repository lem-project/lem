(defsystem "lem-lispworks"
  :depends-on ("lem")
  :serial t
  :components ((:file "package")
               (:file "util")
               (:file "input")
               (:file "window-pane")
               (:file "lem-pane")
               (:file "main")
               (:file "popup-window")))
