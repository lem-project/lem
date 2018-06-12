(defsystem "lem-lispworks"
  :depends-on ("lem")
  :serial t
  :components ((:file "package")
               (:file "util")
               (:file "misc")
               (:file "input")
               (:file "window-pane")
               (:file "window-panel")
               (:file "lem-panel")
               (:file "main")
               (:file "popup-window")))
