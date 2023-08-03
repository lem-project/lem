(defsystem "lem-paredit-mode"
  :depends-on ("lem" "lem-vi-mode")
  :serial t
  :components ((:file "paredit-mode")))
