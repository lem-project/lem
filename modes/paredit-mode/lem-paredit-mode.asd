(defsystem "lem-paredit-mode"
  :depends-on ("lem-core" "lem-vi-mode")
  :serial t
  :components ((:file "paredit-mode")))
