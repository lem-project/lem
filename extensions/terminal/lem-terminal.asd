(defsystem "lem-terminal"
  :depends-on ("lem/core")
  :serial t
  :components ((:file "ffi")
               (:file "terminal")
               (:file "terminal-mode")))
