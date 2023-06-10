(defsystem "lem-vi-mode"
  :depends-on ("esrap"
               "closer-mop"
               "lem")
  :serial t
  :components ((:file "core")
               (:file "word")
               (:file "visual")
               (:file "jump-motions")
               (:file "commands")
               (:file "ex-core")
               (:file "ex-parser")
               (:file "ex-command")
               (:file "ex")
               (:file "binds")
               (:file "vi-mode")))
