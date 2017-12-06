(defsystem "lem-vi-mode"
  :depends-on ("esrap")
  :serial t
  :components ((:file "core")
               (:file "word")
               (:file "visual")
               (:file "commands")
               (:file "ex-core")
               (:file "ex-parser")
               (:file "ex-command")
               (:file "ex")
               (:file "binds")
               (:file "vi-mode")))
