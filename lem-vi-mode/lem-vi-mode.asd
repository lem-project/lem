(defsystem "lem-vi-mode"
  :depends-on ()
  :serial t
  :components ((:file "mode")
               (:file "modeline")
               (:file "state")
               (:file "word")
               (:file "commands")
               (:file "ex-parser")
               (:file "ex")
               (:file "vi-mode")))
