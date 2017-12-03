(defsystem "lem-vi-mode"
  :depends-on ()
  :serial t
  :components ((:file "package")
               (:file "modeline")
               (:file "state")
               (:file "commands")
               (:file "ex-parser")
               (:file "ex")
               (:file "word")
               (:file "vi-mode")))
