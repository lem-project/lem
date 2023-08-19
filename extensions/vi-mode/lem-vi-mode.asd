(defsystem "lem-vi-mode"
  :depends-on ("esrap"
               "closer-mop"
               "lem"
               "cl-ppcre"
               "parse-number"
               "cl-package-locks"
               "alexandria"
               "split-sequence")
  :serial t
  :components ((:file "core")
               (:file "options")
               (:file "word")
               (:file "visual")
               (:file "jump-motions")
               (:module "commands-dir"
                :pathname "commands"
                :components
                ((:file "utils")))
               (:file "commands")
               (:file "ex-core")
               (:file "ex-parser")
               (:file "ex-command")
               (:file "ex")
               (:file "binds")
               (:file "vi-mode")))
