(defsystem "lem-vi-mode"
  :author "cxxxr"
  :license "MIT"
  :description "vi mode for the Lem editor"
  :depends-on ("esrap")
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
