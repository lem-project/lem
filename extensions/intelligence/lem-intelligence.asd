(defsystem "lem-intelligence"
  :depends-on ("yason"
               "dexador"
               "babel"
               "lem/core")
  :serial t
  :components ((:module "lib"
                :components ((:file "ollama")
                             (:file "claude-code")))))
