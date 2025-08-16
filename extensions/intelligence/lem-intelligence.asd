(defsystem "lem-intelligence"
  :depends-on ("yason"
               "dexador"
               "babel"
               "lem/core")
  :serial t
  :components ((:file "ollama")
               (:file "main")))
