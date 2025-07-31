(defsystem "lem-intelligence"
  :depends-on ("yason"
               "dexador"
               "babel"
               "lem")
  :serial t
  :components ((:file "ollama")
               (:file "main")))
