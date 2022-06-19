(defsystem "lem-common"
  :serial t
  :depends-on ("trivia"
               "closer-mop")
  :components ((:file "package")
               (:file "class")
               (:file "utils")))
