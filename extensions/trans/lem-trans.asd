(defsystem "lem-trans"
  :description "Translate text with translate-shell (trans)."
  :depends-on ("lem/core"
               "bordeaux-threads")
  :serial t
  :components ((:file "main")))
