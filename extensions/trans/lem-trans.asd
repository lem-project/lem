(defsystem "lem-trans"
  :description "Translate text with translate-shell (trans)."
  :depends-on ("lem/core"
               "bordeaux-threads"
               "str")
  :serial t
  :components ((:file "main")))
