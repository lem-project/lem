(defsystem "lem-spell-check"
  :description "Spell checking for Lem using the Enchant library"
  :author "Lem Project"
  :license "MIT"
  :depends-on ("lem/core"
               "enchant")
  :serial t
  :components ((:file "spell-check")))
