(defsystem "lem-js-mode"
  :depends-on ("lem"
               "lem-xml-mode")
  :serial t
  :components ((:file "js-mode")
               (:file "eslint")))
