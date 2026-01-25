(defsystem "lem/transient"
  :depends-on ("lem/core")
  :components ((:file "transient")
               (:file "keymap")
               (:file "popup")
               (:file "demo")))