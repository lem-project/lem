(defsystem "lem-welcome"
  :depends-on (:lem/core :lem-vi-mode)
  :serial t 
  :components ((:file "welcome")))
