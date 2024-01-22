(defsystem "lem-format"
  :depends-on (:lem)
  :serial t 
  :components ((:file "format")
               (:file "defaults")))
