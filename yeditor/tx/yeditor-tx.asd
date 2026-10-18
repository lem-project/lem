(defsystem "yeditor-tx"
  :description "Transactional core for Yeditor"
  :depends-on ()
  :components ((:file "tx")
               (:file "context")
               (:file "side")))
