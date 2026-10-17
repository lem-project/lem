(defsystem "yeditor-tx"
  :description "Transactional core for Yeditor"
  :depends-on ("lem")
  :components ((:file "tx")
               (:file "context")))
