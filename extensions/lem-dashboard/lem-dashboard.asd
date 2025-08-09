(defsystem "lem-dashboard"
  :depends-on (:lem/core)
  :serial t
  :components ((:file "lem-dashboard")
               (:file "dashboard-items")
               (:file "default-dashboard")))
