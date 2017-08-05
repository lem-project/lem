(defsystem "lem-ncurses"
  :depends-on ("cl-charms"
               "lem-core")
  :serial t
  :components ((:file "term")
               (:file "ncurses")))
