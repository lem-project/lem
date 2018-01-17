(asdf:defsystem "lem-ncurses"
  :depends-on ("cffi"
               "cl-charms"
               "lem")
  :serial t
  :components ((:file "term")
               (:file "ncurses")))
