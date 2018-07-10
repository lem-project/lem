(defsystem "lem-ncurses"
  :depends-on ("cffi"
               "cl-charms"
               "trivial-clipboard"
               "lem")
  :serial t
  :components ((:file "term")
               (:file "ncurses")))
