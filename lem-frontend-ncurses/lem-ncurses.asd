(defsystem "lem-ncurses"
  :depends-on ("cffi"
               "cl-charms"
               "trivial-clipboard"
               #+(and ccl unix)"lem-setlocale"
               "lem")
  :serial t
  :components ((:file "term")
               (:file "ncurses")))
