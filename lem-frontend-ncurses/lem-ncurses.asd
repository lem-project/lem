(defsystem "lem-ncurses"
  :depends-on ("cffi"
               "cl-charms"
               "trivial-clipboard"
               #+(or (and ccl unix) (and lispworks unix))"lem-setlocale"
               "lem")
  :serial t
  :components ((:file "term")
               (:file "ncurses")))
