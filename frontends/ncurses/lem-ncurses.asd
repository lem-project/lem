(defsystem "lem-ncurses"
  :depends-on ("cffi"
               "cl-charms"
               "trivial-clipboard"
               "cl-setlocale"
               "lem")
  :serial t
  :components (#+win32(:file "cl-charms-pdcurseswin32")
               (:file "term")
               (:file "ncurses")))
