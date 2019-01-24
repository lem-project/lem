(defsystem "lem-ncurses"
  :author "cxxxr"
  :license "MIT"
  :description "ncurses backend for the Lem editor"
  :depends-on ("cffi"
               "cl-charms"
               "trivial-clipboard"
               #+(or (and ccl unix) (and lispworks unix))"lem-setlocale"
               "lem")
  :serial t
  :components (#+win32(:file "cl-charms-pdcurseswin32")
               (:file "term")
               (:file "ncurses")))
