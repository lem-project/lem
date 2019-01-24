(defsystem "lem-pdcurses"
  :author "Hamayama <fkyo0985@gmail.com>"
  :license "MIT"
  :description "pdcurses backend for the Lem editor"
  :depends-on ("lem-ncurses")
  :serial t
  :components (#+win32(:file "ncurses-pdcurseswin32")))
