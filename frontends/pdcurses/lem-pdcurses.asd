(defsystem "lem-pdcurses"
  :author "Hamayama <fkyo0985@gmail.com>"
  :license "MIT"
  :depends-on ("lem-ncurses")
  :serial t
  :components (#+win32(:file "ncurses-pdcurseswin32")))
