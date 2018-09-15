(defsystem "lem-ncurses"
  :depends-on ("cffi"
               "cl-charms"
               "trivial-clipboard"
               #+(or (and ccl unix) (and lispworks unix))"lem-setlocale"
               "lem")
  :serial t
  :components (#+win32
               (:file "cl-charms_patch")
               (:file "term")
	       #+win32
               (:file "term_patch")
               (:file "ncurses")
	       #+win32
               (:file "ncurses_patch")))
