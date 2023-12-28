(defsystem "lem-ncurses"
  :depends-on ("cffi"
               "cl-charms"
               "cl-setlocale"
               "lem"
               "lem/extensions")
  :serial t
  :components (#+win32(:file "cl-charms-pdcurseswin32")
               (:file "term")
               (:file "clipboard")
               (:file "style")
               (:file "key")
               (:file "attribute")
               (:file "drawing-object")
               (:file "view")
               (:file "internal")
               (:file "ncurses")))
