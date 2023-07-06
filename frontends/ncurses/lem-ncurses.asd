(defsystem "lem-ncurses"
  :depends-on ("cffi"
               "cl-charms"
               "cl-setlocale"
               "lem"
               "lem/extensions"
               ;; Include the system when it's ready and tested:
               ;; "lem/legit"
               )
  :serial t
  :components (#+win32(:file "cl-charms-pdcurseswin32")
               (:file "term")
               (:file "clipboard")
               (:file "style")
               (:file "key")
               (:file "ncurses")))
