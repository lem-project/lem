(defsystem "lem-sdl2"
  :depends-on ("sdl2"
               "sdl2-ttf"
               "lem"
               "lem/extensions")
  :serial t
  :components ((:file "key")
               (:file "font")
               (:file "main")))
