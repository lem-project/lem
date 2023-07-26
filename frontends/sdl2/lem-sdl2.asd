(defsystem "lem-sdl2"
  :depends-on ("sdl2"
               "sdl2-ttf"
               "sdl2-image"
               "lem"
               "lem/extensions")
  :serial t
  :components ((:file "resource")
               (:file "platform")
               (:file "keyboard")
               (:file "font")
               (:file "icon")
               (:file "main")
               (:file "text-buffer")
               (:file "image-buffer")
               (:file "tree")))
