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
               (:file "text-surface-cache")
               (:file "log")
               (:file "sdl2")
               (:file "icon-font")
               (:file "mouse")
               (:file "utils")
               (:file "main")
               (:file "text-buffer-impl")
               (:file "graphics")
               (:file "image-buffer")
               (:file "tree")))

(defsystem "lem-sdl2/executable"
  :build-operation program-op
  :build-pathname "../../lem"
  :entry-point "lem:main"
  :depends-on ("lem-sdl2"))
