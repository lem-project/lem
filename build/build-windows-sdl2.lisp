(ql:quickload :lem-sdl2)

(setq lem::*deployed* t)

(cffi:close-foreign-library 'async-process::async-process)
(cffi:close-foreign-library 'sdl2::libsdl2)
(cffi:close-foreign-library 'sdl2-ttf::libsdl2-ttf)
(cffi:close-foreign-library 'sdl2-image::libsdl2-image)

(defun setup-foreign-library-directories ()
  (setf cffi:*foreign-library-directories* '())
  (cffi:load-foreign-library (lem:lem-relative-pathname "lib/libasyncprocess.dll"))
  (cffi:load-foreign-library (lem:lem-relative-pathname "lib/SDL2.dll"))
  (cffi:load-foreign-library (lem:lem-relative-pathname "lib/SDL2_image.dll"))
  (cffi:load-foreign-library (lem:lem-relative-pathname "lib/SDL2_ttf.dll")))

(defun launch (&optional (args (uiop:command-line-arguments)))
  (setup-foreign-library-directories)
  (apply #'lem:lem args))

(lem:copy-file-or-directory (asdf:system-relative-pathname :lem-sdl2 "resources/")
                            "windows/resources/")

(apply #'sb-ext:save-lisp-and-die
       "windows/lem"
       :toplevel 'launch
       :executable t
       #+sb-core-compression
       '(:compression -1)
       #+(not sb-core-compression)
       '(:executable t))
