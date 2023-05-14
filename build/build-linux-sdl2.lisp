(ql:quickload :lem-sdl2)

(setq lem::*deployed* t)

(cffi:close-foreign-library 'async-process::async-process)
(cffi:close-foreign-library 'sdl2::libsdl2)
(cffi:close-foreign-library 'sdl2-ttf::libsdl2-ttf)
(cffi:close-foreign-library 'sdl2-image::libsdl2-image)

(defun setup-foreign-library-directories ()
  (setf cffi:*foreign-library-directories* '())
  (cffi:load-foreign-library (lem:lem-relative-pathname "lib/libasyncprocess.so"))
  (cffi:load-foreign-library (lem:lem-relative-pathname "lib/libdecor-0.so.0"))
  (cffi:load-foreign-library (lem:lem-relative-pathname "lib/libLerc.so.4"))
  (cffi:load-foreign-library (lem:lem-relative-pathname "lib/libtiff.so.6"))
  (cffi:load-foreign-library (lem:lem-relative-pathname "lib/libsamplerate.so.0"))
  (cffi:load-foreign-library (lem:lem-relative-pathname "lib/libSDL2-2.0.so.0"))
  (cffi:load-foreign-library (lem:lem-relative-pathname "lib/libSDL2_image-2.0.so.0"))
  (cffi:load-foreign-library (lem:lem-relative-pathname "lib/libSDL2_ttf-2.0.so.0")))

(defun launch (&optional (args (uiop:command-line-arguments)))
  (setup-foreign-library-directories)
  (apply #'lem:lem args))

(lem:copy-file-or-directory (asdf:system-relative-pathname :lem-sdl2 "resources/")
                            "linux/resources/")

(lem-lisp-mode/swank-modules:require-swank-modules)

(apply #'sb-ext:save-lisp-and-die
       "linux/lem"
       :toplevel 'launch
       :executable t
       #+sb-core-compression
       '(:compression -1)
       #+(not sb-core-compression)
       '(:executable t))
