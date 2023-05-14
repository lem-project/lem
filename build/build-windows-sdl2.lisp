(ql:quickload :lem-sdl2)

(setq lem::*deployed* t)

(cffi:close-foreign-library 'async-process::async-process)
(cffi:close-foreign-library 'cffi::libffi)
(cffi:close-foreign-library 'cl+ssl/config::libcrypto)
(cffi:close-foreign-library 'cl+ssl/config::libssl)
(cffi:close-foreign-library 'sdl2::libsdl2)
(cffi:close-foreign-library 'sdl2-image::libsdl2-image)
(cffi:close-foreign-library 'sdl2-ttf::libsdl2-ttf)

(defun setup-foreign-library-directories ()
  (setf cffi:*foreign-library-directories*
        (list (lem:lem-relative-pathname "lib/")))

  (cffi:load-foreign-library (lem:lem-relative-pathname "lib/libasyncprocess.dll"))
  (cffi:load-foreign-library (lem:lem-relative-pathname "lib/libffi-8.dll"))
  (cffi:load-foreign-library (lem:lem-relative-pathname "lib/libcrypto-1_1-x64.dll"))
  (cffi:load-foreign-library (lem:lem-relative-pathname "lib/libssl-1_1-x64.dll"))
  (cffi:load-foreign-library (lem:lem-relative-pathname "lib/SDL2.dll"))
  (cffi:load-foreign-library (lem:lem-relative-pathname "lib/SDL2_image.dll"))
  (cffi:load-foreign-library (lem:lem-relative-pathname "lib/SDL2_ttf.dll"))
  )

(defun launch (&optional (args (uiop:command-line-arguments)))
  (setup-foreign-library-directories)
  (apply #'lem:lem args))

(lem:copy-file-or-directory (asdf:system-relative-pathname :lem-sdl2 "resources/")
                            "windows/resources/")

(lem-lisp-mode/swank-modules:swank-modules)

(apply #'sb-ext:save-lisp-and-die
       "windows/lem-internal.exe"
       :toplevel 'launch
       :executable t
       :application-type :GUI
       #+sb-core-compression
       '(:compression -1)
       #+(not sb-core-compression)
       '(:executable t))
