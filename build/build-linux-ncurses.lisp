(ql:quickload :lem-ncurses)

(setq lem::*deployed* t)

(cffi:close-foreign-library 'async-process::async-process)

(defun setup-foreign-library-directories ()
  (setf cffi:*foreign-library-directories* '())
  (cffi:load-foreign-library (lem:lem-relative-pathname "lib/libasyncprocess.so")))

(defun launch (&optional (args (uiop:command-line-arguments)))
  (setup-foreign-library-directories)
  (apply #'lem:lem args))

(lem-lisp-mode/swank-modules:swank-modules)

(apply #'sb-ext:save-lisp-and-die
       "linux/lem-ncurses"
       :toplevel 'launch
       :executable t
       #+sb-core-compression
       '(:compression -1)
       #+(not sb-core-compression)
       '(:executable t))
