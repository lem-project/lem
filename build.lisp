#-sbcl
(asdf:make :lem/executable)
#+sbcl
(ql:quickload :lem-ncurses)
#+(and sbcl sb-core-compression)
(sb-ext:save-lisp-and-die "lem"
                          :toplevel 'lem:main
                          :executable t
                          :compression -1)

#+(and sbcl (not sb-core-compression))
(sb-ext:save-lisp-and-die "lem"
                          :toplevel 'lem:main
                          :executable t)
