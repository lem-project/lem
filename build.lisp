#-sbcl
(asdf:make :lem/executable)
#+sbcl
(ql:quickload :lem-ncurses)
#+sbcl
(sb-ext:save-lisp-and-die "lem"
                          :toplevel 'lem:main
                          :executable t
                          :compression -1)
