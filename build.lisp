(ql:quickload :lem-ncurses)

(sb-ext:save-lisp-and-die (format nil "lem-ncurses-~A-~A" (roswell.util:uname) (roswell.util:uname-m))
                          :toplevel #'lem:main
                          :executable t
                          :compression t)
