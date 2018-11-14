(ql:quickload :lem-ncurses)

(sb-ext:save-lisp-and-die (format nil "lem-ncurses-~A-~A" (roswell.util:uname) (roswell.util:uname-m))
                          :toplevel #'lem:lem
                          :executable t
                          :compression t)
