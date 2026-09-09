(ql:quickload :lem-ncurses)

(lem:init-at-build-time)


(setf lem:*splash-function* #'lem/legit::legit-status)

(sb-ext:save-lisp-and-die "legit"
                          :toplevel #'lem:main
                          :executable t)
