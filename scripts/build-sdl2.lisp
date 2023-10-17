(ql:quickload :lem-sdl2)

(lem:init-at-build-time)

(sb-ext:save-lisp-and-die "lem"
                          :toplevel #'lem:lem
                          :executable t)
