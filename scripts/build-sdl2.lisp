(ql:quickload :lem-sdl2)

(lem:init-at-build-time)

(ql:quickload :lem/co-editing/client)

(sb-ext:save-lisp-and-die "lem"
                          :toplevel #'lem:main
                          :executable t)
