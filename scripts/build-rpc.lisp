(ql:quickload :lem-rpc)

(ql:quickload :lem/legit)

(setf lem-shell-mode:*default-shell-command* "/bin/bash")

(lem:init-at-build-time)

(sb-ext:save-lisp-and-die "lem-rpc"
                          :toplevel #'lem-rpc:program
                          :executable t)
