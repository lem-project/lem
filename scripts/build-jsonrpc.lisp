(ql:quickload :lem-jsonrpc)

(ql:quickload :lem/legit)

(setf lem-shell-mode:*default-shell-command* "/bin/bash")

(lem:init-at-build-time)

(sb-ext:save-lisp-and-die "lem-jsonrpc"
                          :toplevel #'lem-jsonrpc:program
                          :executable t)
