(ql:quickload :lem-jsonrpc)

(lem:init-at-build-time)

(sb-ext:save-lisp-and-die "lem-jsonrpc"
                          :toplevel #'lem-jsonrpc:program
                          :executable t)
