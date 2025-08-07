(ql:quickload :lem-webview)

(lem:init-at-build-time)

(sb-ext:save-lisp-and-die "lem"
                          :toplevel #'lem-webview:lem-main
                          :executable t)
