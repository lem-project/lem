(ql:quickload :lem-webview)

(lem:init-at-build-time)

(sb-ext:save-lisp-and-die "lem"
                          :toplevel #'lem-webview:main
                          :executable t)
