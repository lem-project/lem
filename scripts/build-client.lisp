(ql:quickload :lem-webview)

(sb-ext:save-lisp-and-die "lem-client"
                          :toplevel #'lem-webview:webview-main
                          :executable t)
