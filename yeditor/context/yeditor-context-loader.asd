(defsystem "yeditor-context-loader"
  :description "T8 loader: .yeditor.lisp parsing, config reconciliation, HMR watcher"
  :author "Yuri Albuquerque <yuri-albuquerque@users.noreply.github.com>"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("yeditor-tx" "yeditor-context")
  :components ((:file "loader")))
