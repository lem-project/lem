(defsystem "yeditor-context"
  :description "Component registry and lifecycle for Yeditor (L2)"
  :author "Yuri Albuquerque <yuri-albuquerque@users.noreply.github.com>"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("yeditor-tx")
  :components ((:file "registry")))
