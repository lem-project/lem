;;;; yeditor.asd — Yeditor: a spatiotemporally composable, agentic code editor.
;;;; A fork of Lem (https://github.com/lem-project/lem), MIT licensed.
(asdf:defsystem #:yeditor
  :description "Yeditor: a spatiotemporally composable, agentic code editor (Lem fork)"
  :author "Yuri Albuquerque <yuri-albuquerque@users.noreply.github.com>"
  :license "MIT"
  :version "0.1.0"
  :depends-on (:lem)
  :components ((:module "yeditor"
                :components ((:file "package")
                             (:file "core")))
               (:module "yeditor/tx"
                :components ((:file "tx")
                             (:file "context")))))