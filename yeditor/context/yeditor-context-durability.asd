(defsystem "yeditor-context-durability"
  :description "T9 durability: JSONL effects log, boot replay and crash recovery"
  :author "Yuri Albuquerque <yuri-albuquerque@users.noreply.github.com>"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("yeditor-tx" "yeditor-context" "yason")
  :components ((:file "durability")))
