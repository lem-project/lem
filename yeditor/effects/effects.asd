(defsystem "yeditor-effects"
  :description "Standard effects: buffer, mode, hook using real Lem APIs"
  :depends-on ("lem" "yeditor-tx" "yeditor-context")
  :components ((:file "e-buffer")
               (:file "e-mode")
               (:file "e-hook")
               (:file "effects")))