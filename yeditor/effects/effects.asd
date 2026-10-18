(defsystem "yeditor-effects"
  :description "Standard effects T4-T5: buffer, mode, hook, keymap, variable, command, overlay, timer"
  :depends-on ("lem" "yeditor-tx" "yeditor-context")
  :components ((:file "e-buffer")
               (:file "e-mode")
               (:file "e-hook")
               (:file "e-keymap")
               (:file "e-variable")
               (:file "e-command")
               (:file "e-overlay")
               (:file "e-timer")
               (:file "effects")))