(in-package #:asdf-user)

(defsystem "lem-emacs-help-mode"
  :description "Emacs Style Help Keybindings for Lem"
  :author "Robert Wess Burnett"
  :license "MIT"
  :depends-on ("lem/core")
  :serial t
  :components ((:file "main")))
  
