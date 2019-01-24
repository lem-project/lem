(defsystem "lem-modeline-battery"
  :author "cxxxr"
  :license "MIT"
  :description "Show battery status on lem."
  :depends-on ("fukamachi//trivial-battery")
  :serial t
  :components ((:file "main")))
