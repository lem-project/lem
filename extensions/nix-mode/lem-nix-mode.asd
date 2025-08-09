(defsystem "lem-nix-mode"
  :depends-on ("lem/core")
  :serial t
  :components ((:file "indent")
               (:file "nix-mode")))
