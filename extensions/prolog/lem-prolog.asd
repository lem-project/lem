(defsystem "lem-prolog"
  :author ("jgart" "Markus Triska")
  :license "GPL-3.0-or-later"
  :description "Prolog support for Lem: a Prolog major mode and interaction with a Prolog process."
  :depends-on ("lem/core" "lem-process")
  :serial t
  :components ((:file "prolog-mode")
               (:file "run-prolog")))
