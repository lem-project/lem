(defsystem "lem-elisp-mode"
  :depends-on ("lem"
               "lem-lisp-mode"
               #+#.(cl:if (asdf:find-system :async-process cl:nil) '(and) '(or)) "lem-process")
  :serial t
  :components ((:file "elisp-mode")
               (:file "run-elisp")))