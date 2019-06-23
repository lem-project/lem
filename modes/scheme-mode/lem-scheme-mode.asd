(defsystem "lem-scheme-mode"
  :depends-on ("alexandria"
               "uiop"
               "lem-core")
  :defsystem-depends-on ("uiop")
  :serial t
  :components ((:file "syntax-data")
               (:file "syntax-indent")
               (:file "syntax-syntax-table")
               (:file "syntax-misc")
               (:file "lem-scheme-syntax")
               (:file "package")
               (:file "grammer")
               (:file "scheme-mode")))

;;; This is the recommended way to include optional dependencies.
;;; See: https://common-lisp.net/project/asdf/asdf/The-defsystem-grammar.html#index-_003aweakly_002ddepends_002don

(defsystem "lem-scheme-mode/repl"
  :depends-on ("lem-scheme-mode" "lem-process")
  :serial t
  :components ((:file "eval")
               (:file "repl")))

(defmethod perform :after (operation
                           (system (eql (find-system "lem-scheme-mode"))))
  (when (uiop:symbol-call :quicklisp :where-is-system :async-process)
    (operate operation (find-system "lem-scheme-mode/repl"))))
