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
  :depends-on ("lem-process")
  :serial t
  :components ((:file "eval")
               (:file "repl")))

(let ((done nil))
  (defmethod perform :after ((operation load-op)
                             (system (eql (find-system "lem-scheme-mode"))))
    (when (and (not done)
               (uiop:featurep :quicklisp)
               (uiop:symbol-call :quicklisp :where-is-system :async-process))
      (setf done t)
      (load-system "lem-scheme-mode/repl")))

  (defmethod perform :before ((operation prepare-op)
                              (system (eql (find-system "lem-scheme-mode/repl"))))
    (unless done
      (setf done t)
      (load-system "lem-scheme-mode"))))
