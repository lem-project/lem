(defsystem "lem-python-mode"
  :depends-on ("lem-core")
  :defsystem-depends-on ("uiop")
  :serial t
  :components ((:file "python-mode")))

;;; This is the recommended way to include optional dependencies.
;;; See: https://common-lisp.net/project/asdf/asdf/The-defsystem-grammar.html#index-_003aweakly_002ddepends_002don

(defsystem "lem-python-mode/run"
  :depends-on ("lem-process")
  :serial t
  :components ((:file "run-python")))

(let ((done nil))
  (defmethod perform :after ((operation load-op)
                             (system (eql (find-system "lem-python-mode"))))
    (when (and (not done)
               (uiop:featurep :quicklisp)
               (uiop:symbol-call :quicklisp :where-is-system :async-process))
      (setf done t)
      (load-system "lem-python-mode/repl")))

  (defmethod perform :before ((operation prepare-op)
                              (system (eql (find-system "lem-python-mode/run"))))
    (unless done
      (setf done t)
      (load-system "lem-python-mode"))))
