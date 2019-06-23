(defsystem "lem-python-mode"
  :depends-on ("lem-core")
  :defsystem-depends-on ("uiop")
  :serial t
  :components ((:file "python-mode")))

;;; This is the recommended way to include optional dependencies.
;;; See: https://common-lisp.net/project/asdf/asdf/The-defsystem-grammar.html#index-_003aweakly_002ddepends_002don

(defsystem "lem-python-mode/run"
  :depends-on ("lem-pyton-mode" "lem-process")
  :serial t
  :components ((:file "run-python")))

(defmethod perform :after (operation
                           (system (eql (find-system "lem-python-mode"))))
  (when (and (uiop:featurep :quicklisp)
             (uiop:symbol-call :quicklisp :where-is-system :async-process))
    (operate operation (find-system "lem-python-mode/run"))))
