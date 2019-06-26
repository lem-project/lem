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

(let ((lem-python-mode-loading-p nil))
  (defmethod perform :after (operation
                             (system (eql (find-system "lem-python-mode"))))
    (when (and (not lem-python-mode-loading-p)
               (uiop:featurep :quicklisp)
               (uiop:symbol-call :quicklisp :where-is-system :async-process))
      (setf lem-python-mode-loading-p t)
      (operate operation (find-system "lem-python-mode/run"))
      (setf lem-python-mode-loading-p nil)))

  (defmethod perform :before (operation
                              (system (eql (find-system "lem-python-mode/run"))))
    (unless lem-python-mode-loading-p
      (operate operation (find-system "lem-python-mode")))))
