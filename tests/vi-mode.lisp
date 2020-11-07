(defpackage :lem-tests/vi-mode
  (:use :cl
        :lem
        :lem-tests/deftest))
(in-package :lem-tests/vi-mode)

(deftest vi-mode
  (ql:quickload :lem-fake-interface :silent t)
  (lem)
  (lem-vi-mode:vi-mode)
  (testing "enable hook"
    (testing "initialize-vi-modeline"
      (ok (and (lem-vi-mode.core::vi-modeline-element-p
                lem-vi-mode.core::*modeline-element*)))
      (ok (find-if (lambda (element)
                     (and (lem-vi-mode.core::vi-modeline-element-p element)
                          (equal "[COMMAND]" (lem-vi-mode.core::element-name element))))
                   lem::*modeline-status-list*)))
    (testing "(chagne-state 'command)"
      (ok (eq lem-vi-mode.core::*current-state* 'lem-vi-mode::command))
      (ok (eq (mode-keymap (get 'lem-vi-mode:vi-mode 'lem::global-mode))
              (lem-vi-mode.core::vi-state-keymap (lem-vi-mode.core::ensure-state 'lem-vi-mode::command))))
      (ok (equal "[COMMAND]" (lem-vi-mode.core::element-name lem-vi-mode.core::*modeline-element*)))
      (ok (string= (if (eq :dark (lem::display-background-mode))
                       "white"
                       "dark")
                   (attribute-background (ensure-attribute 'cursor nil)))))))
