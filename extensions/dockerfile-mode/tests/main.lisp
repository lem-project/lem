(defpackage :lem-dockerfile-mode/tests
  (:use :cl :rove :lem :lem-dockerfile-mode))
(in-package :lem-dockerfile-mode/tests)

(deftest test-mode-activates
  (testing "mode activates for Dockerfile and Containerfile"
           (ok (lem:find-mode "Dockerfile")
               "Mode should be registered as Dockerfile")))

(deftest test-mode-inherits-from-language-mode
  (testing "mode inherits from language-mode"
           (ok (subtypep 'dockerfile-mode 'lem/language-mode:language-mode)
               "Should inherit from language-mode")))

(deftest test-comment-syntax
  (testing "comment configuration"
           (let ((buffer (make-buffer "*test-dockerfile*" :enable-undo-p nil)))
             (change-buffer-mode buffer 'dockerfile-mode)
             (ok (equal "#" (variable-value 'line-comment :buffer buffer)))
             (ok (equal "# " (variable-value 'insertion-line-comment :buffer buffer))))))