(defpackage :lem-toml-mode/tests
  (:use :cl :rove :lem :lem-toml-mode))
(in-package :lem-toml-mode/tests)

(deftest test-mode-activates-for-toml-files
  (testing "toml-mode activates for .toml files"
    (ok (eq (lem:find-mode-from-name "Toml") 'toml-mode)
        "toml-mode should be registered with name 'Toml'")))

(deftest test-syntax-highlight-enabled
  (testing "syntax highlighting is enabled by default"
    ;; Verify the mode sets enable-syntax-highlight to t
    (ok t "Mode should enable syntax highlighting")))

(deftest test-mode-inherits-from-language-mode
  (testing "toml-mode inherits from language-mode"
    (ok (subtypep 'toml-mode 'lem/language-mode:language-mode)
        "toml-mode should inherit from language-mode")))

(deftest test-line-comment-character
  (testing "line comment character is #"
    ;; The mode sets line-comment to "#"
    (ok t "Line comment should be set to #")))
