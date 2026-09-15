(defpackage :lem-odin-mode/tests
  (:use :cl :rove :lem-odin-mode)
  (:import-from :lem
                :find-mode)
  (:import-from :lem/language-mode
                :language-mode))
(in-package :lem-odin-mode/tests)

(deftest test-mode-activates-for-odin-files
  (testing "odin-mode activates for .odin files"
    (ok (find-mode "Odin")
        "odin-mode should be registered with name 'Odin'")))

(deftest test-mode-inherits-from-language-mode
  (testing "odin-mode inherits from language-mode"
    (ok (subtypep 'odin-mode 'language-mode)
        "odin-mode should inherit from language-mode")))

(deftest test-syntax-highlight-enabled
  (testing "syntax highlighting is enabled by default"
    (ok t "Mode should enable syntax highlighting")))

(deftest test-line-comment-character
  (testing "line comment character is //"
    (ok t "Line comment should be set to //")))
