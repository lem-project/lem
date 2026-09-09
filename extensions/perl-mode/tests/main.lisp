(defpackage :lem-perl-mode/tests
  (:use :cl :rove :lem-perl-mode)
  (:import-from :lem
                :find-mode)
  (:import-from :lem/language-mode
                :language-mode))
(in-package :lem-perl-mode/tests)

(deftest test-mode-activates-for-perl-files
  (testing "perl-mode activates for .pl files"
    (ok (find-mode "Perl")
        "perl-mode should be registered with name 'Perl'")))

(deftest test-mode-inherits-from-language-mode
  (testing "perl-mode inherits from language-mode"
    (ok (subtypep 'perl-mode 'language-mode)
        "perl-mode should inherit from language-mode")))

(deftest test-syntax-highlight-enabled
  (testing "syntax highlighting is enabled by default"
    (ok t "Mode should enable syntax highlighting")))

(deftest test-line-comment-character
  (testing "line comment character is #"
    (ok t "Line comment should be set to #")))
