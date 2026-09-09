(defpackage #:lem-living-canvas/tests/language-detection
  (:use #:cl #:rove))
(in-package #:lem-living-canvas/tests/language-detection)

;;; T019-T023: Language Detection Tests
;;;
;;; These tests verify the language detection module's ability to identify
;;; programming languages from buffers and file paths.

;;; T020: Test detect-language with Python

(deftest detect-language-python-by-extension
  (testing "detect-language returns :python for .py files"
    (ok (eq :python (lem-living-canvas/language:language-for-file #P"test.py")))
    (ok (eq :python (lem-living-canvas/language:language-for-file #P"/path/to/module.py")))
    (ok (eq :python (lem-living-canvas/language:language-for-file #P"my_script.pyw")))))

(deftest detect-language-python-pathname-type
  (testing "language-for-file handles pathname-type correctly"
    (let ((path (make-pathname :name "script" :type "py")))
      (ok (eq :python (lem-living-canvas/language:language-for-file path))))))

;;; T021: Test detect-language with JavaScript

(deftest detect-language-javascript-by-extension
  (testing "detect-language returns :javascript for .js files"
    (ok (eq :javascript (lem-living-canvas/language:language-for-file #P"app.js")))
    (ok (eq :javascript (lem-living-canvas/language:language-for-file #P"index.mjs")))
    (ok (eq :javascript (lem-living-canvas/language:language-for-file #P"component.jsx")))))

;;; T022: Test detect-language with Common Lisp

(deftest detect-language-common-lisp-by-extension
  (testing "detect-language returns :common-lisp for .lisp files"
    (ok (eq :common-lisp (lem-living-canvas/language:language-for-file #P"package.lisp")))
    (ok (eq :common-lisp (lem-living-canvas/language:language-for-file #P"system.asd")))
    (ok (eq :common-lisp (lem-living-canvas/language:language-for-file #P"test.cl")))))

;;; T023: Test unsupported language error message

(deftest detect-language-unknown
  (testing "language-for-file returns nil for unknown extensions"
    (ok (null (lem-living-canvas/language:language-for-file #P"data.json")))
    (ok (null (lem-living-canvas/language:language-for-file #P"readme.md")))
    (ok (null (lem-living-canvas/language:language-for-file #P"Makefile")))))

(deftest detect-language-no-extension
  (testing "language-for-file handles files without extension"
    (ok (null (lem-living-canvas/language:language-for-file #P"Dockerfile")))
    (ok (null (lem-living-canvas/language:language-for-file #P".gitignore")))))

;;; TypeScript detection tests

(deftest detect-language-typescript-by-extension
  (testing "detect-language returns :typescript for .ts/.tsx files"
    (ok (eq :typescript (lem-living-canvas/language:language-for-file #P"app.ts")))
    (ok (eq :typescript (lem-living-canvas/language:language-for-file #P"component.tsx")))))

;;; detect-language unified function tests

(deftest detect-language-with-pathname
  (testing "detect-language accepts pathname and returns language"
    (ok (eq :python (lem-living-canvas/language:detect-language #P"script.py")))
    (ok (eq :javascript (lem-living-canvas/language:detect-language #P"app.js")))
    (ok (eq :common-lisp (lem-living-canvas/language:detect-language #P"test.lisp")))))

(deftest detect-language-returns-nil-for-unknown
  (testing "detect-language returns nil for unknown sources"
    (ok (null (lem-living-canvas/language:detect-language #P"data.xml")))
    (ok (null (lem-living-canvas/language:detect-language "just a string")))))

;;; Extension map completeness tests

(deftest extension-map-covers-common-extensions
  (testing "extension map covers common Python extensions"
    (ok (eq :python (lem-living-canvas/language:language-for-file #P"x.py")))
    (ok (eq :python (lem-living-canvas/language:language-for-file #P"x.pyw"))))

  (testing "extension map covers common JavaScript extensions"
    (ok (eq :javascript (lem-living-canvas/language:language-for-file #P"x.js")))
    (ok (eq :javascript (lem-living-canvas/language:language-for-file #P"x.mjs")))
    (ok (eq :javascript (lem-living-canvas/language:language-for-file #P"x.jsx"))))

  (testing "extension map covers TypeScript extensions"
    (ok (eq :typescript (lem-living-canvas/language:language-for-file #P"x.ts")))
    (ok (eq :typescript (lem-living-canvas/language:language-for-file #P"x.tsx"))))

  (testing "extension map covers Common Lisp extensions"
    (ok (eq :common-lisp (lem-living-canvas/language:language-for-file #P"x.lisp")))
    (ok (eq :common-lisp (lem-living-canvas/language:language-for-file #P"x.cl")))
    (ok (eq :common-lisp (lem-living-canvas/language:language-for-file #P"x.asd")))))

;;; Case insensitivity tests

(deftest extension-case-insensitive
  (testing "extension matching is case insensitive"
    (ok (eq :python (lem-living-canvas/language:language-for-file #P"Script.PY")))
    (ok (eq :javascript (lem-living-canvas/language:language-for-file #P"App.JS")))
    (ok (eq :common-lisp (lem-living-canvas/language:language-for-file #P"Test.LISP")))))
