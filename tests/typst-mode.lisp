(defpackage :lem-tests/typst-mode
  (:use :cl :rove :lem)
  (:import-from :lem-typst-mode
                :typst-mode
                :*typst-syntax-table*)
  (:import-from :lem-core
                :get-mode-object
                :get-file-mode))
(in-package :lem-tests/typst-mode)

;;;; Test Utilities

(defun make-typst-buffer (content)
  "Create a temporary buffer with Typst content and typst-mode enabled."
  (let ((buffer (make-buffer "*typst-test*"
                             :temporary t
                             :enable-undo-p nil
                             :syntax-table *typst-syntax-table*)))
    (setf (variable-value 'enable-syntax-highlight :buffer buffer) t)
    (insert-string (buffer-point buffer) content)
    (buffer-start (buffer-point buffer))
    buffer))

(defmacro with-typst-buffer ((buffer-var content) &body body)
  "Execute BODY with BUFFER-VAR bound to a temporary Typst buffer containing CONTENT."
  `(with-current-buffers ()
     (let ((,buffer-var (make-typst-buffer ,content)))
       ,@body)))

;;;; Mode Definition Tests

(deftest test-typst-mode-exists
  (testing "typst-mode is defined"
    (ok (get-mode-object 'lem-typst-mode:typst-mode))))

(deftest test-typst-mode-name
  (testing "typst-mode has correct display name"
    (ok (string= "Typst" (mode-name (get-mode-object 'lem-typst-mode:typst-mode))))))

(deftest test-typst-mode-inheritance
  (testing "typst-mode inherits from language-mode"
    (ok (subtypep 'typst-mode 'lem/language-mode:language-mode))))

;;;; File Type Associations

(deftest test-file-type-typ
  (testing ".typ files use typst-mode"
    (ok (eq (get-file-mode "document.typ") 'lem-typst-mode:typst-mode))))

(deftest test-file-type-typst
  (testing ".typst files use typst-mode"
    (ok (eq (get-file-mode "document.typst") 'lem-typst-mode:typst-mode))))

;;;; Syntax Table Tests

(deftest test-syntax-table-exists
  (testing "typst-syntax-table is defined"
    (ok *typst-syntax-table*)))

(deftest test-syntax-table-paren-pairs
  (testing "parentheses, brackets, and braces are paired in syntax table"
    (with-typst-buffer (buffer "{ [ ( ) ] }")
      (let ((point (buffer-point buffer)))
        (buffer-start point)
        (ok (scan-lists point 1 0))))))

;;;; Typst Syntax Scanning Tests (with samples inspired by Typst test suite)

(defparameter +typst-suite-sample+
  "// Typst Test Suite Sample
#set page(paper: \"a4\", margin: (x: 2cm, y: 2.5cm))
#set text(font: \"Linux Libertine\", size: 11pt, lang: \"en\")

= Introduction to Typst <intro>

Typst is a *modern* markup-based typesetting system that is designed to be
as powerful as LaTeX while being much easier to learn and use.

== Mathematical Typesetting

In mathematics, the Euler identity is famous:
$ e^(i pi) + 1 = 0 $

Inline equations like $x + y = z$ and fractions $frac(a, b)$ work seamlessly.

== Code Blocks and Syntax

You can write raw code blocks easily:
```typst
#let greet(name) = [Hello, #name!]
#greet(\"World\")
```

Or inline `let x = 10pt`.

== Functions, Constants, and Conditionals

#let author-name = \"Antigravity\"
#let is-draft = false
#let items-count = 42

#if is-draft [
  #text(fill: red)[*DRAFT VERSION*]
] else [
  #align(center)[
    #block(stroke: 1pt + luma(150), inset: 10pt)[
      Published document with #items-count items.
    ]
  ]
]

See reference @intro for details.
/* End of sample file */
")

(deftest test-syntax-scanning-typst-suite
  (testing "Syntax scanner scans realistic Typst document without infinite loops or errors"
    (with-typst-buffer (buffer +typst-suite-sample+)
      ;; Trigger syntax scanner across the buffer
      (syntax-scan-region (buffer-start-point buffer)
                          (buffer-end-point buffer))
      (ok (> (buffer-nlines buffer) 30)
          "Buffer lines should be preserved after scanning")
      (ok (eq (buffer-syntax-table buffer) *typst-syntax-table*)
          "Buffer syntax table should remain *typst-syntax-table*"))))

;;;; Preview and Utility Commands

(deftest test-commands-existence
  (testing "Preview and export commands are registered in Lem"
    (ok (lem:get-command 'lem-typst-mode/preview/preview:typst-preview))
    (ok (lem:get-command 'lem-typst-mode/preview/preview:typst-preview-stop))
    (ok (lem:get-command 'lem-typst-mode/preview/preview:typst-set-preview-root))
    (ok (lem:get-command 'lem-typst-mode/preview/preview:typst-export-file))))
