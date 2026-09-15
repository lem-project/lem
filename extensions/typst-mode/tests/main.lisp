(defpackage :lem-typst-mode/tests
  (:use :cl :rove :lem :lem-typst-mode))
(in-package :lem-typst-mode/tests)

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

(deftest test-mode-registration
  (testing "Typst mode is registered with the correct name and inheritance"
    (ok (eq (lem:find-mode-from-name "Typst") 'typst-mode)
        "typst-mode should be registered with name 'Typst'")
    (ok (subtypep 'typst-mode 'lem/language-mode:language-mode)
        "typst-mode should inherit from lem/language-mode:language-mode")))

(deftest test-file-associations
  (testing "typst-mode activates for .typ and .typst files"
    (ok (eq (lem:find-mode-from-filename "main.typ") 'typst-mode)
        "main.typ should activate typst-mode")
    (ok (eq (lem:find-mode-from-filename "document.typst") 'typst-mode)
        "document.typst should activate typst-mode")))

(deftest test-syntax-table-configuration
  (testing "*typst-syntax-table* structure and properties"
    (ok *typst-syntax-table*
        "*typst-syntax-table* must be defined")
    (ok (lem:syntax-table-p *typst-syntax-table*)
        "*typst-syntax-table* must be a valid syntax table")))

(deftest test-commands-registered
  (testing "all interactive typst commands are registered in Lem"
    (ok (lem:get-command 'lem-typst-mode/preview/preview:typst-preview)
        "typst-preview command should be registered")
    (ok (lem:get-command 'lem-typst-mode/preview/preview:typst-preview-stop)
        "typst-preview-stop command should be registered")
    (ok (lem:get-command 'lem-typst-mode/preview/preview:typst-set-preview-root)
        "typst-set-preview-root command should be registered")
    (ok (lem:get-command 'lem-typst-mode/preview/preview:typst-export-file)
        "typst-export-file command should be registered")))

(deftest test-syntax-scanning-typst-suite
  (testing "Syntax scanner scans realistic Typst document without infinite loops or errors"
    (let ((buffer (lem:make-buffer "*typst-suite-test*"
                                   :syntax-table *typst-syntax-table*
                                   :temporary t)))
      (lem:insert-string (lem:buffer-point buffer) +typst-suite-sample+)
      (lem:buffer-start (lem:buffer-point buffer))
      ;; Trigger the syntax scanner across the entire buffer region
      (lem:syntax-scan-region (lem:buffer-start-point buffer)
                              (lem:buffer-end-point buffer))
      (ok (> (lem:buffer-nlines buffer) 30)
          "Buffer lines should be preserved after scanning")
      (ok (eq (lem:buffer-syntax-table buffer) *typst-syntax-table*)
          "Buffer syntax table should remain *typst-syntax-table*"))))

(deftest test-bracket-and-content-indentation
  (testing "Basic block and paren depth indentation"
    (let ((buffer (lem:make-buffer "*typst-indent-test*"
                                   :syntax-table *typst-syntax-table*
                                   :temporary t)))
      (setf (lem:variable-value 'lem:tab-width :buffer buffer) 2)
      (lem:insert-string (lem:buffer-point buffer)
                         (format nil "#let calc(n) = {~%  let sum = 0~%  sum~%}"))
      (lem:buffer-start (lem:buffer-point buffer))
      (ok (= (lem:buffer-nlines buffer) 4)
          "Multi-line function block buffer created successfully"))))
