(defpackage :lem-latex-mode/tests
  (:use :cl :rove :lem :lem-latex-mode)
  (:import-from :lem-core
                :get-mode-object))
(in-package :lem-latex-mode/tests)

(defun make-latex-buffer (content)
  (let ((buffer (make-buffer "*latex-test*"
                             :temporary t
                             :enable-undo-p nil
                             :syntax-table lem-latex-mode::*latex-syntax-table*)))
    (insert-string (buffer-point buffer) content)
    (change-buffer-mode buffer 'lem-latex-mode:latex-mode)
    buffer))

(defmacro with-latex-buffer ((buffer-var content) &body body)
  `(with-current-buffers ()
     (let ((,buffer-var (make-latex-buffer ,content)))
       (with-current-buffer ,buffer-var
         ,@body))))

(defun get-point-at-line-char (buffer line-number charpos)
  (with-point ((p (buffer-start-point buffer)))
    (line-offset p (1- line-number))
    (character-offset p charpos)
    (copy-point p :temporary)))

(defun attribute-at (buffer line-number charpos)
  (with-point ((p (get-point-at-line-char buffer line-number charpos)))
    (text-property-at p :attribute)))

(defun attribute-is (attribute symbol)
  (eq attribute symbol))

(defun syntax-scan-buffer (buffer)
  (syntax-scan-region (buffer-start-point buffer)
                      (buffer-end-point buffer)))

(defun calc-indent-at-line (buffer line-number)
  (with-point ((p (get-point-at-line-char buffer line-number 0)))
    (funcall (variable-value 'calc-indent-function :buffer buffer)
             (copy-point p :temporary))))

(deftest test-latex-mode-registered
  (testing "latex-mode is registered as a major mode"
    (ok (get-mode-object 'lem-latex-mode:latex-mode))))

(deftest test-file-type-association
  (testing ".tex files are associated with latex-mode"
    (ok (eq (get-file-mode "sample.tex")
            'lem-latex-mode:latex-mode))))

(deftest test-comment-and-command-highlighting
  (with-latex-buffer (buffer "% comment
\\section{Intro}")
    (syntax-scan-buffer buffer)
    (testing "line comments use syntax-comment-attribute"
      (ok (attribute-is (attribute-at buffer 1 0) 'syntax-comment-attribute)))
    (testing "LaTeX commands use syntax-keyword-attribute"
      (ok (attribute-is (attribute-at buffer 2 0) 'syntax-keyword-attribute)))))

(deftest test-command-keywords-require-backslash
  (with-latex-buffer (buffer "section item label
\\section{Title}")
    (syntax-scan-buffer buffer)
    (testing "plain prose words are not treated as LaTeX commands"
      (ok (not (attribute-is (attribute-at buffer 1 0) 'syntax-keyword-attribute))))
    (testing "actual backslash command is still highlighted"
      (ok (attribute-is (attribute-at buffer 2 0) 'syntax-keyword-attribute)))))

(deftest test-math-internal-highlighting
  (with-latex-buffer (buffer "$\\alpha + 12$")
    (syntax-scan-buffer buffer)
    (testing "math command token is highlighted as keyword"
      (ok (attribute-is (attribute-at buffer 1 1) 'syntax-keyword-attribute)))
    (testing "math operator is highlighted as builtin"
      (ok (attribute-is (attribute-at buffer 1 8) 'syntax-builtin-attribute)))
    (testing "math number is highlighted as constant"
      (ok (attribute-is (attribute-at buffer 1 10) 'syntax-constant-attribute)))))

(deftest test-verbatim-like-block-highlighting
  (with-latex-buffer (buffer "\\begin{verbatim}
\\alpha + 12
\\end{verbatim}
\\section{After}")
    (syntax-scan-buffer buffer)
    (testing "inside verbatim block is treated as string-like region"
      (ok (attribute-is (attribute-at buffer 2 0) 'syntax-string-attribute))
      (ok (not (attribute-is (attribute-at buffer 2 0) 'syntax-keyword-attribute))))
    (testing "highlighting resumes after verbatim block"
      (ok (attribute-is (attribute-at buffer 4 0) 'syntax-keyword-attribute)))))

(deftest test-minted-block-highlighting
  (with-latex-buffer (buffer "\\begin{minted}[linenos]{python}
print(1)
\\end{minted}")
    (syntax-scan-buffer buffer)
    (testing "minted body is highlighted as raw block"
      (ok (attribute-is (attribute-at buffer 2 0) 'syntax-string-attribute)))))

(deftest test-environment-indentation
  (with-latex-buffer (buffer "\\begin{itemize}
\\item first
\\end{itemize}")
    (testing "indent follows begin/end environment structure"
      (ok (= 0 (calc-indent-at-line buffer 1)))
      (ok (= 2 (calc-indent-at-line buffer 2)))
      (ok (= 0 (calc-indent-at-line buffer 3))))))

(deftest test-comment-lines-ignored-for-indentation
  (with-latex-buffer (buffer "\\begin{itemize}
% comment line
\\item first")
    (testing "comment-only lines do not affect next-line indentation"
      (ok (= 2 (calc-indent-at-line buffer 3))))))

(deftest test-math-block-indentation
  (with-latex-buffer (buffer "\\[
a+b
\\]
c")
    (testing "display-math delimiters affect indentation"
      (ok (= 0 (calc-indent-at-line buffer 1)))
      (ok (= 2 (calc-indent-at-line buffer 2)))
      (ok (= 0 (calc-indent-at-line buffer 3)))
      (ok (= 0 (calc-indent-at-line buffer 4))))))

(deftest test-linebreak-command-does-not-open-display-math-indent
  (with-latex-buffer (buffer "\\begin{itemize}
  \\\\[1ex]
text")
    (testing "\\\\[len] line-break command does not add extra indent"
      (ok (= 2 (calc-indent-at-line buffer 2)))
      (ok (= 2 (calc-indent-at-line buffer 3))))))

(deftest test-single-line-display-math-does-not-indent-next-line
  (with-latex-buffer (buffer "\\[ a+b \\]
next")
    (testing "single-line \\[ ... \\] does not increase next-line indentation"
      (ok (= 0 (calc-indent-at-line buffer 2))))))

(deftest test-single-line-inline-math-does-not-indent-next-line
  (with-latex-buffer (buffer "\\( a+b \\)
next")
    (testing "single-line \\( ... \\) does not increase next-line indentation"
      (ok (= 0 (calc-indent-at-line buffer 2))))))

(deftest test-single-line-environment-does-not-indent-next-line
  (with-latex-buffer (buffer "\\begin{itemize}\\item x\\end{itemize}
next")
    (testing "single-line begin/end environment does not increase next-line indentation"
      (ok (= 0 (calc-indent-at-line buffer 2))))))

(deftest test-brace-balance-indentation
  (with-latex-buffer (buffer "{
  foo
}
bar")
    (testing "positive/negative brace balance propagates indentation"
      (ok (= 2 (calc-indent-at-line buffer 2)))
      (ok (= 2 (calc-indent-at-line buffer 3)))
      (ok (= 0 (calc-indent-at-line buffer 4))))))

(deftest test-verbatim-indentation-preserves-body
  (with-latex-buffer (buffer "\\begin{verbatim}
    keep
  keep2
\\end{verbatim}
next")
    (testing "verbatim body keeps existing indentation, end aligns with begin"
      (ok (= 4 (calc-indent-at-line buffer 2)))
      (ok (= 2 (calc-indent-at-line buffer 3)))
      (ok (= 0 (calc-indent-at-line buffer 4)))
      (ok (= 0 (calc-indent-at-line buffer 5))))))
