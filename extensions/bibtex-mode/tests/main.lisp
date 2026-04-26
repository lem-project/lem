(defpackage :lem-bibtex-mode/tests
  (:use :cl :rove :lem :lem-bibtex-mode)
  (:import-from :lem-core
                :get-mode-object))
(in-package :lem-bibtex-mode/tests)

(defun make-bibtex-buffer (content)
  (let ((buffer (make-buffer "*bibtex-test*"
                             :temporary t
                             :enable-undo-p nil
                             :syntax-table lem-bibtex-mode::*bibtex-syntax-table*)))
    (insert-string (buffer-point buffer) content)
    (change-buffer-mode buffer 'lem-bibtex-mode:bibtex-mode)
    buffer))

(defmacro with-bibtex-buffer ((buffer-var content) &body body)
  `(with-current-buffers ()
     (let ((,buffer-var (make-bibtex-buffer ,content)))
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

(defun calc-indent-at-line (buffer line-number)
  (with-point ((p (get-point-at-line-char buffer line-number 0)))
    (funcall (variable-value 'calc-indent-function :buffer buffer)
             (copy-point p :temporary))))

(deftest test-bibtex-mode-registered
  (testing "bibtex-mode is registered"
    (ok (get-mode-object 'lem-bibtex-mode:bibtex-mode))))

(deftest test-file-type-association
  (testing ".bib files are associated with bibtex-mode"
    (ok (eq (get-file-mode "refs.bib")
            'lem-bibtex-mode:bibtex-mode))))

(deftest test-basic-highlighting
  (with-bibtex-buffer (buffer "@article{key,
  title = \"A\"
}")
    (syntax-scan-region (buffer-start-point buffer)
                        (buffer-end-point buffer))
    (testing "entry declaration is highlighted as keyword"
      (ok (eq (attribute-at buffer 1 0) 'syntax-keyword-attribute)))
    (testing "field name is highlighted as variable"
      (ok (eq (attribute-at buffer 2 2) 'syntax-variable-attribute)))
    (testing "string content is highlighted as string"
      (ok (eq (attribute-at buffer 2 10) 'syntax-string-attribute)))))

(deftest test-basic-indentation
  (with-bibtex-buffer (buffer "@article{key,
title = \"A\",
}")
    (testing "indentation follows brace structure"
      (ok (= 0 (calc-indent-at-line buffer 1)))
      (ok (= 2 (calc-indent-at-line buffer 2)))
      (ok (= 0 (calc-indent-at-line buffer 3))))))

(deftest test-comment-brace-ignored-for-indentation
  (with-bibtex-buffer (buffer "@article{key,
  title = \"A\", % }
year = 2024
}")
    (testing "brace inside inline comment does not affect following indentation"
      (ok (= 2 (calc-indent-at-line buffer 3)))
      (ok (= 0 (calc-indent-at-line buffer 4))))))

(deftest test-parenthesized-entry-indentation
  (with-bibtex-buffer (buffer "@article(key,
title = \"A\",
)")
    (testing "parenthesized entries indent like braced entries"
      (ok (= 2 (calc-indent-at-line buffer 2)))
      (ok (= 0 (calc-indent-at-line buffer 3))))))

(deftest test-multi-delimiter-depth-indentation
  (with-bibtex-buffer (buffer "@article{key,
  title = {{
next")
    (testing "multiple unmatched delimiters increase indentation by full depth"
      (ok (= 6 (calc-indent-at-line buffer 3))))))
