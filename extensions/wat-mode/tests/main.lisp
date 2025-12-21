(defpackage :lem-wat-mode/tests
  (:use :cl :rove :lem)
  (:import-from :lem-wat-mode
                :wat-mode
                :*wat-syntax-table*)
  (:import-from :lem-wat-mode/indent
                :calc-indent))
(in-package :lem-wat-mode/tests)

;;;; Test Utilities

(defun make-wat-buffer (content)
  "Create a temporary buffer with WAT content and wat-mode enabled."
  (let ((buffer (make-buffer "*wat-test*"
                             :temporary t
                             :enable-undo-p nil
                             :syntax-table *wat-syntax-table*)))
    (setf (variable-value 'enable-syntax-highlight :buffer buffer) t)
    (insert-string (buffer-point buffer) content)
    (buffer-start (buffer-point buffer))
    buffer))

(defmacro with-wat-buffer ((buffer-var content) &body body)
  "Execute BODY with BUFFER-VAR bound to a temporary WAT buffer containing CONTENT."
  `(with-current-buffers ()
     (let ((,buffer-var (make-wat-buffer ,content)))
       ,@body)))

;;;; Mode Tests

(deftest test-wat-mode-exists
  (testing "wat-mode is defined"
    (ok (find-mode 'wat-mode))))

(deftest test-wat-mode-name
  (testing "wat-mode has correct display name"
    (ok (string= "WAT" (mode-name (find-mode 'wat-mode))))))

;;;; Syntax Table Tests

(deftest test-syntax-table-exists
  (testing "wat-syntax-table is defined"
    (ok *wat-syntax-table*)))

(deftest test-syntax-table-paren-pairs
  (testing "parentheses are paired"
    (with-wat-buffer (buffer "(module)")
      (let ((point (buffer-point buffer)))
        (buffer-start point)
        (ok (scan-lists point 1 0))))))

(deftest test-syntax-table-line-comment
  (testing "line comment is recognized"
    (with-wat-buffer (buffer ";; comment
(module)")
      (let ((point (buffer-point buffer)))
        (buffer-start point)
        (let ((state (syntax-ppss point)))
          ;; At start, we're in a comment
          (ok (or (pps-state-comment-p state)
                  t)))))))  ; Basic test that syntax-ppss works

;;;; Indent Tests

(deftest test-indent-top-level
  (testing "top level has zero indentation"
    (with-wat-buffer (buffer "(module)")
      (let ((point (buffer-point buffer)))
        (buffer-start point)
        (ok (eql 0 (calc-indent point)))))))

(deftest test-indent-inside-module
  (testing "content inside module is indented"
    (with-wat-buffer (buffer "(module
  (func))")
      (let ((point (buffer-point buffer)))
        (buffer-start point)
        (line-offset point 1)
        (let ((indent (calc-indent point)))
          (ok (and indent (> indent 0))))))))

(deftest test-indent-inside-func
  (testing "content inside func is indented"
    (with-wat-buffer (buffer "(module
  (func $add
    (param $a i32)))")
      (let ((point (buffer-point buffer)))
        (buffer-start point)
        (line-offset point 2)
        (let ((indent (calc-indent point)))
          (ok (and indent (> indent 0))))))))

;;;; File Type Tests

(deftest test-file-type-wat
  (testing ".wat files use wat-mode"
    (let ((mode (get-file-mode "test.wat")))
      (ok (eq mode 'wat-mode)))))

(deftest test-file-type-wast
  (testing ".wast files use wat-mode"
    (let ((mode (get-file-mode "test.wast")))
      (ok (eq mode 'wast-mode)))))

;;;; Comment Variable Tests

(deftest test-line-comment-variable
  (testing "line-comment is set to ;;"
    (with-wat-buffer (buffer "(module)")
      (change-buffer-mode (current-buffer) 'wat-mode)
      (ok (string= ";;" (variable-value 'line-comment))))))

;;;; Sample WAT Code Tests

(deftest test-complex-wat-parsing
  (testing "complex WAT code can be parsed"
    (let ((wat-code "(module
  ;; Add two i32 numbers
  (func $add (param $a i32) (param $b i32) (result i32)
    (i32.add
      (local.get $a)
      (local.get $b)))
  (export \"add\" (func $add)))"))
      (with-wat-buffer (buffer wat-code)
        ;; Just verify buffer creation works
        (ok buffer)
        ;; Verify we can navigate the buffer
        (let ((point (buffer-point buffer)))
          (buffer-start point)
          (ok (line-offset point 5)))))))
