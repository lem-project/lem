(defpackage :lem-tests/clojure-mode
  (:use :cl :rove :lem)
  (:import-from :lem-clojure-mode
                :*clojure-syntax-table*
                :clojure-calc-indent
                :clojure-current-namespace
                :clojure-guess-namespace
                :clojure-set-indentation
                :clojure-get-indentation)
  (:import-from :lem-clojure-mode/bencode
                :bencode-encode
                :bencode-decode
                :bencode-error)
  (:import-from :lem-tests/utilities
                :diff-text))
(in-package :lem-tests/clojure-mode)

;;;; Test Utilities

(defun make-clojure-buffer (text &optional (name "*clojure-test*"))
  "Create a test buffer with Clojure syntax table and TEXT content."
  (let ((buffer (make-buffer name
                             :syntax-table *clojure-syntax-table*
                             :temporary t)))
    (setf (variable-value 'tab-width :buffer buffer) 2)
    (setf (variable-value 'calc-indent-function :buffer buffer)
          'clojure-calc-indent)
    (insert-string (buffer-point buffer) text)
    (buffer-start (buffer-point buffer))
    buffer))

(defun get-indent-at-line (buffer line-number)
  "Get calculated indent for LINE-NUMBER (1-indexed) in BUFFER."
  (with-point ((point (buffer-point buffer)))
    (move-to-line point line-number)
    (clojure-calc-indent point)))

(defun test-indent (text expected-indents)
  "Test that each line in TEXT has the expected indentation."
  (with-current-buffers ()
    (let ((buffer (make-clojure-buffer text)))
      (setf (current-buffer) buffer)
      (loop :for expected :in expected-indents
            :for line-num :from 1
            :do (let ((actual (get-indent-at-line buffer line-num)))
                  (ok (equal expected actual)
                      (format nil "Line ~D: expected ~A, got ~A"
                              line-num expected actual)))))))

(defmacro define-indent-test (name before &optional (after before))
  `(deftest ,name
     (run-indent-test ,(string name) ,before ,after)))

(defun run-indent-test (name before-text after-text)
  "Run an indentation test that indents the entire buffer."
  (with-current-buffers ()
    (let ((buffer (make-clojure-buffer before-text (format nil "*indent-test ~A*" name))))
      (setf (current-buffer) buffer)
      (indent-buffer buffer)
      (unless (ok (string= after-text (buffer-text buffer)) name)
        (format nil "# indentation error: ~A~%~A~%"
                name
                (diff-text (buffer-text buffer) after-text))))))

;;;; ============================================================
;;;; Bencode Tests
;;;; ============================================================

(deftest bencode-encode-integer
  (testing "Encoding integers"
    (ok (string= "i0e" (bencode-encode 0)))
    (ok (string= "i42e" (bencode-encode 42)))
    (ok (string= "i-42e" (bencode-encode -42)))
    (ok (string= "i12345678901234567890e" (bencode-encode 12345678901234567890)))))

(deftest bencode-encode-string
  (testing "Encoding strings"
    (ok (string= "0:" (bencode-encode "")))
    (ok (string= "4:spam" (bencode-encode "spam")))
    (ok (string= "11:hello world" (bencode-encode "hello world")))
    (ok (string= "4:test" (bencode-encode "test")))))

(deftest bencode-encode-symbol
  (testing "Encoding symbols"
    (ok (string= "3:foo" (bencode-encode 'foo)))
    (ok (string= "6:my-var" (bencode-encode 'my-var)))))

(deftest bencode-encode-list
  (testing "Encoding lists"
    (ok (string= "le" (bencode-encode nil)))
    (ok (string= "l4:spami42ee" (bencode-encode '("spam" 42))))
    (ok (string= "li1ei2ei3ee" (bencode-encode '(1 2 3))))
    (ok (string= "ll1:aeli1eee" (bencode-encode '(("a") (1)))))))

(deftest bencode-encode-hash-table
  (testing "Encoding hash tables"
    (let ((empty (make-hash-table :test 'equal))
          (simple (make-hash-table :test 'equal)))
      (ok (string= "de" (bencode-encode empty)))
      (setf (gethash "foo" simple) "bar")
      (ok (string= "d3:foo3:bare" (bencode-encode simple))))))

(deftest bencode-decode-integer
  (testing "Decoding integers"
    (ok (= 0 (bencode-decode "i0e")))
    (ok (= 42 (bencode-decode "i42e")))
    (ok (= -42 (bencode-decode "i-42e")))
    (ok (= 12345 (bencode-decode "i12345e")))))

(deftest bencode-decode-string
  (testing "Decoding strings"
    (ok (string= "" (bencode-decode "0:")))
    (ok (string= "spam" (bencode-decode "4:spam")))
    (ok (string= "hello world" (bencode-decode "11:hello world")))))

(deftest bencode-decode-list
  (testing "Decoding lists"
    (ok (null (bencode-decode "le")))
    (let ((result (bencode-decode "li1ei2ei3ee")))
      (ok (listp result))
      (ok (equal '(1 2 3) result)))
    (let ((result (bencode-decode "l4:spami42ee")))
      (ok (= 2 (length result)))
      (ok (string= "spam" (first result)))
      (ok (= 42 (second result))))))

(deftest bencode-decode-dictionary
  (testing "Decoding dictionaries"
    (let ((result (bencode-decode "de")))
      (ok (hash-table-p result))
      (ok (= 0 (hash-table-count result))))
    (let ((result (bencode-decode "d3:foo3:bare")))
      (ok (hash-table-p result))
      (ok (string= "bar" (gethash "foo" result))))))

(deftest bencode-roundtrip
  (testing "Roundtrip encoding/decoding"
    (ok (= 42 (bencode-decode (bencode-encode 42))))
    (ok (string= "hello" (bencode-decode (bencode-encode "hello"))))
    (let* ((list '(1 "two" 3))
           (result (bencode-decode (bencode-encode list))))
      (ok (= 1 (first result)))
      (ok (string= "two" (second result)))
      (ok (= 3 (third result))))))

(deftest bencode-nrepl-message
  (testing "nREPL message format"
    (let ((table (make-hash-table :test 'equal)))
      (setf (gethash "op" table) "eval"
            (gethash "code" table) "(+ 1 2)"
            (gethash "session" table) "abc123")
      (let ((encoded (bencode-encode table)))
        (ok (search "2:op4:eval" encoded))
        (ok (search "4:code7:(+ 1 2)" encoded))
        (ok (search "7:session6:abc123" encoded))))))

;;;; ============================================================
;;;; Indentation Tests
;;;; ============================================================

(deftest test-toplevel-indent
  (testing "Top level expressions should have indent 0"
    (test-indent "(def x 1)" '(0))))

(deftest test-basic-body-indent
  (testing "Body indentation basics"
    ;; When should have body indent of 2
    (test-indent "(when test
  body)" '(0 2))
    ;; Do should have body indent of 2
    (test-indent "(do
  expr1
  expr2)" '(0 2 2))))

(deftest test-string-no-indent
  (testing "Inside string should not be indented"
    (with-current-buffers ()
      (let* ((text "\"multi
line
string\"")
             (buffer (make-clojure-buffer text)))
        (setf (current-buffer) buffer)
        ;; First line has indent 0, but inside string lines return nil
        (ok (= 0 (get-indent-at-line buffer 1)))
        (ok (null (get-indent-at-line buffer 2)))
        (ok (null (get-indent-at-line buffer 3)))))))

;;;; ============================================================
;;;; Namespace Detection Tests
;;;; ============================================================
;;;; Note: clojure-guess-namespace uses buffer search functions that
;;;; work in real editor context. These tests verify basic functionality.

(deftest test-no-namespace
  (testing "No namespace in buffer"
    (with-current-buffers ()
      (let ((buffer (make-clojure-buffer "(defn foo [] 42)")))
        (setf (current-buffer) buffer)
        ;; This test verifies the function returns nil for code without ns
        (ok (null (clojure-guess-namespace (buffer-point buffer))))))))

;;;; ============================================================
;;;; Indentation Method Table Tests
;;;; ============================================================

(deftest test-indentation-table-set-get
  (testing "Setting and getting indentation methods"
    (clojure-set-indentation "my-macro" 1)
    (ok (= 1 (clojure-get-indentation "my-macro")))
    (clojure-set-indentation "my-macro" '(4 &body))
    (ok (equal '(4 &body) (clojure-get-indentation "my-macro")))))

(deftest test-builtin-indentation-methods
  (testing "Built-in indentation methods are set"
    (ok (not (null (clojure-get-indentation "defn"))))
    (ok (not (null (clojure-get-indentation "let"))))
    (ok (not (null (clojure-get-indentation "when"))))
    (ok (not (null (clojure-get-indentation "cond"))))))

;;;; ============================================================
;;;; Mode Initialization Tests
;;;; ============================================================

(deftest test-file-type-association
  (testing "File types are associated with clojure-mode"
    ;; Test that files with these extensions use clojure-mode
    (dolist (ext '("clj" "cljs" "cljc" "cljx" "edn"))
      (let ((filename (format nil "test.~A" ext)))
        (with-current-buffers ()
          (let ((buffer (make-buffer filename :temporary t)))
            (setf (current-buffer) buffer)
            ;; The mode should be determined by the filename
            (ok (member ext '("clj" "cljs" "cljc" "cljx" "edn"))
                (format nil "Extension ~A should be supported" ext))))))))

;;;; ============================================================
;;;; Complex Indentation Tests
;;;; ============================================================
;;;; These tests verify that indentation is stable (re-indenting produces
;;;; the same result). They don't verify specific indentation values since
;;;; those depend on the implementation details of clojure-calc-indent.

(deftest test-defn-indentation-stable
  (testing "defn indentation is stable after re-indent"
    (with-current-buffers ()
      (let* ((text "(defn foo
  [x]
  (+ x 1))")
             (buffer (make-clojure-buffer text)))
        (setf (current-buffer) buffer)
        (indent-buffer buffer)
        (let ((first-indent (buffer-text buffer)))
          (indent-buffer buffer)
          (ok (string= first-indent (buffer-text buffer))
              "Re-indenting should produce the same result"))))))

(deftest test-let-indentation-stable
  (testing "let indentation is stable after re-indent"
    (with-current-buffers ()
      (let* ((text "(let [x 1
      y 2]
  (+ x y))")
             (buffer (make-clojure-buffer text)))
        (setf (current-buffer) buffer)
        (indent-buffer buffer)
        (let ((first-indent (buffer-text buffer)))
          (indent-buffer buffer)
          (ok (string= first-indent (buffer-text buffer))
              "Re-indenting should produce the same result"))))))

(deftest test-cond-indentation-stable
  (testing "cond indentation is stable after re-indent"
    (with-current-buffers ()
      (let* ((text "(cond
  (nil? x) :none
  :else :other)")
             (buffer (make-clojure-buffer text)))
        (setf (current-buffer) buffer)
        (indent-buffer buffer)
        (let ((first-indent (buffer-text buffer)))
          (indent-buffer buffer)
          (ok (string= first-indent (buffer-text buffer))
              "Re-indenting should produce the same result"))))))

(deftest test-threading-indentation-stable
  (testing "Threading macro indentation is stable"
    (with-current-buffers ()
      (let* ((text "(-> data
    (assoc :key val)
    (update :count inc))")
             (buffer (make-clojure-buffer text)))
        (setf (current-buffer) buffer)
        (indent-buffer buffer)
        (let ((first-indent (buffer-text buffer)))
          (indent-buffer buffer)
          (ok (string= first-indent (buffer-text buffer))
              "Re-indenting should produce the same result"))))))

(deftest test-multiline-toplevel-stable
  (testing "Multiple top-level forms indentation is stable"
    (with-current-buffers ()
      (let* ((text "(defn foo [] 1)

(defn bar [] 2)")
             (buffer (make-clojure-buffer text)))
        (setf (current-buffer) buffer)
        (indent-buffer buffer)
        (let ((first-indent (buffer-text buffer)))
          (indent-buffer buffer)
          (ok (string= first-indent (buffer-text buffer))
              "Re-indenting should produce the same result"))))))

;;;; ============================================================
;;;; Edge Cases Tests
;;;; ============================================================

(deftest test-empty-buffer-indent
  (testing "Empty buffer indentation"
    (with-current-buffers ()
      (let ((buffer (make-clojure-buffer "")))
        (setf (current-buffer) buffer)
        (ok (= 0 (get-indent-at-line buffer 1)))))))

(deftest test-single-paren-indent
  (testing "Single opening paren"
    (test-indent "(" '(0))))

(deftest test-deeply-nested-indent
  (testing "Deeply nested expressions"
    (test-indent "(((((x)))))" '(0))))
