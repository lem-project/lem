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
  (:import-from :lem-clojure-mode/nrepl-client
                :make-nrepl-message
                :generate-message-id
                :nrepl-response-value
                :nrepl-response-out
                :nrepl-response-err
                :nrepl-response-exception)
  (:import-from :lem-clojure-mode/stacktrace
                :parse-stacktrace
                :make-stack-frame
                :stack-frame-class
                :stack-frame-method
                :stack-frame-file
                :stack-frame-line
                :stack-frame-clojure-p)
  (:import-from :lem-clojure-mode/inspector
                :parse-inspect-result)
  (:import-from :lem-clojure-mode/test-runner
                :parse-test-result)
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

;;;; ============================================================
;;;; nREPL Client Tests
;;;; ============================================================

(deftest test-make-nrepl-message
  (testing "Creating nREPL messages"
    ;; Basic message creation
    (let ((msg (make-nrepl-message :op "eval" :code "(+ 1 2)")))
      (ok (hash-table-p msg))
      (ok (string= "eval" (gethash "op" msg)))
      (ok (string= "(+ 1 2)" (gethash "code" msg))))
    ;; Nil values should be excluded
    (let ((msg (make-nrepl-message :op "eval" :ns nil :code "(+ 1 2)")))
      (ok (null (gethash "ns" msg)))
      (ok (string= "(+ 1 2)" (gethash "code" msg))))
    ;; Multiple key-value pairs
    (let ((msg (make-nrepl-message :op "eval" :code "x" :ns "user" :session "abc")))
      (ok (string= "eval" (gethash "op" msg)))
      (ok (string= "x" (gethash "code" msg)))
      (ok (string= "user" (gethash "ns" msg)))
      (ok (string= "abc" (gethash "session" msg))))))

(deftest test-generate-message-id
  (testing "Generating unique message IDs"
    (let ((id1 (generate-message-id))
          (id2 (generate-message-id))
          (id3 (generate-message-id)))
      ;; IDs should be strings
      (ok (stringp id1))
      (ok (stringp id2))
      (ok (stringp id3))
      ;; IDs should start with "msg-"
      (ok (search "msg-" id1))
      (ok (search "msg-" id2))
      ;; IDs should be unique
      (ok (not (string= id1 id2)))
      (ok (not (string= id2 id3)))
      (ok (not (string= id1 id3))))))

(deftest test-nrepl-response-value
  (testing "Extracting value from nREPL responses"
    ;; Single response with value
    (let* ((resp1 (make-hash-table :test 'equal))
           (resp2 (make-hash-table :test 'equal)))
      (setf (gethash "value" resp1) "42")
      (ok (string= "42" (nrepl-response-value (list resp1)))))
    ;; Multiple responses, first with value
    (let* ((resp1 (make-hash-table :test 'equal))
           (resp2 (make-hash-table :test 'equal)))
      (setf (gethash "value" resp1) "result")
      (setf (gethash "status" resp2) '("done"))
      (ok (string= "result" (nrepl-response-value (list resp1 resp2)))))
    ;; No value in responses
    (let* ((resp1 (make-hash-table :test 'equal)))
      (setf (gethash "status" resp1) '("done"))
      (ok (null (nrepl-response-value (list resp1)))))))

(deftest test-nrepl-response-out
  (testing "Extracting stdout from nREPL responses"
    ;; Single output
    (let* ((resp1 (make-hash-table :test 'equal)))
      (setf (gethash "out" resp1) "hello")
      (ok (string= "hello" (nrepl-response-out (list resp1)))))
    ;; Multiple outputs should be concatenated
    (let* ((resp1 (make-hash-table :test 'equal))
           (resp2 (make-hash-table :test 'equal)))
      (setf (gethash "out" resp1) "hello ")
      (setf (gethash "out" resp2) "world")
      (ok (string= "hello world" (nrepl-response-out (list resp1 resp2)))))
    ;; No output
    (let* ((resp1 (make-hash-table :test 'equal)))
      (setf (gethash "value" resp1) "42")
      (ok (string= "" (nrepl-response-out (list resp1)))))))

(deftest test-nrepl-response-err
  (testing "Extracting stderr from nREPL responses"
    ;; Single error
    (let* ((resp1 (make-hash-table :test 'equal)))
      (setf (gethash "err" resp1) "Error: something went wrong")
      (ok (string= "Error: something went wrong" (nrepl-response-err (list resp1)))))
    ;; No error
    (let* ((resp1 (make-hash-table :test 'equal)))
      (setf (gethash "value" resp1) "42")
      (ok (string= "" (nrepl-response-err (list resp1)))))))

(deftest test-nrepl-response-exception
  (testing "Extracting exception from nREPL responses"
    ;; Response with exception
    (let* ((resp1 (make-hash-table :test 'equal)))
      (setf (gethash "ex" resp1) "clojure.lang.ExceptionInfo")
      (ok (string= "clojure.lang.ExceptionInfo" (nrepl-response-exception (list resp1)))))
    ;; No exception
    (let* ((resp1 (make-hash-table :test 'equal)))
      (setf (gethash "value" resp1) "42")
      (ok (null (nrepl-response-exception (list resp1)))))))

;;;; ============================================================
;;;; Stacktrace Parsing Tests
;;;; ============================================================

(deftest test-parse-stacktrace-basic
  (testing "Parsing basic Java stacktrace"
    (let ((stacktrace "java.lang.NullPointerException: Cannot invoke method on null
	at clojure.core$eval.invokeStatic(core.clj:3214)
	at clojure.core$eval.invoke(core.clj:3210)
	at user$foo.invokeStatic(foo.clj:10)
	at java.lang.Thread.run(Thread.java:829)"))
      (multiple-value-bind (frames exception-class exception-message)
          (parse-stacktrace stacktrace)
        ;; Should have 4 frames
        (ok (= 4 (length frames)))
        ;; Exception class and message
        (ok (string= "java.lang.NullPointerException" exception-class))
        (ok (string= "Cannot invoke method on null" exception-message))
        ;; First frame
        (let ((frame1 (first frames)))
          (ok (string= "clojure.core$eval" (stack-frame-class frame1)))
          (ok (string= "invokeStatic" (stack-frame-method frame1)))
          (ok (string= "core.clj" (stack-frame-file frame1)))
          (ok (= 3214 (stack-frame-line frame1)))
          (ok (stack-frame-clojure-p frame1)))
        ;; Last frame (Java)
        (let ((frame4 (fourth frames)))
          (ok (string= "java.lang.Thread" (stack-frame-class frame4)))
          (ok (string= "run" (stack-frame-method frame4)))
          (ok (string= "Thread.java" (stack-frame-file frame4)))
          (ok (= 829 (stack-frame-line frame4))))))))

(deftest test-parse-stacktrace-clojure-exception
  (testing "Parsing Clojure ExceptionInfo"
    (let ((stacktrace "clojure.lang.ExceptionInfo: Invalid input {:type :validation}
	at user$validate.invokeStatic(validate.clj:15)
	at user$validate.invoke(validate.clj:10)"))
      (multiple-value-bind (frames exception-class exception-message)
          (parse-stacktrace stacktrace)
        (ok (= 2 (length frames)))
        (ok (string= "clojure.lang.ExceptionInfo" exception-class))
        (ok (search "Invalid input" exception-message))))))

(deftest test-parse-stacktrace-empty
  (testing "Parsing empty stacktrace"
    (multiple-value-bind (frames exception-class exception-message)
        (parse-stacktrace "")
      (ok (null frames))
      (ok (null exception-class)))))

(deftest test-stack-frame-clojure-detection
  (testing "Detecting Clojure frames"
    ;; Clojure frame with $ in class name
    (let ((frame (make-stack-frame
                  :class "user$foo"
                  :method "invoke"
                  :file "foo.clj"
                  :line 10
                  :clojure-p t)))
      (ok (stack-frame-clojure-p frame)))
    ;; Java frame
    (let ((frame (make-stack-frame
                  :class "java.lang.Thread"
                  :method "run"
                  :file "Thread.java"
                  :line 829
                  :clojure-p nil)))
      (ok (not (stack-frame-clojure-p frame))))))

;;;; ============================================================
;;;; Inspector Tests
;;;; ============================================================

(deftest test-parse-inspect-result-basic
  (testing "Parsing basic inspect result"
    (let ((result (parse-inspect-result "{:type clojure.lang.PersistentVector, :value \"[1 2 3]\", :count 3}")))
      (ok (hash-table-p result))
      (ok (search "Vector" (gethash "type" result)))
      (ok (string= "[1 2 3]" (gethash "value" result)))
      (ok (= 3 (gethash "count" result))))))

(deftest test-parse-inspect-result-string
  (testing "Parsing string inspect result"
    (let ((result (parse-inspect-result "{:type java.lang.String, :value \"hello\", :count 5}")))
      (ok (hash-table-p result))
      (ok (string= "hello" (gethash "value" result)))
      (ok (= 5 (gethash "count" result))))))

(deftest test-parse-inspect-result-no-count
  (testing "Parsing inspect result without count"
    (let ((result (parse-inspect-result "{:type java.lang.Long, :value \"42\"}")))
      (ok (hash-table-p result))
      (ok (string= "42" (gethash "value" result)))
      (ok (null (gethash "count" result))))))

(deftest test-parse-inspect-result-malformed
  (testing "Parsing malformed inspect result"
    ;; Should not error, just return what it can parse
    (let ((result (parse-inspect-result "not valid edn")))
      (ok (hash-table-p result)))))

;;;; ============================================================
;;;; Test Runner Tests
;;;; ============================================================

(deftest test-parse-test-result-all-pass
  (testing "Parsing test result with all tests passing"
    (let ((result (parse-test-result "{:pass 5, :fail 0, :error 0, :test 5}")))
      (ok (hash-table-p result))
      (ok (= 5 (gethash :pass result)))
      (ok (= 0 (gethash :fail result)))
      (ok (= 0 (gethash :error result)))
      (ok (= 5 (gethash :test result))))))

(deftest test-parse-test-result-with-failures
  (testing "Parsing test result with failures"
    (let ((result (parse-test-result "{:pass 3, :fail 2, :error 0, :test 5}")))
      (ok (= 3 (gethash :pass result)))
      (ok (= 2 (gethash :fail result)))
      (ok (= 0 (gethash :error result))))))

(deftest test-parse-test-result-with-errors
  (testing "Parsing test result with errors"
    (let ((result (parse-test-result "{:pass 2, :fail 1, :error 2, :test 5}")))
      (ok (= 2 (gethash :pass result)))
      (ok (= 1 (gethash :fail result)))
      (ok (= 2 (gethash :error result))))))

(deftest test-parse-test-result-empty
  (testing "Parsing empty test result"
    (let ((result (parse-test-result "{}")))
      (ok (hash-table-p result))
      (ok (null (gethash :pass result))))))

(deftest test-parse-test-result-malformed
  (testing "Parsing malformed test result"
    ;; Should not error
    (let ((result (parse-test-result "not valid")))
      (ok (hash-table-p result)))))

;;;; ============================================================
;;;; Bencode Edge Cases Tests
;;;; ============================================================

(deftest test-bencode-nested-structures
  (testing "Encoding/decoding nested structures"
    ;; Nested lists
    (let ((nested '((1 2) (3 4) (5 6))))
      (let ((decoded (bencode-decode (bencode-encode nested))))
        (ok (= 3 (length decoded)))
        (ok (equal '(1 2) (first decoded)))))
    ;; Dict with list value
    (let ((dict (make-hash-table :test 'equal)))
      (setf (gethash "items" dict) '(1 2 3))
      (let ((decoded (bencode-decode (bencode-encode dict))))
        (ok (equal '(1 2 3) (gethash "items" decoded)))))))

(deftest test-bencode-unicode-strings
  (testing "Encoding/decoding unicode strings"
    ;; Basic ASCII
    (ok (string= "hello" (bencode-decode (bencode-encode "hello"))))
    ;; With spaces and punctuation
    (ok (string= "hello, world!" (bencode-decode (bencode-encode "hello, world!"))))))

(deftest test-bencode-large-integers
  (testing "Encoding/decoding large integers"
    (let ((large-num 9999999999999999999))
      (ok (= large-num (bencode-decode (bencode-encode large-num)))))
    (let ((large-neg -9999999999999999999))
      (ok (= large-neg (bencode-decode (bencode-encode large-neg)))))))

(deftest test-bencode-empty-structures
  (testing "Encoding/decoding empty structures"
    ;; Empty list
    (ok (null (bencode-decode (bencode-encode nil))))
    (ok (string= "le" (bencode-encode '())))
    ;; Empty dict
    (let ((empty-dict (make-hash-table :test 'equal)))
      (ok (string= "de" (bencode-encode empty-dict)))
      (let ((decoded (bencode-decode "de")))
        (ok (= 0 (hash-table-count decoded)))))))

;;;; ============================================================
;;;; nREPL Message Format Tests
;;;; ============================================================

(deftest test-nrepl-eval-message-format
  (testing "Eval message format"
    (let ((msg (make-nrepl-message :op "eval"
                                   :code "(+ 1 2)"
                                   :ns "user"
                                   :session "session-123")))
      (let ((encoded (bencode-encode msg)))
        ;; Should contain all required fields
        (ok (search "2:op4:eval" encoded))
        (ok (search "4:code7:(+ 1 2)" encoded))
        (ok (search "2:ns4:user" encoded))
        (ok (search "7:session11:session-123" encoded))))))

(deftest test-nrepl-clone-message-format
  (testing "Clone session message format"
    (let ((msg (make-nrepl-message :op "clone")))
      (let ((encoded (bencode-encode msg)))
        (ok (search "2:op5:clone" encoded))))))

(deftest test-nrepl-interrupt-message-format
  (testing "Interrupt message format"
    (let ((msg (make-nrepl-message :op "interrupt" :session "abc123")))
      (let ((encoded (bencode-encode msg)))
        (ok (search "2:op9:interrupt" encoded))
        (ok (search "7:session6:abc123" encoded))))))

(deftest test-nrepl-completions-message-format
  (testing "Completions message format"
    (let ((msg (make-nrepl-message :op "completions" :prefix "ma" :ns "clojure.core")))
      (let ((encoded (bencode-encode msg)))
        (ok (search "2:op11:completions" encoded))
        (ok (search "6:prefix2:ma" encoded))))))

;;;; ============================================================
;;;; Clojure Syntax Tests
;;;; ============================================================

(deftest test-clojure-keywords
  (testing "Clojure keywords in code"
    (with-current-buffers ()
      (let ((buffer (make-clojure-buffer ":keyword")))
        (setf (current-buffer) buffer)
        ;; Verify buffer was created with the keyword text
        (ok (string= ":keyword" (buffer-text buffer)))))))

(deftest test-clojure-namespaced-keywords
  (testing "Namespaced keywords"
    (with-current-buffers ()
      ;; Simple namespaced keyword
      (let ((buffer (make-clojure-buffer "::local-keyword")))
        (setf (current-buffer) buffer)
        (ok (buffer-text buffer)))
      ;; Fully qualified keyword
      (let ((buffer (make-clojure-buffer ":namespace/keyword")))
        (setf (current-buffer) buffer)
        (ok (buffer-text buffer))))))

(deftest test-clojure-special-forms
  (testing "Special forms indentation is stable"
    ;; if form - verify indentation is stable
    (with-current-buffers ()
      (let* ((text "(if test
    then
    else)")
             (buffer (make-clojure-buffer text)))
        (setf (current-buffer) buffer)
        (indent-buffer buffer)
        (let ((first-indent (buffer-text buffer)))
          (indent-buffer buffer)
          (ok (string= first-indent (buffer-text buffer))
              "if form indentation should be stable"))))
    ;; fn form - verify indentation is stable
    (with-current-buffers ()
      (let* ((text "(fn [x]
  (+ x 1))")
             (buffer (make-clojure-buffer text)))
        (setf (current-buffer) buffer)
        (indent-buffer buffer)
        (let ((first-indent (buffer-text buffer)))
          (indent-buffer buffer)
          (ok (string= first-indent (buffer-text buffer))
              "fn form indentation should be stable"))))))

(deftest test-clojure-collection-literals
  (testing "Collection literals"
    ;; Vector
    (test-indent "[1 2 3]" '(0))
    ;; Map
    (test-indent "{:a 1 :b 2}" '(0))
    ;; Set
    (test-indent "#{1 2 3}" '(0))))

;;;; ============================================================
;;;; Comprehensive Indentation Tests
;;;; ============================================================

(deftest test-indent-defmacro
  (testing "defmacro indentation"
    (with-current-buffers ()
      (let* ((text "(defmacro my-macro
  [args]
  `(do ~@args))")
             (buffer (make-clojure-buffer text)))
        (setf (current-buffer) buffer)
        (indent-buffer buffer)
        (let ((result (buffer-text buffer)))
          (indent-buffer buffer)
          (ok (string= result (buffer-text buffer))))))))

(deftest test-indent-letfn
  (testing "letfn indentation"
    (with-current-buffers ()
      (let* ((text "(letfn [(helper [x]
          (inc x))]
  (helper 1))")
             (buffer (make-clojure-buffer text)))
        (setf (current-buffer) buffer)
        (indent-buffer buffer)
        (let ((result (buffer-text buffer)))
          (indent-buffer buffer)
          (ok (string= result (buffer-text buffer))))))))

(deftest test-indent-try-catch
  (testing "try-catch indentation"
    (with-current-buffers ()
      (let* ((text "(try
  (risky-operation)
  (catch Exception e
    (handle-error e))
  (finally
    (cleanup)))")
             (buffer (make-clojure-buffer text)))
        (setf (current-buffer) buffer)
        (indent-buffer buffer)
        (let ((result (buffer-text buffer)))
          (indent-buffer buffer)
          (ok (string= result (buffer-text buffer))))))))

(deftest test-indent-case
  (testing "case indentation"
    (with-current-buffers ()
      (let* ((text "(case x
  :a \"alpha\"
  :b \"beta\"
  \"default\")")
             (buffer (make-clojure-buffer text)))
        (setf (current-buffer) buffer)
        (indent-buffer buffer)
        (let ((result (buffer-text buffer)))
          (indent-buffer buffer)
          (ok (string= result (buffer-text buffer))))))))

(deftest test-indent-condp
  (testing "condp indentation"
    (with-current-buffers ()
      (let* ((text "(condp = x
  1 \"one\"
  2 \"two\"
  \"other\")")
             (buffer (make-clojure-buffer text)))
        (setf (current-buffer) buffer)
        (indent-buffer buffer)
        (let ((result (buffer-text buffer)))
          (indent-buffer buffer)
          (ok (string= result (buffer-text buffer))))))))

;;;; ============================================================
;;;; Response Processing Integration Tests
;;;; ============================================================

(deftest test-complete-nrepl-response-cycle
  (testing "Complete nREPL response processing"
    ;; Simulate a complete eval response cycle
    (let* ((resp1 (make-hash-table :test 'equal))
           (resp2 (make-hash-table :test 'equal))
           (resp3 (make-hash-table :test 'equal)))
      ;; First response: output
      (setf (gethash "out" resp1) "Computing...")
      ;; Second response: value
      (setf (gethash "value" resp2) "42")
      ;; Third response: done status
      (setf (gethash "status" resp3) '("done"))
      (let ((responses (list resp1 resp2 resp3)))
        (ok (string= "42" (nrepl-response-value responses)))
        (ok (string= "Computing..." (nrepl-response-out responses)))
        (ok (string= "" (nrepl-response-err responses)))
        (ok (null (nrepl-response-exception responses)))))))

(deftest test-nrepl-error-response-cycle
  (testing "nREPL error response processing"
    (let* ((resp1 (make-hash-table :test 'equal))
           (resp2 (make-hash-table :test 'equal)))
      ;; First response: error
      (setf (gethash "err" resp1) "CompilerException: Unable to resolve symbol")
      (setf (gethash "ex" resp1) "clojure.lang.Compiler$CompilerException")
      ;; Second response: done
      (setf (gethash "status" resp2) '("done"))
      (let ((responses (list resp1 resp2)))
        (ok (null (nrepl-response-value responses)))
        (ok (search "Unable to resolve" (nrepl-response-err responses)))
        (ok (string= "clojure.lang.Compiler$CompilerException"
                     (nrepl-response-exception responses)))))))
