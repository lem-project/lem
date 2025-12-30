(defpackage :lem-clojure-mode/test-runner
  (:use :cl :lem)
  (:import-from :lem-clojure-mode/nrepl-client
                :check-nrepl-connection
                :nrepl-eval
                :nrepl-eval-sync
                :nrepl-response-value
                :nrepl-response-out
                :nrepl-response-err)
  (:import-from :lem-clojure-mode
                :clojure-mode
                :clojure-current-namespace)
  (:export :clojure-run-test-at-point
           :clojure-run-tests
           :clojure-run-all-tests
           :clojure-rerun-last-test))

(in-package :lem-clojure-mode/test-runner)

;;;; Attributes

(define-attribute test-pass-attribute
  (t :foreground "#98c379" :bold t))  ; green

(define-attribute test-fail-attribute
  (t :foreground "#e06c75" :bold t))  ; red

(define-attribute test-error-attribute
  (t :foreground "#d19a66" :bold t))  ; orange

(define-attribute test-pending-attribute
  (t :foreground "#61afef"))  ; blue

;;;; Test Result Buffer

(defvar *test-result-buffer-name* "*clojure-test-results*")
(defvar *last-test-ns* nil)
(defvar *last-test-name* nil)

(define-major-mode clojure-test-result-mode clojure-mode
    (:name "Clojure Test Results"
     :keymap *clojure-test-result-mode-keymap*)
  (setf (buffer-read-only-p (current-buffer)) t))

(define-key *clojure-test-result-mode-keymap* "q" 'quit-active-window)
(define-key *clojure-test-result-mode-keymap* "g" 'clojure-rerun-last-test)
(define-key *clojure-test-result-mode-keymap* "n" 'clojure-test-next-failure)
(define-key *clojure-test-result-mode-keymap* "p" 'clojure-test-prev-failure)

(defun test-result-buffer ()
  "Get or create the test result buffer."
  (or (get-buffer *test-result-buffer-name*)
      (let ((buffer (make-buffer *test-result-buffer-name* :enable-undo-p nil)))
        (change-buffer-mode buffer 'clojure-test-result-mode)
        buffer)))

(defun show-test-results ()
  "Show the test result buffer."
  (let ((buffer (test-result-buffer)))
    (pop-to-buffer buffer)))

;;;; Test Discovery

(defun find-deftest-at-point ()
  "Find the deftest form at or before point and return its name."
  (with-point ((p (current-point)))
    ;; Search backward for (deftest name
    (when (search-backward-regexp p "\\(deftest\\s+([a-zA-Z][a-zA-Z0-9*+!_'?<>=/-]*)")
      (character-offset p 9)  ; skip past "(deftest "
      (skip-whitespace-forward p)
      (let ((start (copy-point p :temporary)))
        (skip-chars-forward p (lambda (c)
                                (or (alphanumericp c)
                                    (member c '(#\- #\_ #\* #\+ #\! #\' #\? #\< #\> #\= #\/)))))
        (points-to-string start p)))))

;;;; Test Execution

(defun run-test-code (test-name ns)
  "Generate code to run a single test."
  (format nil "
(require 'clojure.test)
(let [test-var (resolve (symbol ~S ~S))
      result (atom {:pass 0 :fail 0 :error 0})]
  (binding [clojure.test/*report-counters* result
            clojure.test/report (fn [m]
                                  (case (:type m)
                                    :pass (swap! result update :pass inc)
                                    :fail (do
                                            (swap! result update :fail inc)
                                            (swap! result update :failures
                                                   (fnil conj [])
                                                   {:expected (:expected m)
                                                    :actual (:actual m)
                                                    :message (:message m)}))
                                    :error (do
                                             (swap! result update :error inc)
                                             (swap! result update :errors
                                                    (fnil conj [])
                                                    {:message (str (:actual m))}))
                                    nil))]
    (test-var)
    @result))
" ns test-name))

(defun run-ns-tests-code (ns)
  "Generate code to run all tests in a namespace."
  (format nil "
(require 'clojure.test)
(require '~A :reload)
(let [summary (clojure.test/run-tests '~A)]
  {:pass (:pass summary)
   :fail (:fail summary)
   :error (:error summary)
   :test (:test summary)})
" ns ns))

(defun run-all-tests-code ()
  "Generate code to run all tests."
  "
(require 'clojure.test)
(let [summary (clojure.test/run-all-tests)]
  {:pass (:pass summary)
   :fail (:fail summary)
   :error (:error summary)
   :test (:test summary)})
")

(defun parse-test-result (result-str)
  "Parse a test result EDN string into a plist."
  (let ((result (make-hash-table :test 'equal)))
    (handler-case
        (let ((str (string-trim '(#\space #\newline) result-str)))
          ;; Extract counts
          (alexandria:when-let ((pass (nth-value 1 (ppcre:scan-to-strings ":pass\\s+([0-9]+)" str))))
            (setf (gethash :pass result) (parse-integer (aref pass 0))))
          (alexandria:when-let ((fail (nth-value 1 (ppcre:scan-to-strings ":fail\\s+([0-9]+)" str))))
            (setf (gethash :fail result) (parse-integer (aref fail 0))))
          (alexandria:when-let ((error (nth-value 1 (ppcre:scan-to-strings ":error\\s+([0-9]+)" str))))
            (setf (gethash :error result) (parse-integer (aref error 0))))
          (alexandria:when-let ((test (nth-value 1 (ppcre:scan-to-strings ":test\\s+([0-9]+)" str))))
            (setf (gethash :test result) (parse-integer (aref test 0))))
          result)
      (error (e)
        (declare (ignore e))
        result))))

(defun display-test-result (ns test-name result output)
  "Display test results in the result buffer."
  (let ((buffer (test-result-buffer))
        (pass (or (gethash :pass result) 0))
        (fail (or (gethash :fail result) 0))
        (error-count (or (gethash :error result) 0))
        (total (or (gethash :test result) 1)))
    (with-buffer-read-only buffer nil
      (erase-buffer buffer)
      (with-point ((point (buffer-point buffer)))
        ;; Header
        (insert-string point "Clojure Test Results" :attribute 'test-pending-attribute)
        (insert-character point #\newline)
        (insert-string point (make-string 50 :initial-element #\=))
        (insert-character point #\newline)
        (insert-character point #\newline)
        ;; Test info
        (insert-string point (format nil "Namespace: ~A~%" ns))
        (when test-name
          (insert-string point (format nil "Test: ~A~%" test-name)))
        (insert-character point #\newline)
        ;; Summary
        (insert-string point "Summary: ")
        (let ((all-pass (and (zerop fail) (zerop error-count))))
          (insert-string point
                         (format nil "~D passed" pass)
                         :attribute (if all-pass 'test-pass-attribute 'test-pending-attribute))
          (insert-string point ", ")
          (insert-string point
                         (format nil "~D failed" fail)
                         :attribute (if (plusp fail) 'test-fail-attribute 'test-pending-attribute))
          (insert-string point ", ")
          (insert-string point
                         (format nil "~D errors" error-count)
                         :attribute (if (plusp error-count) 'test-error-attribute 'test-pending-attribute))
          (insert-string point (format nil " (~D total)~%" total)))
        ;; Overall status
        (insert-character point #\newline)
        (cond
          ((and (zerop fail) (zerop error-count))
           (insert-string point "STATUS: PASSED" :attribute 'test-pass-attribute))
          ((plusp error-count)
           (insert-string point "STATUS: ERROR" :attribute 'test-error-attribute))
          (t
           (insert-string point "STATUS: FAILED" :attribute 'test-fail-attribute)))
        (insert-character point #\newline)
        ;; Output
        (when (and output (plusp (length output)))
          (insert-character point #\newline)
          (insert-string point (make-string 50 :initial-element #\-))
          (insert-character point #\newline)
          (insert-string point "Test Output:")
          (insert-character point #\newline)
          (insert-string point output))
        ;; Navigation hint
        (insert-character point #\newline)
        (insert-character point #\newline)
        (insert-string point "Press 'q' to quit, 'g' to rerun, 'n'/'p' for next/prev failure"
                       :attribute 'test-pending-attribute)))
    (buffer-start (buffer-point buffer))
    (show-test-results)))

;;;; Commands

(define-command clojure-run-test-at-point () ()
  "Run the test at point."
  (check-nrepl-connection)
  (let ((test-name (find-deftest-at-point))
        (ns (clojure-current-namespace)))
    (unless test-name
      (editor-error "No test found at point"))
    (unless ns
      (editor-error "Could not determine namespace"))
    (setf *last-test-ns* ns
          *last-test-name* test-name)
    (message "Running test: ~A/~A..." ns test-name)
    (let* ((code (run-test-code test-name ns))
           (responses (nrepl-eval-sync code :ns ns :timeout 60)))
      (let ((value (nrepl-response-value responses))
            (output (nrepl-response-out responses))
            (err (nrepl-response-err responses)))
        (cond
          (err
           (message "Test error: ~A" err)
           (display-test-result ns test-name (make-hash-table) err))
          (value
           (let ((result (parse-test-result value)))
             (if (and (zerop (or (gethash :fail result) 0))
                      (zerop (or (gethash :error result) 0)))
                 (message "Test passed: ~A" test-name)
                 (message "Test failed: ~A" test-name))
             (display-test-result ns test-name result output))))))))

(define-command clojure-run-tests () ()
  "Run all tests in the current namespace."
  (check-nrepl-connection)
  (let ((ns (clojure-current-namespace)))
    (unless ns
      (editor-error "Could not determine namespace"))
    (setf *last-test-ns* ns
          *last-test-name* nil)
    (message "Running tests in namespace: ~A..." ns)
    (let* ((code (run-ns-tests-code ns))
           (responses (nrepl-eval-sync code :ns "user" :timeout 120)))
      (let ((value (nrepl-response-value responses))
            (output (nrepl-response-out responses))
            (err (nrepl-response-err responses)))
        (cond
          (err
           (message "Test error: ~A" err)
           (display-test-result ns nil (make-hash-table) err))
          (value
           (let ((result (parse-test-result value)))
             (let ((fail (or (gethash :fail result) 0))
                   (error-count (or (gethash :error result) 0))
                   (total (or (gethash :test result) 0)))
               (if (and (zerop fail) (zerop error-count))
                   (message "All ~D tests passed" total)
                   (message "~D failures, ~D errors out of ~D tests" fail error-count total)))
             (display-test-result ns nil result output))))))))

(define-command clojure-run-all-tests () ()
  "Run all tests in all namespaces."
  (check-nrepl-connection)
  (setf *last-test-ns* nil
        *last-test-name* nil)
  (message "Running all tests...")
  (let* ((code (run-all-tests-code))
         (responses (nrepl-eval-sync code :ns "user" :timeout 300)))
    (let ((value (nrepl-response-value responses))
          (output (nrepl-response-out responses))
          (err (nrepl-response-err responses)))
      (cond
        (err
         (message "Test error: ~A" err)
         (display-test-result "all" nil (make-hash-table) err))
        (value
         (let ((result (parse-test-result value)))
           (let ((fail (or (gethash :fail result) 0))
                 (error-count (or (gethash :error result) 0))
                 (total (or (gethash :test result) 0)))
             (if (and (zerop fail) (zerop error-count))
                 (message "All ~D tests passed" total)
                 (message "~D failures, ~D errors out of ~D tests" fail error-count total)))
           (display-test-result "all" nil result output)))))))

(define-command clojure-rerun-last-test () ()
  "Rerun the last test or test suite."
  (cond
    (*last-test-name*
     ;; Rerun specific test
     (let ((ns *last-test-ns*)
           (test-name *last-test-name*))
       (message "Rerunning test: ~A/~A..." ns test-name)
       (let* ((code (run-test-code test-name ns))
              (responses (nrepl-eval-sync code :ns ns :timeout 60)))
         (let ((value (nrepl-response-value responses))
               (output (nrepl-response-out responses))
               (err (nrepl-response-err responses)))
           (cond
             (err (display-test-result ns test-name (make-hash-table) err))
             (value (display-test-result ns test-name (parse-test-result value) output)))))))
    (*last-test-ns*
     ;; Rerun namespace tests
     (clojure-run-tests))
    (t
     (editor-error "No previous test to rerun"))))

(define-command clojure-test-next-failure () ()
  "Jump to the next test failure in the result buffer."
  (or (search-forward-regexp (current-point) "FAIL\\|ERROR")
      (message "No more failures")))

(define-command clojure-test-prev-failure () ()
  "Jump to the previous test failure in the result buffer."
  (or (search-backward-regexp (current-point) "FAIL\\|ERROR")
      (message "No previous failures")))

;;;; Add keybindings to clojure-mode

(define-key lem-clojure-mode:*clojure-mode-keymap* "C-c C-t t" 'clojure-run-test-at-point)
(define-key lem-clojure-mode:*clojure-mode-keymap* "C-c C-t n" 'clojure-run-tests)
(define-key lem-clojure-mode:*clojure-mode-keymap* "C-c C-t a" 'clojure-run-all-tests)
(define-key lem-clojure-mode:*clojure-mode-keymap* "C-c C-t l" 'clojure-rerun-last-test)
