(in-package :testif/tests)

(defparameter *current-file* (asdf:system-relative-pathname :testif/tests "tests/tests.lisp"))

(defvar *was-run* nil)

(test was-run
  (setq *was-run* t))

(defun was-run-test ()
  (let ((*was-run* nil))
    (run-test 'was-run)
    (assert (eq *was-run* t))))

(defun undefined-test ()
  (handler-case (run-test 'fiddle-de-dee)
    (error (c)
      (assert (equal "Test FIDDLE-DE-DEE is not defined" (princ-to-string c))))
    (:no-error (&rest values)
      (declare (ignore values))
      (error "fail"))))

(test nested-test
  (test nested-one
    (test nested-two))
  (test nested-three))

(defun call-nested-test ()
  (let ((output
          (with-output-to-string (*standard-output*)
            (run-test 'nested-test))))
    (assert (string=
             (format nil "~
Run NESTED-TEST
  Run NESTED-ONE
    Run NESTED-TWO
  Run NESTED-THREE
")
             output))))

(test "example 1"
  (setq *was-run* :example-1))
(test "example 2"
  (setq *was-run* :example-2))

(defun string-name-test ()
  (let ((*was-run* nil))
    (run-test "example 1")
    (assert (eq :example-1 *was-run*)))
  (let ((*was-run* nil))
    (run-test "example 2")
    (assert (eq :example-2 *was-run*))))

(test simple
  (ok (= 1 2) "1 is not 2")
  (ok (= (+ 2 3) 5) "2 + 3 = 5")
  (ok (eq 'a 'b)))

(defun simple-test ()
  (let ((out (with-output-to-string (*standard-output*)
               (run-test 'simple))))
    (assert (string=
             (format nil "Run SIMPLE
  Run (= 1 2): 1 is not 2
  Run (= (+ 2 3) 5): 2 + 3 = 5
  Run (EQ 'A 'B)
------------------------------
Did 3 checks.
  Pass: 1 (33%)
  Fail: 2 (67%)
------------------------------
Failure Details:
  ~A ~A: 1 is not 2
  ~A ~A: (EQ 'A 'B)
------------------------------
"
                     *current-file* 'simple
                     *current-file* 'simple)
             out))))

(defvar *dynamic-name-log* '())

(test dynamic-name
  (loop :for i :from 1 :to 3
        :do (test (format nil "case-~D" i)
              (push i *dynamic-name-log*))))

(defun dynamic-name-test ()
  (let ((*dynamic-name-log* nil))
    (run-test 'dynamic-name)
    (assert (equal '(3 2 1) *dynamic-name-log*))))

(test passed-failed
  (pass "foo")
  (fail "bar"))

(defun passed-failed-test ()
  (let ((out (with-output-to-string (*standard-output*)
               (run-test 'passed-failed))))
    (assert (string=
             (format nil "Run PASSED-FAILED
  Pass: foo
  Fail: bar
------------------------------
Did 2 checks.
  Pass: 1 (50%)
  Fail: 1 (50%)
------------------------------
Failure Details:
  ~A ~A: bar
------------------------------
"
                     *current-file* 'passed-failed)
             out))))

(defun self-test ()
  (identifier-test)
  (let ((*package* (find-package :testif/tests)))
    (was-run-test)
    (undefined-test)
    (call-nested-test)
    (string-name-test)
    (simple-test)
    (dynamic-name-test)
    (passed-failed-test)))
