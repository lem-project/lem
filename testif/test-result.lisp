(in-package :testif)

(defvar *result*)

(defclass test-result ()
  ((form :initarg :form
         :reader test-result-form)
   (description :initarg :description
                :reader test-result-description)
   (pathname :initarg :pathname
             :reader test-result-pathname)
   (test-id :initarg :test-id
            :type identifier
            :reader test-result-test-id)))

(defclass test-passed (test-result) ())
(defclass test-failed (test-result) ())

(defclass test-result-set ()
  ((passed :initform '()
           :accessor test-result-set-passed)
   (failed :initform '()
           :accessor test-result-set-failed)))

(defmethod compute-results ((result test-result-set))
  (let* ((num-passed (length (test-result-set-passed result)))
         (num-failed (length (test-result-set-failed result)))
         (num-tests (+ num-passed num-failed)))
    (if (zerop num-tests)
        (values 0 0 0 nil nil)
        (values num-tests
                num-passed
                num-failed
                (round (* (/ num-passed num-tests) 100))
                (round (* (/ num-failed num-tests) 100))))))

(defun report-result-count (result)
  (multiple-value-bind (num-tests num-passed num-failed passed% failed%)
      (compute-results result)
    (unless (zerop num-tests)
      (format t "~&~
------------------------------
Did ~D checks.
  Pass: ~D (~2D%)
  Fail: ~D (~2D%)
------------------------------
"
              num-tests
              num-passed passed%
              num-failed failed%))))

(defun report-failure-details (result)
  (when-let (failed (test-result-set-failed result))
    (write-line "Failure Details:")
    (dolist (f (reverse failed))
      (format t "  ~A ~A: "
              (test-result-pathname f)
              (identifier-content (test-result-test-id f)))
      (if-let (desc (test-result-description f))
        (format t "~A" desc)
        (format t "~S" (test-result-form f)))
      (terpri))
    (write-line "------------------------------")))

(defmethod test-success-p ((result test-result-set))
  (null (test-result-set-failed result)))

(defmethod report ((result test-result-set))
  (report-result-count result)
  (report-failure-details result)
  (test-success-p result))

(defun add-result (result)
  (etypecase result
    (test-passed (push result (test-result-set-passed *result*)))
    (test-failed (push result (test-result-set-failed *result*)))))

(defun call-with-test-result (function)
  (let ((*result* (make-instance 'test-result-set)))
    (funcall function)
    (report *result*)))
