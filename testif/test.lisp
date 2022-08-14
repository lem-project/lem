(in-package :testif)

;;; test classes
(defvar *running-test-stack* '())

(defgeneric run (test))
(defgeneric content (test))

(defclass <test> ()
  ((function
    :initarg :function
    :type function
    :reader test-function)
   (depth
    :initarg :depth
    :initform (current-depth)
    :type (integer 0 *)
    :reader test-depth)
   (description
    :initarg :description
    :initform nil
    :type (or null string)
    :reader test-description)))

(defun current-test ()
  (when *running-test-stack*
    (first *running-test-stack*)))

(defun toplevel-test ()
  (when *running-test-stack*
    (lastcar *running-test-stack*)))

(defun toplevel-test-location ()
  (let ((test (toplevel-test)))
    (list (test-pathname test) (test-id test))))

(defun current-depth ()
  (if-let ((test (current-test)))
    (1+ (test-depth test))
    0))

(defun nested-test-p (test)
  (plusp (test-depth test)))

(defun indent (test)
  (when (nested-test-p test)
    (format t "~vA" (* 2 (test-depth test)) #\Space)))

(defun print-test-log (test)
  (indent test)
  (format t "~A~:[~;: ~:*~A~]~%" (content test) (test-description test)))

(defmethod run :around ((test <test>))
  (let ((*running-test-stack* (cons test *running-test-stack*)))
    (print-test-log test)
    (call-next-method)))

;; test class
(defclass test (<test>)
  ((id
    :initarg :id
    :type identifier
    :reader test-id)
   (pathname
    :initarg :pathname
    :reader test-pathname)))

(defmethod print-object ((test test) stream)
  (print-unreadable-object (test stream :type t :identity t)
    (princ (identifier-name (test-id test)) stream)))

(defmethod content ((test test))
  (format nil "Run ~A" (identifier-name (test-id test))))

(defmethod run ((test test))
  (funcall (test-function test)))

;; assertion class
(defclass assertion (<test>)
  ((form :initarg :form
         :reader assertion-form)))

(defmethod content ((test assertion))
  (format nil "Run ~S" (assertion-form test)))

(defmethod run ((test assertion))
  (destructuring-bind (pathname test-id) (toplevel-test-location)
    (add-result (make-instance (if (funcall (test-function test))
                                   'test-passed
                                   'test-failed)
                               :form (assertion-form test)
                               :description (test-description test)
                               :pathname pathname
                               :test-id test-id))))

;; pass class
(defclass pass (<test>)
  ())

(defmethod content ((test pass))
  "Pass")

(defmethod run ((test pass))
  (destructuring-bind (pathname test-id) (toplevel-test-location)
    (add-result (make-instance 'test-passed
                               :description (test-description test)
                               :pathname pathname
                               :test-id test-id))))

;; fail class
(defclass fail (<test>)
  ())

(defmethod content ((test fail))
  "Fail")

(defmethod run ((test fail))
  (destructuring-bind (pathname test-id) (toplevel-test-location)
    (add-result (make-instance 'test-failed
                               :description (test-description test)
                               :pathname pathname
                               :test-id test-id))))
