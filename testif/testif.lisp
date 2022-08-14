(in-package :testif)

(defmacro test (name &body body)
  (if (symbolp name)
      `(%test ',name (lambda () ,@body) (sb-c:source-location))
      `(%test ,name (lambda () ,@body) (sb-c:source-location))))

(defun %test (name function source-location)
  (let ((test (make-instance
               'test
               :id (ensure-identifier name)
               :function function
               :pathname (sb-c:definition-source-location-namestring source-location))))
    (if (nested-test-p test)
        (run test)
        (register-test test))))

(defmacro signals (form &optional (condition 'error))
  (let ((c (gensym))
        (condition-type (gensym)))
    `(let ((,condition-type ,condition))
       (typep (block nil
                (handler-bind ((condition
                                 (lambda (,c)
                                   (when (typep ,c ,condition-type)
                                     (return ,c)))))
                  ,form
                  nil))
              ,condition-type))))

(defmacro ok (form &optional description)
  `(run (make-instance 'assertion
                       :form ',form
                       :function (lambda () ,form)
                       :description ,description)))

(defun pass (description)
  (run (make-instance 'pass
                      :description description)))

(defun fail (description)
  (run (make-instance 'fail
                      :description description)))

(defun run-test (name)
  (let ((test (find-test name)))
    (unless test
      (error "Test ~A is not defined" name))
    (call-with-test-result
     (lambda ()
       (run test)))))

(defun run-tests (&key package)
  (call-with-test-result
   (lambda ()
     (if package
         (mapc #'run (package-tests package))
         (mapc #'run (all-tests))))))
