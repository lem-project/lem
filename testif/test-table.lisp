(in-package :testif)

(defvar *tests-per-package* (make-hash-table))

(defun register-test (test)
  (let ((id (test-id test)))
    (remove-test id)
    (push test (gethash (identifier-package id) *tests-per-package*))
    (values)))

(defun find-test (name)
  (let ((id (ensure-identifier name)))
    (find id
          (gethash (identifier-package id) *tests-per-package*)
          :key #'test-id
          :test #'identifier-equal)))

(defun remove-test (name)
  (let ((id (ensure-identifier name)))
    (deletef (gethash (identifier-package id) *tests-per-package*)
             id
             :key #'test-id
             :test #'identifier-equal))
  (values))

(defun ensure-package (package-designator)
  (let ((package (find-package package-designator)))
    (unless package
      (error "The name ~A does not designate any package." package-designator))
    package))

(defun remove-package-tests (package-designator)
  (remhash (ensure-package package-designator)
           *tests-per-package*))

(defun package-tests (package-designator)
  (gethash (ensure-package package-designator)
           *tests-per-package*))

(defun all-tests ()
  (loop :for tests :in (hash-table-values *tests-per-package*)
        :append tests))
