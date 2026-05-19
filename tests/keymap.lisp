(defpackage :lem-tests/keymap-test
  (:use :cl :rove)
  (:import-from :lem-core
                :make-keymap
                :make-prefix
                :keymap-add-prefix
                :make-key
                :traverse-keymap))
(in-package :lem-tests/keymap-test)

(defun make-test-keymap-with-closure-suffix ()
  "A keymap holding one normal symbol binding and one prefix whose
suffix is a closure -- the shape produced by transient toggle/choice
infixes."
  (let ((keymap (make-keymap :description 'test-keymap))
        (closure (lambda () 'transient-thunk)))
    (keymap-add-prefix keymap (make-prefix :key (make-key :sym "a")
                                           :suffix 'real-command))
    (keymap-add-prefix keymap (make-prefix :key (make-key :sym "b")
                                           :suffix closure))
    (values keymap closure)))

(deftest traverse-keymap-skips-non-symbol-suffixes
  (multiple-value-bind (keymap closure) (make-test-keymap-with-closure-suffix)
    (let ((yielded '()))
      (traverse-keymap keymap
                       (lambda (kseq cmd)
                         (declare (ignore kseq))
                         (push cmd yielded)))
      (ok (every #'symbolp yielded))
      (ok (member 'real-command yielded))
      (ng (member closure yielded)))))

#+sbcl
(deftest describe-bindings-internal-terminates
  (let ((keymap (make-test-keymap-with-closure-suffix))
        (output nil))
    (let ((result
            (handler-case
                (bordeaux-threads:with-timeout (5)
                  (setf output
                        (with-output-to-string (s)
                          (lem-core/commands/help::describe-bindings-internal
                           s "test" keymap t)))
                  :ok)
              (bordeaux-threads:timeout () :timed-out)
              (error (e) (cons :error e)))))
      (ok (eq result :ok))
      (ok (search "real-command" (or output ""))))))
