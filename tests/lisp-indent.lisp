(in-package :lem-tests)

(defmacro define-indent-test (name before after)
  `(define-test ,name
     (run-indent-test ',name ,before ,after)))

(define-indent-test cond-1
"
(cond ((foo 1
2)))
"
"
(cond ((foo 1
            2)))
")

(define-indent-test defclass-1
  "
`(defclass foo ()
,(mapcar x
y))"
  "
`(defclass foo ()
   ,(mapcar x
            y))
")

(defun run-indent-test (name before-text after-text)
  (let ((buffer (lem:make-buffer (format nil "*indent-test ~A*" name)
                                 :syntax-table lem-lisp-syntax:*syntax-table*)))
    (setf (lem:variable-value 'lem:calc-indent-function :buffer buffer)
          'lem-lisp-syntax:calc-indent)
    (lem:erase-buffer buffer)
    (lem:with-point ((p (lem:buffer-point buffer)))
      (lem:insert-string p before-text))
    (lem:indent-buffer buffer)
    (test (string= after-text (lem:buffer-text buffer))
          (format nil "# error: ~A~%actual: ~S~%expected: ~S~%"
                  name
                  (lem:buffer-text buffer)
                  after-text))))
