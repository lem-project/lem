(in-package :lem-tests)

(defmacro define-indent-test (name before after)
  `(deftest ,name
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

(defun indent-line (p)
  (let ((indent (lem-lisp-syntax:calc-indent p)))
    (lem:back-to-indentation p)
    (lem:with-point ((start p))
      (lem:line-start start)
      (lem:delete-between-points start p))
    (lem:insert-string p (make-string indent :initial-element #\space))))

(defun indent-buffer (buffer)
  (lem:with-point ((p (lem:buffer-point buffer)))
    (lem:buffer-start p)
    (loop
      (indent-line p)
      (unless (lem:line-offset p 1)
        (return)))))

(defun run-indent-test (name before-text after-text)
  (let ((buffer (lem:make-buffer (format nil "*indent-test ~A*" name)
                                 :syntax-table lem-lisp-syntax:*syntax-table*)))
    (lem:erase-buffer buffer)
    (lem:with-point ((p (lem:buffer-point buffer)))
      (lem:insert-string p before-text))
    (indent-buffer buffer)
    (ok (string= after-text (lem:buffer-text buffer)))))
