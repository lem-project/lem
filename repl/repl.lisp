;; -*- mode: lisp; package: repl -*-

(in-package :repl)

(rl:register-function :complete #'(lambda (text start end)
                                    (declare (ignore start end))
                                    (symbol-complete text)))

(defun add-history (str)
  (cffi:foreign-funcall "add_history"
                        :string str
                        :void))

(defun add-history-expr (x)
  (add-history (string-downcase (prin1-to-string x))))

(defun readline-read (prompt)
  (let ((line (rl:readline :prompt prompt)))
    (loop
      :with x :and pos
      :for error-p := nil :do
      (handler-case (setf (values x pos)
                          (read-from-string line nil))
        (error () (setq error-p t)))
      (cond (error-p
             (setq line
                   (concatenate 'string line " "
                                (rl:readline :already-prompted t))))
            (t
             (add-history-expr x)
             (return x))))))

(let (values)
  (defun eval-print (-)
    (setq values
          (multiple-value-list (eval -)))
    (setq +++ ++ /// //     *** (car ///)
          ++  +  //  /      **  (car //)
          +   -  /   values *   (car /))
    (mapc #'pprint values)
    (terpri))
  (defun repl ()
    (loop
      (restart-case (eval-print
                     (setq - (readline-read
                              (format nil "~&~a> "
                                      (package-name *package*)))))
        (restart-toplevel () :report "Restart toplevel.")))))
