(uiop:define-package :lem-lisp-mode/commands/call-defun
  (:use :cl
        :lem
        :lem-lisp-mode))
(in-package :lem-lisp-mode/commands/call-defun)

(define-key *lisp-mode-keymap* "C-c C-y" 'lisp-call-defun)

(defun define-form-name-p (operator-name)
  (or (member operator-name
              '("defun" "defmacro" "defmethod" "defgeneric" "defsetf")
              :test #'string-equal)
      (alexandria:starts-with-subseq "define-" operator-name)))

(defun parse-toplevel-form (point)
  (lisp-beginning-of-defun point 1)
  (scan-lists point 1 -1)
  (skip-whitespace-forward point)
  (let ((operator-name (symbol-string-at-point point)))
    (cond ((string-equal operator-name "defclass")
           (form-offset point 2)
           (let ((arg-name (symbol-string-at-point point)))
             (values arg-name :defclass)))
          ((string-equal operator-name "defstruct")
           (form-offset point 2)
           (let ((arg-name (symbol-string-at-point point)))
             (values arg-name :defstruct)))
          ((define-form-name-p operator-name)
           (form-offset point 1)
           (skip-whitespace-forward point)
           (cond ((looking-at point "\\(\\s*setf\\S*")
                  (scan-lists point 1 -1)
                  (form-offset point 1)
                  (skip-whitespace-forward point)
                  (values (symbol-string-at-point point) :defun-setf))
                 (t
                  (let ((arg-name (symbol-string-at-point point)))
                    (values arg-name :defun))))))))

(define-command lisp-call-defun () ()
  (with-point ((point (current-point)))
    (multiple-value-bind (name kind)
        (parse-toplevel-form point)
      (when name
        (ecase kind
          (:defun
              (send-string-to-listener (format nil "(~A )" name)
                                       :evaluate nil
                                       :focus t)
              (scan-lists (current-point) -1 -1))
          (:defun-setf
              (send-string-to-listener (format nil "(setf (~A ) )" name)
                                       :evaluate nil
                                       :focus t)
            (scan-lists (current-point) -1 -1))
          (:defstruct
              (send-string-to-listener (format nil "(make-~A)" name)
                                       :evaluate nil
                                       :focus t)
            (scan-lists (current-point) -1 -1))
          (:defclass
                (send-string-to-listener (format nil "(make-instance '~A)" name)
                                         :evaluate nil
                                         :focus t)
              (scan-lists (current-point) -1 -1)))))))
              
(define-command test () ()
  (message "~A" (parse-toplevel-form (current-point))))
