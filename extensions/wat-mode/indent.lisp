(defpackage :lem-wat-mode/indent
  (:use :cl :lem :lem/language-mode)
  (:import-from :lem-lisp-syntax.indent
                :calc-function-indent
                :compute-indent-method
                :default-indent)
  (:export :calc-indent))
(in-package :lem-wat-mode/indent)

(defparameter *body-indent* 2)
(defparameter *max-depth* 4)

(defvar *static-indent-table* (make-hash-table :test 'equal))

(defun get-indentation (name)
  "Get indentation rule for NAME."
  (gethash name *static-indent-table*))

(defun set-indentation (name method)
  "Set indentation rule METHOD for NAME."
  (setf (gethash name *static-indent-table*) method))

(defparameter *wat-indentation-rules*
  '(;; Module structure
    ("module" 1)
    ("func" 1)
    ("type" 1)
    ("import" 1)
    ("export" 1)
    ("memory" 1)
    ("table" 1)
    ("global" 1)
    ("data" 1)
    ("elem" 1)
    ("start" 1)
    ;; Function body
    ("param" 0)
    ("result" 0)
    ("local" 0)
    ;; Control flow
    ("block" 1)
    ("loop" 1)
    ("if" 1)
    ("then" 1)
    ("else" 1)
    ;; WAST assertions
    ("assert_return" 1)
    ("assert_trap" 1)
    ("assert_exhaustion" 1)
    ("assert_malformed" 1)
    ("assert_invalid" 1)
    ("assert_unlinkable" 1)
    ("invoke" 1)))

(defun load-indentation-rules ()
  "Load WAT indentation rules into the table."
  (loop :for (name method) :in *wat-indentation-rules*
        :do (set-indentation name method)))

(defun find-indent-method (name)
  "Find indentation method for NAME."
  (get-indentation name))

(defun calc-indent-1 (indent-point)
  "Calculate indentation for INDENT-POINT within an S-expression."
  (with-point ((p indent-point))
    (loop :with path := '()
          :repeat *max-depth*
          :do
             ;; Count forms at current level
             (loop :for n :from 0 :do
                      (when (start-line-p p)
                        (return-from calc-indent-1 nil))
                      (unless (form-offset p -1)
                        (push n path)
                        (return)))
             ;; Check if we're at the first position after opening paren
             (when (and (null (cdr path))
                        (= 0 (car path))
                        (scan-lists p -1 1 t))
               (return-from calc-indent-1 (1+ (point-column p))))
             ;; Get the name of the current form
             (let ((name (string-downcase (or (symbol-string-at-point p) ""))))
               (unless (scan-lists p -1 1 t)
                 (return-from calc-indent-1 'default-indent))
               (let ((sexp-column (point-column p))
                     (method (find-indent-method name)))
                 (when method
                   (return-from calc-indent-1
                     (cond ((integerp method)
                            (+ sexp-column 1 method))
                           (t
                            (compute-indent-method method
                                                   path
                                                   indent-point
                                                   sexp-column))))))))))

(defun calc-indent (point)
  "Calculate indentation for POINT in WAT mode."
  (line-start point)
  (with-point-syntax point
    (let ((state (syntax-ppss point)))
      (cond
        ;; Inside a string - no indentation
        ((pps-state-string-p state) nil)
        ;; Top level - no indentation
        ((zerop (pps-state-paren-depth state)) 0)
        ;; Inside parens - calculate
        (t (let ((calculated (calc-indent-1 point)))
             (if (or (null calculated)
                     (eq calculated 'default-indent))
                 (calc-function-indent point)
                 calculated)))))))

;; Initialize indentation rules
(load-indentation-rules)
