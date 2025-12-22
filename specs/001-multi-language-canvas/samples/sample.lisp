;;;; Sample Common Lisp file for testing Living Canvas multi-language support.
;;;;
;;;; This file contains various function definitions and call patterns
;;;; to test the call graph extraction via micros provider.

(defpackage #:living-canvas-sample
  (:use #:cl)
  (:export #:greet
           #:farewell
           #:process-name
           #:validate-input
           #:transform-data
           #:save-result
           #:process-pipeline
           #:calculator
           #:make-calculator
           #:calc-add
           #:calc-subtract
           #:calc-multiply
           #:calc-divide
           #:calc-reset
           #:calc-value
           #:use-calculator
           #:fibonacci
           #:factorial
           #:math-operations
           #:main))

(in-package #:living-canvas-sample)

;;; Greeting functions

(defun greet (name)
  "Return a greeting message."
  (format nil "Hello, ~A!" name))

(defun farewell (name)
  "Return a farewell message."
  (format nil "Goodbye, ~A!" name))

(defun process-name (name)
  "Process a name by greeting and then saying farewell."
  (let ((greeting (greet name))
        (goodbye (farewell name)))
    (format nil "~A ... ~A" greeting goodbye)))

;;; Data processing functions

(defun validate-input (data)
  "Validate input data."
  (and data
       (listp data)
       (not (null data))))

(defun transform-data (data)
  "Transform data into a new format."
  (when (validate-input data)
    (mapcar (lambda (pair)
              (cons (string-upcase (string (car pair)))
                    (cdr pair)))
            data)))

(defun save-result (result)
  "Save result to storage (mock)."
  (format t "Saving: ~A~%" result)
  t)

(defun process-pipeline (data)
  "Main processing pipeline."
  (unless (validate-input data)
    (format t "Invalid input~%")
    (return-from process-pipeline nil))
  (let ((transformed (transform-data data)))
    (when transformed
      (save-result transformed))
    transformed))

;;; Calculator using structures

(defstruct calculator
  "Simple calculator structure."
  (value 0 :type number))

(defun calc-add (calc x)
  "Add X to calculator value."
  (incf (calculator-value calc) x)
  calc)

(defun calc-subtract (calc x)
  "Subtract X from calculator value."
  (decf (calculator-value calc) x)
  calc)

(defun calc-multiply (calc x)
  "Multiply calculator value by X."
  (setf (calculator-value calc)
        (* (calculator-value calc) x))
  calc)

(defun calc-divide (calc x)
  "Divide calculator value by X."
  (unless (zerop x)
    (setf (calculator-value calc)
          (/ (calculator-value calc) x)))
  calc)

(defun calc-reset (calc)
  "Reset calculator value to zero."
  (setf (calculator-value calc) 0)
  calc)

(defun calc-value (calc)
  "Get calculator current value."
  (calculator-value calc))

(defun use-calculator ()
  "Demonstrate calculator usage."
  (let ((calc (make-calculator :value 10)))
    (calc-add calc 5)
    (calc-multiply calc 2)
    (calc-subtract calc 3)
    (calc-value calc)))

;;; Recursive functions

(defun fibonacci (n)
  "Calculate nth Fibonacci number recursively."
  (if (<= n 1)
      n
      (+ (fibonacci (- n 1))
         (fibonacci (- n 2)))))

(defun factorial (n)
  "Calculate factorial of N."
  (if (<= n 1)
      1
      (* n (factorial (- n 1)))))

(defun math-operations (x y)
  "Perform various math operations."
  (let ((fib-x (fibonacci x))
        (fact-y (factorial y)))
    (+ fib-x fact-y)))

;;; Higher-order functions

(defun apply-twice (f x)
  "Apply function F to X twice."
  (funcall f (funcall f x)))

(defun compose (f g)
  "Return a function that composes F and G."
  (lambda (x)
    (funcall f (funcall g x))))

(defun curry (f x)
  "Return a curried version of F with X as first argument."
  (lambda (y)
    (funcall f x y)))

;;; Generic functions (CLOS)

(defclass person ()
  ((name :initarg :name
         :accessor person-name
         :type string)
   (age :initarg :age
        :accessor person-age
        :type integer))
  (:documentation "A person with a name and age."))

(defgeneric greet-person (person)
  (:documentation "Generate a greeting for a person."))

(defmethod greet-person ((p person))
  "Greet a person by name."
  (greet (person-name p)))

(defgeneric describe-person (person)
  (:documentation "Describe a person."))

(defmethod describe-person ((p person))
  "Describe a person with name and age."
  (format nil "~A is ~D years old"
          (person-name p)
          (person-age p)))

(defun create-and-greet (name age)
  "Create a person and greet them."
  (let ((person (make-instance 'person :name name :age age)))
    (values (greet-person person)
            (describe-person person))))

;;; Main entry point

(defun main ()
  "Main entry point."
  ;; Test greeting functions
  (let ((message (process-name "World")))
    (format t "~A~%" message))

  ;; Test data pipeline
  (let* ((data '((name . "test") (value . 42)))
         (result (process-pipeline data)))
    (format t "Pipeline result: ~A~%" result))

  ;; Test calculator
  (let ((calc-result (use-calculator)))
    (format t "Calculator result: ~A~%" calc-result))

  ;; Test math operations
  (let ((math-result (math-operations 10 5)))
    (format t "Math result: ~A~%" math-result))

  ;; Test CLOS
  (multiple-value-bind (greeting description)
      (create-and-greet "Alice" 30)
    (format t "~A~%~A~%" greeting description)))

;;; Run main when loaded
;; (main)
