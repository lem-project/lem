(defpackage :lem-lisp-syntax.indent
  (:use :cl :lem-base))
(in-package :lem-lisp-syntax.indent)

(in-package :lem-user)

(defparameter *body-indent* 2)
(defvar *max-depth* 4)
(defvar *indent-table* (make-hash-table :test 'equal))

(defun get-indentation (name)
  (gethash name *indent-table*))

(defun set-indentation (name method)
  (setf (gethash name *indent-table*) method))

(mapc (lambda (elt)
        (let ((name (string-downcase (car elt)))
              (method (if (symbolp (cdr elt))
                          (get-indentation (string-downcase (cdr elt)))
                          (cadr elt))))
          (set-indentation name method)))
      '((block 1)
        (case        (4 &rest (&whole 2 &rest 1)))
        (ccase . case)
        (ecase . case)
        (typecase . case)
        (etypecase . case)
        (ctypecase . case)
        (catch 1)
        (cond        (&rest (&whole 2 &rest 1)))
        (defvar      (4 2 2))
        (defclass    (6 4 (&whole 2 &rest 1) (&whole 2 &rest 1)))
        (defconstant . defvar)
        (defcustom   (4 2 2 2))
        (defparameter . defvar)
        (defconst     . defcustom)
        (define-condition  . defclass)
        (define-modify-macro (4 &lambda &body))
        (defsetf     (4 &lambda 4 &body))
        (defun       (4 &lambda &body))
        (defgeneric  (4 &lambda &body))
        (define-setf-method . defun)
        (define-setf-expander . defun)
        (defmacro . defun)
        (defsubst . defun)
        (deftype . defun)
        (defmethodlisp-indent-defmethod)
        (defpackage  (4 2))
        (defstruct   ((&whole 4 &rest (&whole 2 &rest 1))
                      &rest (&whole 2 &rest 1)))
        (destructuring-bind
            ((&whole 6 &rest 1) 4 &body))
        (do          lisp-indent-do)
        (do* . do)
        (dolist      ((&whole 4 2 1) &body))
        (dotimes . dolist)
        (eval-when   1)
        (flet        ((&whole 4 &rest (&whole 1 &lambda &body)) &body))
        (labels . flet)
        (macrolet . flet)
        (generic-flet . flet)
        (generic-labels . flet)
        (handler-case (4 &rest (&whole 2 &lambda &body)))
        (restart-case . handler-case)
        ;; `else-body' style
        (if          (nil nil &body))
        ;; single-else style (then and else equally indented)
        (if          (&rest nil))
        (lambda      (&lambda &rest lisp-indent-function-lambda-hack))
        (let         ((&whole 4 &rest (&whole 1 1 2)) &body))
        (let* . let)
        (compiler-let . let) ;barf
        (handler-bind . let)
        (restart-bind . let)
        (locally 1)
        ;(loop         lisp-indent-loop)
        (:method (&lambda &body)) ; in `defgeneric'
        (multiple-value-bind ((&whole 6 &rest 1) 4 &body))
        (multiple-value-call (4 &body))
        (multiple-value-prog1 1)
        (multiple-value-setq (4 2))
        (multiple-value-setf . multiple-value-setq)
        (pprint-logical-block (4 2))
        (print-unreadable-object ((&whole 4 1 &rest 1) &body))
        ;; Combines the worst features of BLOCK, LET and TAGBODY
        (prog        (&lambda &rest lisp-indent-tagbody))
        (prog* . prog)
        (prog1 1)
        (prog2 2)
        (progn 0)
        (progv       (4 4 &body))
        (return 0)
        (return-from (nil &body))
        (symbol-macrolet . let)
        (tagbody     lisp-indent-tagbody)
        (throw 1)
        (unless 1)
        (unwind-protect (5 &body))
        (when 1)
        (with-accessors . multiple-value-bind)
        (with-condition-restarts . multiple-value-bind)
        (with-compilation-unit (&lambda &body))
        (with-output-to-string (4 2))
        (with-slots . multiple-value-bind)
        (with-standard-io-syntax (2))))

(defun lisp-indent-do (path indent-point sexp-column)
  (declare (ignore path indent-point sexp-column))
  'default-indent)

(defun lisp-indent-function-lambda-hack (path indent-point sexp-column)
  (declare (ignore path indent-point sexp-column))
  'default-indent)

(defun lisp-indent-tagbody (path indent-point sexp-column)
  (declare (ignore path indent-point sexp-column))
  'default-indent)

(defun compute-indent-lambda-list (path indent-point sexp-column)
  (declare (ignore path indent-point sexp-column))
  'default-indent)

(defun compute-indent-integer-method (method path indent-point sexp-column)
  (declare (ignore indent-point))
  (cond ((cdr path)
         'default-indent)
        ((<= (car path) method)
         (+ sexp-column 4))
        (t
         (+ sexp-column *body-indent*))))

(defun compute-indent-complex-method (method path indent-point sexp-column)
  (loop :named exit
        :for pathrest :on path
        :for n := (car pathrest)
        :do (let ((restp nil))
              (loop :repeat n
                    :while method
                    :do (case (pop method)
                          ((&body)
                           (return-from exit
                             (if (null pathrest)
                                 (+ sexp-column *body-indent*)
                                 'default-indent)))
                          ((&rest)
                           (setf restp t)
                           (return))))
              (let ((method1 (car method)))
                (cond ((eq method1 'nil)
                       (return-from exit 'default-indent))
                      ((eq method1 '&lambda)
                       (return-from exit
                         (cond ((null pathrest)
                                (+ sexp-column 4))
                               ((null (cdr pathrest))
                                (compute-indent-lambda-list path indent-point sexp-column))
                               (t
                                'default-indent))))
                      ((integerp method1)
                       (return-from exit
                         (if (null pathrest)
                             (+ sexp-column method1)
                             'default-indent)))
                      ((symbolp method1)
                       (return-from exit
                         (funcall method1 path indent-point sexp-column)))
                      (t
                       (if pathrest
                           (setf method (cddr method1))
                           (return-from exit
                             (let ((method1 (cadr method1)))
                               (cond (restp
                                      'default-indent)
                                     ((eq method1 'nil)
                                      'default-indent)
                                     ((integerp method1)
                                      (+ sexp-column method1))
                                     (t
                                      (call-indent-function method1))))))))))))

(defun compute-indent-method (method path indent-point sexp-column)
  (funcall (if (integerp method)
               #'compute-indent-integer-method
               #'compute-indent-complex-method)
           method path indent-point sexp-column))

(defun count-arg-until-car (p)
  (loop :for n :from 0
        :do
        (when (and (< 0 n) (start-line-p p)) (throw 'drop-out nil))
        (unless (form-offset p -1) (return n))))

(defun quote-form-point-p (p)
  (and (eql (character-at p -1) #\')
       (not (eql (character-at p -2) #\#))))

(defun vector-form-point-p (p)
  (eql (character-at p -1) #\#))  

(defun find-indent-method (name path)
  (flet ((f (name)
           (let ((method (get-indentation name)))
             (when method
               (return-from find-indent-method method)))))
    (f name)
    (let ((name1 (ppcre:scan-to-strings "(?<=:)[^:]+" name)))
      (when name1
        (setq name name1)
        (f name)))
    (when (and (null (cdr path))
               (ppcre:scan "^(?:with|do)" name))
      (return-from find-indent-method '(&lambda &body)))))

(defun calc-default-indent (point)
  (loop :while (form-offset point -1))
  (with-point ((car-point point))
    (form-offset point 1)
    (skip-space-and-comment-forward point)
    (if (same-line-p car-point point)
        (point-column point)
        (point-column car-point))))

(defun calc-indent-1 (indent-point)
  (let ((calculated
         (with-point ((p indent-point))
           (loop :with path := '()
                 :repeat *max-depth*
                 :do
                 (push (count-arg-until-car p) path)
                 (let ((name (string-downcase (symbol-string-at-point p))))
                   (scan-lists p -1 1)
                   (let ((sexp-column (point-column p)))
                     (when (or (quote-form-point-p p)
                               (vector-form-point-p p))
                       (return (1+ sexp-column)))
                     (let ((method (find-indent-method name path)))
                       (when method
                         (return (compute-indent-method method
                                                        path
                                                        indent-point
                                                        sexp-column))))))))))
    (if (or (null calculated)
            (eq calculated 'default-indent))
        (calc-default-indent indent-point)
        calculated)))

(defun calc-indent (point)
  (line-start point)
  (if (in-string-p point)
      nil
      (catch 'drop-out
        (calc-indent-1 point))))
