(defpackage :lem-scheme-syntax.indent
  (:use :cl :lem-base)
  (:export :set-indentation
           :calc-indent))
(in-package :lem-scheme-syntax.indent)

(defparameter *body-indent* 2)
(defparameter *max-depth* 4)

(defvar *static-indent-table* (make-hash-table :test 'equal))

(defvar *lambda-list-indentation* t)
(defvar *lambda-list-keyword-parameter-alignment* t)
(defvar *lambda-list-keyword-alignment* t)

(defun get-indentation (name)
  (gethash name *static-indent-table*))

(defun set-indentation (name method)
  (setf (gethash name *static-indent-table*) method))

(mapc (lambda (elt)
        (let ((name (car elt))
              (method (if (stringp (cdr elt))
                          (get-indentation (cdr elt))
                          (cadr elt))))
          (set-indentation name method)))
      '(;; added
        ;("use"         0)
        ("import"      0)
        ("cond"        (&rest (&whole 1 &rest 1)))
        ("cond-expand" . "cond")
        ;("set!"        1)
        ;("eq?"         0)
        ;("eqv?"        0)
        ;("equal?"      0)
        ;("and"         0)
        ;("or"          0)
        ;("not"         0)
        ;; part 1
        ("begin"       0)
        ("case"        (4 &rest (&whole 2 &rest 1)))
        ("define"      1)
        ("define-library" 1)
        ("define-record-type" 3)
        ("define-syntax" 1)
        ("define-values" 1)
        ("delay"       0)
        ("do"          2)
        ("export"      0)
        ("if"          1)
        ("lambda"      1)
        ("let"         ((&whole 4 &rest (&whole 1 2)) &body))
        ("let*" . "let")
        ("letrec" . "let")
        ("letrec*" . "let")
        ("library"     1)
        ("syntax-case" 2)
        ("with-input-from-file" 1)
        ("with-output-to-file" 1)
        ;; part 2
        ("define-class" 2)
        ("define-condition-type" 3)
        ("define-constant" 1)
        ("define-dict-interface" 1)
        ("define-in-module" 2)
        ("define-macro" 1)
        ("define-method" 2)
        ("define-module" 1)
        ("define-reader-ctor" 1)
        ("and-let1"    2)
        ("ecase" . "case")
        ("glet1"       2)
        ("hash-table" . "define")
        ("if-let1"     2)
        ("match-lambda" . "cond")
        ("match-lambda*" . "cond")
        ("match-let" . "let")
        ("match-let*" . "let")
        ("match-letrec" . "let")
        ("match-let1"  2)
        ("rlet1"       2)
        ("unwind-protect" 1)
        ;; part 3
        ("and-let*" . "let")
        ("begin0"      0)
        ("call-with-client-socket" 1)
        ("call-with-input-conversion" 1)
        ("call-with-input-file" 1)
        ("call-with-input-process" 1)
        ("call-with-input-string" 1)
        ("call-with-iterator" 1)
        ("call-with-output-conversion" 1)
        ("call-with-output-file" 1)
        ("call-with-output-string" 0)
        ("call-with-temporary-file" 1)
        ("call-with-values" 1)
        ("dolist"      1)
        ("dotimes"     1)
        ("if-match"    2)
        ("let*-values" . "let")
        ("let-args"    (4 (&whole 4 &rest (&whole 1 2)) &body))
        ("let-keywords*" . "let-args")
        ("let-match" . "let-args")
        ("let-optionals*" . "let-args")
        ("let-syntax" . "let")
        ("let-values" . "let")
        ("let/cc"      1)
        ("let1"        2)
        ("letrec-syntax" . "let")
        ("make"        1)
        ("multiple-value-bind" 2)
        ("match" . "case")
        ("parameterize" 1)
        ("parse-options" . "case")
        ("receive"     2)
        ("rxmatch-case" . "case")
        ("rxmatch-cond" . "cond")
        ("rxmatch-if"  2)
        ("rxmatch-let" . "let-args")
        ("syntax-rules" 1)
        ("unless"      1)
        ("until"       1)
        ("when"        1)
        ("while"       1)
        ("with-builder" 1)
        ("with-error-handler" 0)
        ("with-error-to-port" 1)
        ("with-input-conversion" 1)
        ("with-input-from-port" 1)
        ("with-input-from-process" 1)
        ("with-input-from-string" 1)
        ("with-iterator" 1)
        ("with-module" 1)
        ("with-output-conversion" 1)
        ("with-output-to-port" 1)
        ("with-output-to-process" 1)
        ("with-output-to-string" 0)
        ("with-port-locking" 1)
        ("with-string-io" 1)
        ("with-time-counter" 1)
        ("with-signal-handlers" 1)
        ("with-locking-mutex" 1)
        ("guard"       1)))

(defun lambda-list-keyword-p (name)
  (and (stringp name)
       (find name
             '("&optional" "&rest" "&key" "&allow-other-keys" "&aux"
               "&whole" "&body" "&environment")
             :test #'string-equal)))

(defun search-lambda-list-keyword (p)
  (loop
    (unless (form-offset p -1)
      (return nil))
    (when (lambda-list-keyword-p (symbol-string-at-point p))
      (return p))))

(defun compute-indent-lambda-list (path indent-point sexp-column)
  (declare (ignore path))
  (unless *lambda-list-indentation*
    (return-from compute-indent-lambda-list (1+ sexp-column)))
  (with-point ((p indent-point))
    (cond
      ((progn
         (back-to-indentation p)
         (lambda-list-keyword-p (symbol-string-at-point p)))
       (if *lambda-list-keyword-alignment*
           (if (search-lambda-list-keyword p)
               (point-column p)
               (1+ sexp-column))
           (1+ sexp-column)))
      (t
       (cond
         ((search-lambda-list-keyword p)
          (if *lambda-list-keyword-parameter-alignment*
              (if (looking-at p "[\\w&]+\\s*$")
                  (point-column p)
                  (+ 1 (point-column (form-offset p 1))))
              (+ 2 (point-column p))))
         (t
          (1+ sexp-column)))))))

(defun compute-indent-integer-method (method path indent-point sexp-column)
  (declare (ignore indent-point))
  (cond ((cdr path)
         'default-indent)
        ((<= (car path) method)
         (+ sexp-column 4))
        (t
         (+ sexp-column *body-indent*))))

(defun compute-indent-symbol-method (method path indent-point sexp-column)
  (funcall method path indent-point sexp-column))

(defun compute-indent-complex-method (method path indent-point sexp-column)
  (loop
    :named exit
    :for pathrest :on path
    :for n := (1- (car pathrest))
    :do (let ((restp nil))
          (loop
            (let ((method1 (car method)))
              (cond ((and restp
                          (not (or (consp method1)
                                   (and (symbolp method1)
                                        (not (member method1 '(&rest &body &whole &lambda)))))))
                     (return-from exit
                       'default-indent))
                    ((eq method1 '&body)
                     (return-from exit
                       (if (null (cdr pathrest))
                           (+ sexp-column *body-indent*)
                           'default-indent)))
                    ((eq method1 '&rest)
                     (setf restp (> n 0))
                     (setf n 0)
                     (pop method))
                    ((> n 0)
                     (decf n)
                     (pop method))
                    ((eq method1 'nil)
                     (return-from exit
                       'default-indent))
                    ((eq method1 '&lambda)
                     (return-from exit
                       (cond ((null (cdr pathrest))
                              (+ sexp-column 4))
                             (t
                              (compute-indent-lambda-list path indent-point sexp-column))
                             ;; ((null (cddr pathrest))
                             ;;  (compute-indent-lambda-list path indent-point sexp-column))
                             ;; (t
                             ;;  'default-indent)
                             )))
                    ((integerp method1)
                     (return-from exit
                       (if (null (cdr pathrest))
                           (+ sexp-column method1)
                           'default-indent)))
                    ((symbolp method1)
                     (return-from exit
                       (compute-indent-symbol-method method1 path indent-point sexp-column)))
                    ;; (&whole ...)
                    ((not (null (cdr pathrest)))
                     (setf method (cddr method1))
                     (return))
                    (t
                     (return-from exit
                       (let ((method1 (cadr method1)))
                         (cond (restp
                                'default-indent)
                               ((eq method1 'nil)
                                'default-indent)
                               ((integerp method1)
                                (+ sexp-column method1))
                               (t
                                (compute-indent-symbol-method
                                 method1 path indent-point sexp-column))))))))))))

(defun compute-indent-method (method path indent-point sexp-column)
  (funcall (etypecase method
             (integer #'compute-indent-integer-method)
             (symbol #'compute-indent-symbol-method)
             (list #'compute-indent-complex-method))
           method path indent-point sexp-column))

(defun quote-form-point-p (p)
  (and (eql (character-at p -1) #\')
       (not (eql (character-at p -2) #\#))))

(defun vector-form-point-p (p)
  (eql (character-at p -1) #\#))  

(defun find-indent-method (name path)
  (flet ((f (method)
           (when method
             (return-from find-indent-method method))))
    (f (get-indentation name))
    (let ((name1 (ppcre:scan-to-strings "(?<=:)[^:]+" name)))
      (when name1
        (f (get-indentation name1)))
      (f (and (null (cdr path))
              (ppcre:scan "^(?:with-|without-|within-|do-|def)" (or name1 name))
              '(&lambda &body))))))

(defun calc-function-indent (point)
  (loop
    (unless (form-offset point -1)
      (let ((charpos (point-charpos point)))
        (form-offset point 1)
        (skip-whitespace-forward point t)
        (when (or (eql #\; (character-at point))
                  (end-line-p point))
          (line-offset point 0 charpos)))
      (return))
    (let ((charpos (point-charpos point)))
      (back-to-indentation point)
      (when (= charpos (point-charpos point))
        (return))
      (line-offset point 0 charpos)))
  (point-column point))

(defun calc-indent-1 (indent-point)
  (let* ((const-flag nil)
         (innermost-sexp-column nil)
         (calculated
           (with-point ((p indent-point))
             (loop
               :named outer
               :with path := '() :and sexp-column
               :for innermost := t :then nil
               :repeat *max-depth*
               :do
               (loop :for n :from 0 :do
                     (when (and (< 0 n) (start-line-p p))
                       (return-from outer nil))
                     (unless (form-offset p -1)
                       (push n path)
                       (return)))
               (when (and (null (cdr path))
                          (= 0 (car path))
                          (scan-lists p -1 1 t))
                 (return-from outer (1+ (point-column p))))
               (when (and innermost
                          (or (member (character-at p 0) '(#\: #\"))
                              (looking-at p "#!?[+-]")))
                 (setf const-flag t))
               (let ((name (string-downcase (symbol-string-at-point p))))
                 (unless (scan-lists p -1 1 t)
                   (return-from outer 'default-indent))
                 (unless sexp-column (setf sexp-column (point-column p)))
                 (when (or (quote-form-point-p p)
                           (vector-form-point-p p))
                   (return-from outer (1+ sexp-column)))
                 (when innermost
                   (setf innermost-sexp-column sexp-column))
                 (let ((method (find-indent-method name path)))
                   (when method
                     (return-from outer (compute-indent-method method
                                                               path
                                                               indent-point
                                                               sexp-column)))))))))
    (if (or (null calculated)
            (eq calculated 'default-indent))
        (if (and const-flag innermost-sexp-column)
            (1+ innermost-sexp-column)
            (calc-function-indent indent-point))
        calculated)))

(defun calc-indent (point)
  (line-start point)
  (lem-base::with-point-syntax point
    (let ((state (syntax-ppss point)))
      (cond
        ((pps-state-string-p state) nil)
        ((zerop (pps-state-paren-depth state))
         0)
        (t (calc-indent-1 point))))))
