(defpackage :lem-lisp-mode/detective
  (:use :cl :lem)
  (:export :capture-reference))

(in-package :lem-lisp-mode/detective)

(defun %default-capture (class position)
  (let* ((line (str:split #\Space (line-string position)))
         (name (remove #\) (second line))))
    (make-instance class
                   :reference-name name
                   :reference-point position)))

(defmethod capture-reference ((position lem:point) (class (eql :function-reference)))
  (let* ((line (str:split #\Space (line-string position)))
         (pname (second line))
         (name (or (and (str:starts-with-p "(setf" pname)
                        (str:concat pname " " (third line)))
                   pname)))
    (make-instance 'lem/detective:function-reference
                   :reference-name name
                   :reference-point position)))

(defmethod capture-reference ((position lem:point) (class (eql :class-reference)))
  (%default-capture 'lem/detective:class-reference position))

(defmethod capture-reference ((position lem:point) (class (eql :variable-reference)))
  (%default-capture 'lem/detective:variable-reference position))

(defmethod capture-reference ((position lem:point) (class (eql :package-reference)))
  (%default-capture 'lem/detective:package-reference position))

(defmethod capture-reference ((position lem:point) (class (eql :misc-reference)))
  (let* ((line (str:split #\Space (line-string position)))
         (type (str:replace-all "(" "" (first line)))
         (name (remove #\) (second line))))
    (make-instance 'lem/detective:misc-reference
                   :misc-custom-type type
                   :reference-name name
                   :reference-point position)))
