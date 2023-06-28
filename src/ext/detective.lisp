(defpackage :lem/detective
  (:use :cl :lem)
  (:export)
  
  #+sbcl
  (:lock t))

(in-package :lem/detective)

(defclass reference ()
  ((name :type string 
         :initarg :reference-name
         :reader reference-name)

   (point :initarg :reference-point
          :reader reference-point)))

(defclass function-reference (reference)
  ((arguments :type list
              :initarg :function-arguments
              :reader function-arguments)))

(defclass package-reference (reference)
  ((use :type list
        :initarg :package-use
        :reader package-use )
   (export :type list
           :initarg :package-export
           :reader package-export)))

(defclass class-reference (reference)
  ((parents :type list
            :initarg :class-parents
            :reader class-parents)
   (attributes :type list
               :initarg :class-attributes
               :reader class-attributes)))

(defclass variable-reference (reference)
  ((value :initarg :variable-reference-value
          :reader variable-reference-value)))


(defclass misc-reference (reference)
  ((custom-type :initarg :misc-custom-type
                :type string
                :reader misc-custom-type)))


(defclass imenu-search () ())

(defclass search-regex (imenu-search)
  ((function-regex :initform nil
                   :initarg :function-regex
                   :reader search-function-regex)
   (package-regex :initform nil
                  :initarg :package-regex
                  :reader search-package-regex)
   (class-regex :initform nil
                :initarg :class-regex
                :reader search-class-regex)
   (variable-regex :initform nil
                   :initarg :variable-regex
                   :reader search-variable-regex )
   (misc-regex :initform nil
               :initarg :misc-regex
               :reader search-misc-regex)))


(defun buffer-references (buffer)
  (buffer-value buffer 'references))

(defun (setf buffer-references) (value buffer)
  (setf (buffer-value buffer 'references) value))


(defmethod search-references ((search search-regex))
  (with-slots (function-regex
               package-regex
               class-regex
               variable-regex
               misc-regex)
      search
    
    (setf (buffer-references (current-buffer))
          (make-hash-table :test 'equal))

    (when function-regex
      (setf 
       (gethash "functions" (buffer-references (current-buffer)))
       
       (with-point ((p (buffer-start-point (point-buffer (current-point)))))
         (loop :for position = (search-forward-regexp p function-regex)
               :while position
               :for line = (str:split #\Space (line-string position))
               :for pname = (second line)
               :for name = (or (and (str:starts-with-p "(setf" pname)
                                    (str:concat pname " " (third line)))
                               pname)
               :collect (make-instance 'function-reference
                                       :reference-point (copy-point position) 
                                       :reference-name name))))) 
    
    (when package-regex
      (setf 
       (gethash "packages" (buffer-references (current-buffer)))
      
       (with-point ((p (buffer-start-point (point-buffer (current-point)))))
         (loop :for position = (search-forward-regexp p package-regex)
               :while position
               :for line = (str:split #\Space (line-string position))
               :collect (make-instance 'package-reference
                                       :reference-point (copy-point position)
                                       :reference-name (second line))))))

    (when class-regex
      (setf
       (gethash "classes" (buffer-references (current-buffer)))
       
       (with-point ((p (buffer-start-point (point-buffer (current-point)))))
         (loop :for position = (search-forward-regexp p class-regex)
               :while position
               :for line = (str:split #\Space (line-string position))
               :collect (make-instance 'package-reference
                                       :reference-point (copy-point position)
                                       :reference-name (second line))))))
    (when variable-regex
      (setf

       (gethash "variables" (buffer-references (current-buffer)))
       (with-point ((p (buffer-start-point (point-buffer (current-point)))))
         (loop :for position = (search-forward-regexp p variable-regex)
               :while position
               :for line = (str:split #\Space (line-string position))
               :collect (make-instance 'package-reference
                                       :reference-point (copy-point position)
                                       :reference-name (second line))))))))