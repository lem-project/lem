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

(defstruct capture-regex regex function)

(defclass detective-search () ())

(defclass search-regex (detective-search)
  ((function-regex :type (or capture-regex null)
                   :initform nil
                   :initarg :function-regex
                   :writer set-function-regex
                   :reader search-function-regex)
   (package-regex :type (or capture-regex null)
                  :initform nil
                  :initarg :package-regex
                  :writer set-package-regex
                  :reader search-package-regex)
   (class-regex :type (or capture-regex null)
                :initform nil
                :initarg :class-regex
                :writer set-class-regex
                :reader search-class-regex)
   (variable-regex :type (or capture-regex null)
                   :initform nil
                   :initarg :variable-regex
                   :writer set-variable-regex
                   :reader search-variable-regex )
   (misc-regex :type (or capture-regex null)
               :initform nil
               :initarg :misc-regex
               :writer set-misc-regex
               :reader search-misc-regex)))


(defun buffer-references (buffer)
  (buffer-value buffer 'references))

(defun (setf buffer-references) (value buffer)
  (setf (buffer-value buffer 'references) value))

(defgeneric search-references (search-class))


(defmethod search-references ((search search-regex))
  (labels ((find-ref (class regex)
             (with-point ((p (buffer-start-point (point-buffer (current-point)))))
               (loop :for position = (search-forward-regexp p regex)
                     :while position
                     :collect (make-instance class
                                             :reference-point (copy-point position))))))
  (with-slots (function-regex
               package-regex
               class-regex
               variable-regex
               misc-regex)
      search
    (let ((slots (list (cons (cons "functions" function-regex)'function-reference)
                       (cons (cons "classes" class-regex) 'class-reference)
                       (cons (cons "packages" package-regex) 'package-reference)
                       (cons (cons "variables" variable-regex) 'variable-reference)
                       (cons (cons "misc" misc-regex) 'misc-reference))))
    
    (setf (buffer-references (current-buffer))
          (make-hash-table :test 'equal))

      (loop :for ((id . regex) . class ) :in slots
            :when regex
            :do (setf (gethash id (buffer-references (current-buffer)))
                     (find-ref class regex)))))))

;;(or (and (str:starts-with-p "(setf" pname)
;;         (str:concat pname " " (third line)))
;;   pname)