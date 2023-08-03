(defpackage :lem/detective
  (:use :cl :lem)
  (:export

   :reference
   :reference-name
   :reference-point

   :function-reference
   :function-arguments

   :package-reference
   :package-use
   :package-export

   :class-reference
   :class-parents
   :class-attributes

   :variable-reference
   :variable-reference-value

   :misc-reference
   :misc-custom-type

   :capture-regex
   :make-capture-regex

   :detective-all
   :detective-class
   :detective-function
   :detective-next
   :detective-package
   :detective-previous
   :detective-variable

   :search-regex

   :search-function-regex
   :set-function-regex
   :search-package-regex
   :set-package-regex
   :search-class-regex
   :set-class-regex
   :search-variable-regex
   :set-variable-regex
   :search-misc-regex
   :set-misc-regex

   :buffer-references

   :search-references

   :capture-references

   :navigate-reference

   :move-to-reference)
  
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

(defclass <detective-search> () ())

(defclass search-regex (<detective-search>)
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
  (labels ((find-ref (regex class)
             (with-point ((p (buffer-start-point (point-buffer (current-point)))))
               (loop :for point = (search-forward-regexp p (capture-regex-regex regex))
                     :while point
                     :collect (funcall (capture-regex-function regex) 
                                       (copy-point point :temporary)
                                       class)))))
    (with-accessors ((function-regex search-function-regex)
                     (package-regex search-package-regex)
                     (class-regex search-class-regex)
                     (variable-regex search-variable-regex)
                     (misc-regex search-misc-regex))
        search
      (let ((slots
              (list (cons (cons "functions" function-regex):function-reference)
                    (cons (cons "classes" class-regex) :class-reference)
                    (cons (cons "packages" package-regex) :package-reference)
                    (cons (cons "variables" variable-regex) :variable-reference)
                    (cons (cons "misc" misc-regex) :misc-reference))))
    
        (setf (buffer-references (current-buffer))
              (make-hash-table :test 'equal))

        (loop :for ((id . regex) . class) :in slots
              :when (and regex (capture-regex-function regex))
              :do (setf (gethash id (buffer-references (current-buffer)))
                        (find-ref regex class)))))))

(defgeneric capture-reference (point class))

(defun %get-reference (references)
  (alexandria:when-let* ((name-references (mapcar #'reference-name references))
                         (item
                          (prompt-for-string "Navigate: "
                                             :completion-function (lambda (x) (completion-strings x name-references))

                                             :test-function (lambda (name)
                                                              (member name name-references :test #'string=)))))
    (find item references :key #'reference-name :test #'string=)))

(defgeneric navigate-reference (references))

(defmethod navigate-reference ((type String))
  (alexandria:when-let ((references (gethash type (buffer-references (current-buffer)))))
    (%get-reference references)))

(defmethod navigate-reference ((references List))
  (%get-reference references))

(defgeneric move-to-reference (reference))

(defmethod move-to-reference ((reference reference))
  (let ((location (reference-point reference)))
    (move-point (current-point) location)
    (lem/peek-source:highlight-matched-line location)))

(defmethod move-to-reference (reference)
  (message "Not reference available in current buffer."))

(defun check-change ()
  (cond 
   ((null (buffer-references (current-buffer)))
    (search-references 
     (variable-value 'lem/language-mode:detective-search :buffer)))

   ((not (eql (buffer-value (current-buffer) 'prev-tick)
              (buffer-modified-tick (current-buffer))))

    (setf (buffer-value (current-buffer) 'prev-tick)
          (buffer-modified-tick (current-buffer)))
    (search-references (variable-value 'lem/language-mode:detective-search :buffer)))))

(defun %detective-move (&key (direction :up))
  (let* ((references (buffer-references (current-buffer)))
         (lreferences (alexandria:flatten(alexandria:hash-table-values references)))
         (reference-points (mapcar #'reference-point  lreferences))
         (closest (point-closest (current-point)
                                           reference-points
                                           :direction direction )))
    (if closest
        (move-to-reference
         (find closest lreferences :key #'reference-point :test #'point=))
        (message "No found reference."))))

(define-command detective-next () ()
  "Move point to the next reference."
  (check-change)
  (%detective-move :direction :down))

(define-command detective-previous () ()
  "Move point to the previous reference."
  (check-change)
  (%detective-move :direction :up))

(define-command detective-function () ()
  "Prompt for a function defined in this buffer and move to it."
  (check-change)
  (let ((reference (navigate-reference "functions")))
    (move-to-reference reference)))

(define-command detective-class () ()
  "Prompt for a class defined in this buffer and move to it."
  (check-change)
  (let ((reference (navigate-reference "classes")))
    (move-to-reference reference)))

(define-command detective-package () ()
  "Prompt for a package defined or used in this buffer and move to it."
  (check-change)
  (let ((reference (navigate-reference "packages")))
    (move-to-reference reference)))

(define-command detective-variable () ()
  "Prompt for a variable defined in this buffer and move to it."
  (check-change)
  (let ((reference (navigate-reference "variables")))
    (move-to-reference reference)))

(define-command detective-all () ()
  "Prompt for a definition defined in this buffer and move the point to it."
  (check-change)
  (let* ((references
          (alexandria:flatten
           (alexandria:hash-table-values (buffer-references (current-buffer)))))
        (reference (navigate-reference references)))
     (move-to-reference reference)))
