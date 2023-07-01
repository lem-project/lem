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

   :detective-search

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
  (labels ((find-ref (regex class)
             (with-point ((p (buffer-start-point (point-buffer (current-point)))))
               (loop :for position = (search-forward-regexp p (capture-regex-regex regex))
                     :while position
                     :collect (funcall (capture-regex-function regex) 
                                       (copy-point position)
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

(defgeneric capture-reference (position class))

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
    (move-point (current-point) location)))

(defmethod move-to-reference (reference)
  (message "Not reference available in current buffer."))

(defun check-change ()
  (cond 
   ((null (buffer-references (current-buffer)))
    (search-references 
     (variable-value 'lem/language-mode:detective-search :buffer)))
   
   ((changed-disk-p (current-buffer))
    (search-references (variable-value 'lem/language-mode:detective-search :buffer)))))

(define-command detective-next () ()
  (check-change)
  (let* ((references (buffer-references (current-buffer)))
         (lreferences (alexandria:hash-table-values references))
         closest )
    (loop :for ref :in (alexandria:flatten lreferences)
          :for flag = t then nil
          :with cline = (+ (line-number-at-point (current-point)) 1)
          :for rline = (line-number-at-point (reference-point ref))
          :do (progn
                (when flag (setf closest ref))
                (let* ((cdiff (- (line-number-at-point (reference-point closest)) cline))
                       (ldiff (- rline cline)))

                  (when  (or (and (> rline cline)
                                  (<  ldiff cdiff))
                             (< cdiff 0))
                    (setf closest ref)))))

    (if (< (- (+ (line-number-at-point (current-point)) 1)
              (line-number-at-point (reference-point closest))) 0)
        (move-to-reference closest)
        (message "No next reference."))))


(define-command detective-prev () ()
  (check-change)
  (let* ((references (buffer-references (current-buffer)))
         (lreferences (alexandria:hash-table-values references))
         closest )
    (loop :for ref :in (alexandria:flatten lreferences)
          :for flag = t then nil
          :with cline = (- (line-number-at-point (current-point)) 1)
          :for rline = (line-number-at-point (reference-point ref))
          :do (progn
                (when flag (setf closest ref))
                (let* ((cdiff (- (line-number-at-point (reference-point closest)) cline))
                       (ldiff (- rline cline)))

                  (when  (or (and (< rline cline)
                                  (>  ldiff cdiff))
                             (> cdiff 0))
                    (setf closest ref)))))

    (if (> (- (- (line-number-at-point (current-point)) 1)
              (line-number-at-point (reference-point closest))) 0)
        (move-to-reference closest)
        (message "No previous reference."))))

(define-command detective-function () ()
  (check-change)
  (let ((reference (navigate-reference "functions")))
    (move-to-reference reference)))

(define-command detective-class () ()
  (check-change)
  (let ((reference (navigate-reference "classes")))
    (move-to-reference reference)))

(define-command detective-package () ()
  (check-change)
  (let ((reference (navigate-reference "packages")))
    (move-to-reference reference)))

(define-command detective-variable () ()
  (check-change)
  (let ((reference (navigate-reference "variables")))
    (move-to-reference reference)))

(define-command detective-all () ()
  (check-change)
  (let* ((references
          (alexandria:flatten
           (alexandria:hash-table-values (buffer-references (current-buffer)))))
        (reference (navigate-reference references)))
     (move-to-reference reference)))

