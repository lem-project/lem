;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Customization.html

(defpackage :lem/settings
  (:use :cl :lem-core)
  (:export #:customize-variable
           #:customize-group
           #:defcustom
           #:defgroup
           #:set-variable
           #:find-variable
           #:find-group))

(in-package :lem/settings)

(defvar *custom-vars* (make-hash-table)
  "map of customization variables.")

(defvar *custom-groups* (make-hash-table)
  "map of customization groups.")

(defclass custom-variable ()
  ((name :initarg :name
         :accessor variable-name
         :type symbol
         :initform (error "Provide the variable name")
         :documentation "The variable name")
   (type :initarg :type
         :accessor variable-type
         :initform (error "Provide the variable type")
         :documentation "The variable type")
   (default :initarg :default
     :accessor variable-default
     :initform (error "Provide the variable default value")
     :documentation "The variable default value")
   (group :initarg :group
          :accessor group-of
          :type symbol
          :initform (error "Provide the group for the variable")
          :documentation "The group the variable belongs to")
   (documentation :initarg :documentation
                  :accessor documentation-of
                  :type (or string null)
                  :initform nil
                  :documentation "The variable documentation")))

(defclass custom-group ()
  ((name :initarg :name
         :accessor group-name
         :type symbol
         :initform (error "Provide the group name")
         :documentation "The group name")
   (group :initarg :group
          :accessor group-of
          :type (or null symbol)
          :initform nil
          :documentation "The parent group of this group. If it is NIL, then this is a top-level group.")
   (documentation :initarg :documentation
                  :accessor documentation-of
                  :type (or string null)
                  :initform nil
                  :documentation "The group documentation")))

;; TODO: perhaps rethink this implementation: variables and groups could form a graph with
;; actual references stored in the CLOS object slots (parent, group, members, etc).
(defun group-variables (group)
  (remove-if-not (lambda (var)
                   (eql (group-of var) (group-name group)))
                 (alexandria:hash-table-values *custom-vars*)))

;; TODO: perhaps rethink this implementation: variables and groups could form a graph with
;; actual references stored in the CLOS object slots (parent, group, members, etc).
(defun subgroups (group)
  (remove-if-not (lambda (g)
                   (eql (group-of g) (group-name group)))
                 (alexandria:hash-table-values *custom-groups*)))

(defmacro defcustom (symbol default doc &rest args)
  "Define customization variable."
  (let ((vartype (or (getf args :type)
                     (error "Provide the type"))))
    `(progn
       (defvar ,symbol ,default ,doc)
       (check-type ,symbol ,vartype)
       (setf (gethash ',symbol *custom-vars*)
             (make-instance 'custom-variable
                            :name ',symbol
                            :type ',vartype
                            :group ',(getf args :group)
                            :default ,default
                            :documentation ,doc))
       (gethash ',symbol *custom-vars*))))

(defmacro defgroup (symbol members doc &rest args)
  `(progn
     (setf (gethash ',symbol *custom-groups*)
           (make-instance 'custom-group
                          :name ',symbol
                          :documentation ,doc
                          ,@args))
     ,@(loop for member in members
             collect `(defcustom ,@member))))

(defun find-variable (name &optional (error-p t))
  (or (gethash name *custom-vars*)
      (when error-p
        (error "Customization variable not found: ~s" name))))

(defun find-group (name &optional (error-p t))
  (or (gethash name *custom-groups*)
      (when error-p
        (error "Custom group not found: ~s" name))))

(defgeneric %prompt-for-type-instance (type-discriminator type-arguments prompt &rest args))

(defmethod %prompt-for-type-instance ((type (eql 'string)) type-args prompt &rest args)
  (lem-core::prompt-for-string prompt :initial-value (getf args :initial-value)))

(defmethod %prompt-for-type-instance ((type (eql 'integer)) type-args prompt &rest args)
  (lem-core::prompt-for-integer prompt :initial-value (getf args :initial-value)))

(defmethod %prompt-for-type-instance ((type (eql 'boolean)) type-args prompt &rest args)
  (declare (ignore args))
  (lem-core::prompt-for-y-or-n-p prompt))

(defun prompt-for-type-instance (type-spec prompt &rest args)
  "Prompt for an instance of TYPE-SPEC using PROMPT."
  (let ((type-discriminator (if (listp type-spec)
                                (car type-spec)
                                type-spec))
        (type-arguments (if (listp type-spec)
                            (cdr type-spec)
                            nil)))
    (apply #'%prompt-for-type-instance 
           type-discriminator type-arguments prompt
           args)))

(define-command customize-variable () ()
  (let* ((var-names (mapcar
                     (alexandria:compose #'prin1-to-string #'variable-name)
                     (alexandria:hash-table-values *custom-vars*)))
         (variable-name 
           (prompt-for-string "Customize variable: "
                               :test-function (lambda (str) (< 0 (length str)))
                               :completion-function (lambda (string)
                                                      (completion string var-names))))
         (variable (find-variable (read-from-string variable-name)))
         (value (prompt-for-type-instance (variable-type variable) "Value: ")))
      (setf (symbol-value (variable-name variable)) value)))

(define-command customize-group () ()
  (let* ((group-names (mapcar
                       (alexandria:compose #'prin1-to-string #'group-name)
                       (alexandria:hash-table-values *custom-groups*)))
         (group-name 
           (prompt-for-string "Customize group: "
                              :test-function (lambda (str) (< 0 (length str)))
                              :completion-function (lambda (string)
                                                     (completion string group-names))))
         (group (find-group (read-from-string group-name)))
         (var-names (mapcar
                     (alexandria:compose #'prin1-to-string #'variable-name)
                     (group-variables group)))
         (variable-name 
           (prompt-for-string (format nil "Customize variable in ~a: " group-name)
                              :test-function (lambda (str) (< 0 (length str)))
                              :completion-function (lambda (string)
                                                     (completion string var-names))))
         (variable (find-variable (read-from-string variable-name)))
         (value (prompt-for-type-instance (variable-type variable) "Value: ")))
    (setf (symbol-value (variable-name variable)) value)))         
         