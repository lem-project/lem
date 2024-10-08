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
   (setter :initarg :setter
           :accessor variable-setter
           :type (or symbol function null)
           :initform nil
           :documentation "A function designator used for setting the value of the variable")
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

(defun ensure-variable (variable-or-symbol)
  (etypecase variable-or-symbol
    (custom-variable variable-or-symbol)
    (symbol (find-variable variable-or-symbol))))

(defun ensure-group (group-or-symbol)
  (etypecase group-or-symbol
    (custom-group group-or-symbol)
    (symbol (find-group group-or-symbol))))

(defun get-variable-value (var)
  (symbol-value (variable-name (ensure-variable var))))

(defun set-variable-value (var value)
  (alexandria:if-let (setter (variable-setter (ensure-variable var)))
    (if setter
        (funcall setter value var)
        (setf (symbol-value (variable-name (ensure-variable var))) value))))

(defun reset-variable-value (var)
  (set-variable-value var (variable-default var)))

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

(defun prompt-for-variable (prompt &optional var-names)
  (let*
      ((actual-var-names (or var-names
                             (mapcar
                              (alexandria:compose #'prin1-to-string #'variable-name)
                              (alexandria:hash-table-values *custom-vars*))))
       (variable-name 
         (prompt-for-string prompt
                            :test-function (lambda (str) (< 0 (length str)))
                            :completion-function (lambda (string)
                                                   (completion string actual-var-names)))))
    (find-variable (read-from-string variable-name))))

(define-command reset-variable () ()
  (let ((variable (prompt-for-variable "Reset variable: ")))
    (reset-variable-value variable)))

(define-command customize-variable (var-designator) (:universal-nil)
  (let* ((variable (or (and var-designator (ensure-variable var-designator))
                       (prompt-for-variable "Customize variable: ")))
         (value (prompt-for-type-instance (variable-type variable) "Value: ")))
      (set-variable-value variable value)))

(defun prompt-for-group (prompt &optional group-names)
  (let* ((actual-group-names (or group-names
                                 (mapcar
                                  (alexandria:compose #'prin1-to-string #'group-name)
                                  (alexandria:hash-table-values *custom-groups*))))
         (group-name 
           (prompt-for-string "Customize group: "
                              :test-function (lambda (str) (< 0 (length str)))
                              :completion-function (lambda (string)
                                                     (completion string actual-group-names)))))
    (find-group (read-from-string group-name))))
         
(define-command customize-group (group-designator) (:universal-nil)
  (let* ((group (or (and group-designator (ensure-group group-designator))
                    (prompt-for-group "Customize group: ")))
         (variable (prompt-for-variable (format nil "Customize variable in ~a: " (group-name group))
                                        (mapcar
                                         (alexandria:compose #'prin1-to-string #'variable-name)
                                         (group-variables group))))
         (value (prompt-for-type-instance (variable-type variable) "Value: ")))
    (set-variable-value variable value)))

(defun open-customize-variable-buffer (variable)
  (let ((buf (make-buffer (format nil "*Customize variable: ~a*" (variable-name variable)))))
    (with-current-buffer buf
      (with-open-stream (stream (make-buffer-output-stream
                                 (buffer-end-point buf)))
        (write-string "Customize: " stream)
        (prin1 (variable-name variable) stream)
        (terpri stream)
        (write-string "Value: " stream)
        (prin1 (get-variable-value variable) stream)
        (write-string " " stream)
        (lem/button:insert-button 
         (current-point) "[Set]"
         (lambda ()
           (customize-variable variable)))
        (write-string " " stream)
        (lem/button:insert-button 
         (current-point) "[Reset]"
         (lambda ()
           (customize-variable variable)))
        (terpri stream)
        (terpri stream)
        (write-string (documentation-of variable) stream)
        
        (setf (lem-core::buffer-read-only-p buf) t)
        ;;(lem-core:switch-to-buffer buf)
        (lem-core:pop-to-buffer buf)
        ;;(lem-core:change-buffer-mode )
        ))))