;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Customization.html

(defpackage :lem/settings
  (:use :cl :lem-core)
  (:import-from :lem/button
                :button-at
                :button-action
                :forward-button
                :insert-button)
  (:import-from :lem-core/commands/window
                :quit-active-window)
  (:export #:customize-variable
           #:customize-group
           #:defcustom
           #:defgroup
           #:set-variable
           #:find-variable
           #:find-group))

(in-package :lem/settings)

(define-attribute settings-label-attribute)

(define-attribute settings-value-attribute
  (t :foreground :base0D :bold t))

(define-attribute settings-action-attribute
  (:dark :foreground :base09 :bold t))

(define-attribute settings-docs-attribute)

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

(defmethod group-of ((var custom-variable))
  (find-group (slot-value var 'group)))

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

(defmacro defgroup (symbol members doc &key group)
  `(progn
     (setf (gethash ',symbol *custom-groups*)
           (make-instance 'custom-group
                          :name ',symbol
                          :documentation ,doc
                          :group ',group))
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

(defun set-variable-value (var-designator value)
  (let ((var (ensure-variable var-designator)))
    (alexandria:if-let (setter (variable-setter var))
      (funcall setter value var)
      (setf (symbol-value (variable-name var)) value))))

(defun reset-variable-value (var)
  (set-variable-value var (variable-default var)))

(defgeneric %prompt-for-type-instance (type-discriminator type-arguments prompt &rest args))

(defmethod %prompt-for-type-instance ((type (eql 'string)) type-args prompt &rest args)
  (prompt-for-string prompt :initial-value (getf args :initial-value)))

(defmethod %prompt-for-type-instance ((type (eql 'integer)) type-args prompt &rest args)
  (prompt-for-integer prompt :initial-value (getf args :initial-value)))

(defmethod %prompt-for-type-instance ((type (eql 'boolean)) type-args prompt &rest args)
  (declare (ignore args))
  (prompt-for-y-or-n-p prompt))

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

(define-command reset-variable (var-designator) (:universal-nil)
  (let ((variable (or (and var-designator (ensure-variable var-designator))
                      (prompt-for-variable "Reset variable: "))))
    (reset-variable-value variable)))

(define-command set-variable (var-designator) (:universal-nil)
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
         
(define-command customize-group-with-menu (group-designator) (:universal-nil)
  (let* ((group (or (and group-designator (ensure-group group-designator))
                    (prompt-for-group "Customize group: ")))
         (variable (prompt-for-variable (format nil "Customize variable in ~a: " (group-name group))
                                        (mapcar
                                         (alexandria:compose #'prin1-to-string #'variable-name)
                                         (group-variables group))))
         (value (prompt-for-type-instance (variable-type variable) "Value: ")))
    (set-variable-value variable value)))

(define-command apropos-custom-variable (pattern) (:universal-nil)
  (error "TODO"))

(defun make-settings-buffer (name &rest args)
  (let ((buffer (apply #'make-buffer name args)))
    (change-buffer-mode buffer 'settings-mode)
    buffer))

(define-command customize-variable (var-designator) (:universal-nil)
  (let* ((variable (or (and var-designator (ensure-variable var-designator))
                       (prompt-for-variable "Customize variable: ")))
         (buf (make-settings-buffer (format nil "*Customize variable: ~a*" (variable-name variable)))))
    (labels ((render-buffer ()
               (with-current-buffer buf
                 (with-open-stream (stream (make-buffer-output-stream
                                            (buffer-end-point buf)))
                   (write-string "Customize: " stream)
                   (insert-string (current-point) (prin1-to-string (variable-name variable)) :attribute 'document-header1-attribute)
                   (write-string " in: " stream)
                   (insert-button (current-point) (string (group-name (group-of variable)))
                                  (lambda ()
                                    (customize-group (group-of variable)))
                                  :attribute 'document-link-attribute)
                   (terpri stream)
                   (insert-string (current-point) "Value: " :attribute 'settings-label-attribute)
                   (insert-string (current-point) (prin1-to-string (get-variable-value variable)) :attribute 'settings-value-attribute)
                   (write-string " " stream)
                   (insert-button (current-point) "[Set]"
                                  (lambda ()
                                    (set-variable variable)
                                    (with-buffer-read-only buf nil 
                                      (erase-buffer buf)
                                      (render-buffer)))
                                  :attribute 'settings-action-attribute)
                   (write-string " " stream)
                   (lem/button:insert-button 
                    (current-point) "[Reset]"
                    (lambda ()
                      (reset-variable variable)
                      (with-buffer-read-only buf nil 
                        (erase-buffer buf)
                        (render-buffer)))
                    :attribute 'settings-action-attribute)
                   (terpri stream)
                   (terpri stream)
                   (insert-string (current-point) (documentation-of variable)
                                  :attribute 'settings-docs-attribute)))))
      (with-buffer-read-only buf nil 
        (render-buffer))
      (switch-to-buffer buf))))

(defun open-customize-group-buffer (group)
  (let ((buf (make-settings-buffer (format nil "*Customize group: ~a*" (group-name group)))))
    (with-current-buffer buf
      (with-buffer-read-only buf nil 
        (with-open-stream (stream (make-buffer-output-stream
                                   (buffer-end-point buf)))
          (write-string "Customize group: " stream)
          (prin1 (group-name group) stream)
          (terpri stream) (terpri stream)
          (write-string (documentation-of group) stream)
          (terpri stream) 
          (dolist (var (group-variables group))
            (terpri stream)
            (prin1 (variable-name var) stream)
            (write-string " - " stream)
            (write-string (documentation-of var) stream))

          (when (subgroups group)
            (terpri stream) (terpri stream)
            (write-string "Subgroups:" stream)
            (terpri stream)
            (dolist (subgroup (subgroups group))
              (terpri stream)
              (write-string (string (group-name subgroup)) stream)))           
                
          (switch-to-buffer buf)
          )))))

(define-major-mode settings-mode nil
    (:name "settings"
     :keymap *settings-keymap*)
  (setf (buffer-read-only-p (current-buffer)) t))

(define-key *settings-keymap* "Return" 'settings-default-action)
(define-key *settings-keymap* "Tab" 'settings-forward-button)
(define-key *settings-keymap* "q" 'quit-active-window)
(define-key *settings-keymap* "M-q" 'quit-active-window)

(define-command settings-default-action () ()
  (let ((button (button-at (current-point))))
    (when button (button-action button))))

(define-command settings-forward-button () ()
  (let ((p (current-point)))
    (or (forward-button p)
        (progn
          (buffer-start p)
          (forward-button p)))))