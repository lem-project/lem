(defpackage :lem-vi-mode/core
  (:use :cl
        :lem
        :lem/universal-argument)
  (:import-from :cl-package-locks)
  (:export :*enable-hook*
           :*disable-hook*
           :vi-state
           :vi-mode
           :define-vi-state
           :current-state
           :change-state
           :with-state
           :*command-keymap*
           :*insert-keymap*
           :*inactive-keymap*
           :post-command-hook
           :state-enabled-hook
           :state-disabled-hook
           :normal
           :insert))
(in-package :lem-vi-mode/core)

(defvar *default-cursor-color* nil)

(defvar *enable-hook* '())
(defvar *disable-hook* '())

(defun enable-hook ()
  (run-hooks *enable-hook*))

(defun disable-hook ()
  (run-hooks *disable-hook*))

(define-global-mode vi-mode (emacs-mode)
  (:name "vi"
   :enable-hook #'enable-hook
   :disable-hook #'disable-hook))


(defvar *modeline-element*)

(define-attribute state-attribute
  (t :reverse t))

(defstruct (vi-modeline-element (:conc-name element-))
  name)

(defmethod convert-modeline-element ((element vi-modeline-element) window)
  (values (element-name element) 'state-attribute))

(defun initialize-vi-modeline ()
  (setf *modeline-element* (make-vi-modeline-element))
  (modeline-add-status-list *modeline-element*))

(defun finalize-vi-modeline ()
  (modeline-remove-status-list *modeline-element*))

(defun change-element-name (name)
  (setf (element-name *modeline-element*) name))


(defclass vi-state ()
  ((message
  :initarg :message
  :reader state-message)
  (cursor-type 
  :initarg :cursor-type
  :reader state-cursor-type)
  (keymap
  :initarg :keymap
  :reader state-keymap)
  (cursor-color
  :initarg :cursor-color
  :accessor state-cursor-color))
  (:default-initargs
        :message nil
        :cursor-color nil
        :keymap *global-keymap*))

(defvar *current-state* nil)

;;; vi-state methods
(defmacro define-vi-state (name direct-super-classes direct-slot-specs &rest options)
  (let ((cleaned-super-classes (if (null direct-super-classes) '(vi-state) direct-super-classes)))
    `(progn
       (assert (find 'vi-state ',cleaned-super-classes :test #'(lambda (expected-class class) (closer-mop:subclassp class expected-class))) (',cleaned-super-classes) "At least one of the direct-super-classes should be vi-state or a subclass of vi-state!")
       (defclass ,name ,cleaned-super-classes
         ,direct-slot-specs
         ,@options)
       (setf (get ',name 'state)
             (make-instance ',name)))))

(defgeneric post-command-hook (state))

(defmethod post-command-hook ((state vi-state)))

(defgeneric state-enabled-hook (state))

(defmethod state-enabled-hook ((state vi-state))
  (let ((msg (state-message state)))
    (unless (null msg)
      (message msg))))

(defgeneric state-disabled-hook (state))

(defmethod state-disabled-hook ((state vi-state)))

(defun current-state ()
  *current-state*)

(defun ensure-state (state)
  (setf state
        (if (symbolp state)
            (get state 'state)
            state))
  (assert (typep state 'vi-state))
  state)

(defun change-state (name)
  (and *current-state*
       (state-disabled-hook (ensure-state *current-state*))) 
  (let ((state (ensure-state name)))
    (setf *current-state* name)
    (change-global-mode-keymap 'vi-mode (state-keymap state))
    (change-element-name (format nil "[~A]" name))
    (state-enabled-hook state)
    (unless *default-cursor-color*
      (setf *default-cursor-color*
            (attribute-background (ensure-attribute 'cursor nil))))
    (set-attribute 'cursor :background (or (state-cursor-color state) *default-cursor-color*))))

(defmacro with-state (state &body body)
  (alexandria:with-gensyms (old-state)
    `(let ((,old-state (current-state)))
       (change-state ,state)
       (unwind-protect (progn ,@body)
         (change-state ,old-state)))))


(defvar *command-keymap* (make-keymap :name '*command-keymap*
                                      :parent *global-keymap*))
(defvar *inactive-keymap* (make-keymap :parent *global-keymap*))

(define-vi-state normal () ()
  (:default-initargs
   :keymap *command-keymap*))

;; insert state
(defvar *insert-keymap* (make-keymap :name '*insert-keymap* :parent *global-keymap*))

(define-vi-state insert () () 
  (:default-initargs
   :message "-- INSERT --"
   :cursor-color "IndianRed"
   :keymap *insert-keymap*))

(define-vi-state vi-modeline () () 
  (:default-initargs
   :keymap *inactive-keymap*))

(defun prompt-activate-hook () (change-state 'vi-modeline))
(defun prompt-deactivate-hook () (change-state 'normal))

(defun vi-post-command-hook ()
  (when (mode-active-p (current-buffer) 'vi-mode)
    (post-command-hook (ensure-state (current-state)))))

(add-hook *post-command-hook* 'vi-post-command-hook)

(add-hook *enable-hook*
          (lambda ()
            (initialize-vi-modeline)
            (change-state 'normal)
            (add-hook *prompt-activate-hook* 'prompt-activate-hook)
            (add-hook *prompt-deactivate-hook* 'prompt-deactivate-hook)))

(add-hook *disable-hook*
          (lambda ()
            (finalize-vi-modeline)
            (remove-hook *prompt-activate-hook* 'prompt-activate-hook)
            (remove-hook *prompt-deactivate-hook* 'prompt-deactivate-hook)))
