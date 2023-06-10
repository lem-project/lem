(defpackage :lem-vi-mode/core
  (:use :cl
        :lem
        :lem/universal-argument)
  (:import-from :cl-package-locks)
  (:export :*enable-hook*
           :*disable-hook*
           :vi-mode
           :define-vi-state
           :define-vi-operator
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
  :accessor state-cursor-color)))

(defvar *current-state* nil)

;;; vi-state methods
(defmacro define-vi-state (name (&key tag message cursor-type keymap cursor-color) &body spec)
  `(progn
     (defclass ,name (vi-state) ())
       (setf (get ',name 'state)
             (make-instance ',name
                            :message ,message
                            :cursor-type ,cursor-type
                            :keymap ,keymap
                            :cursor-color ,cursor-color))))

(defgeneric post-command-hook (state))

(defmethod post-command-hook ((state vi-state)))

(defgeneric state-enabled-hook (state &rest args))

(defmethod state-enabled-hook ((state vi-state) &rest args))

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

(defun change-state (name &rest args)
  (and *current-state*
       (state-disabled-hook (ensure-state *current-state*))) 
  (let ((state (ensure-state name)))
    (setf *current-state* name)
    (change-global-mode-keymap 'vi-mode (state-keymap state))
    (change-element-name (format nil "[~A]" name))
    (state-enabled-hook state args)
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

(define-vi-state normal (:keymap *command-keymap*))


;; insert state
(defvar *insert-keymap* (make-keymap :name '*insert-keymap* :parent *global-keymap*))

(define-vi-state insert (:keymap *insert-keymap* :cursor-color "IndianRed"))

(defmethod state-enabled-hook ((state insert) &rest args)
  (message "-- INSERT --"))

(define-vi-state vi-modeline (:keymap *inactive-keymap*))

;; vi-commands CLOS
(defmacro %define-vi-action (&whole form vi-action name-and-options params (&rest arg-descriptors) &body body)
  (destructuring-bind (name . options) (uiop:ensure-list name-and-options)
    (let ((advice-classes (alexandria:assoc-value options :advice-classes))
          (class-name (alexandria:if-let (elt (assoc :class options))
                        (second elt)
                        name))
          (command-name (alexandria:if-let (elt (assoc :name options))
                          (second elt)
                          (string-downcase name))))
                          `(define-command (,class-name (:advice-classes vi-action ,@advice-classes)) ,params ,arg-descriptors
                           ,@body))))

(defclass vi-action () ())

(defmacro define-vi-action (&whole form name-and-options params (&rest arg-descriptors) &body body)
  `(%define-vi-action vi-action ,name-and-options ,params ,arg-descriptors ,@body))

(defclass vi-operator (vi-action) ())

(defmacro define-vi-operator (&whole form name-and-options params (&rest arg-descriptors) &body body)
  `(%define-vi-action vi-operator ,name-and-options ,params ,arg-descriptors ,@body))

;; A command is something like <Esc>, :, v, i, etc.
(defclass vi-command (vi-action) ())

(defmacro define-vi-command (&whole form name-and-options params (&rest arg-descriptors) &body body)
  `(%define-vi-action vi-command ,name-and-options ,params ,arg-descriptors ,@body))
;;;;;;;;;;;;;;;;;;;;;;;

(defclass vi-motion (vi-command) ())

(defmacro define-vi-motion (&whole form name-and-options params (&rest arg-descriptors) &body body)
  `(%define-vi-action vi-motion ,name-and-options ,params ,arg-descriptors ,@body))

(defclass vi-text-object (vi-motion) ())

(defmacro define-vi-text-object (&whole form name-and-options params (&rest arg-descriptors) &body body)
  `(%define-vi-action vi-text-object ,name-and-options ,params ,arg-descriptors ,@body))
;;

(defun prompt-activate-hook () (change-state 'vi-modeline))
(defun prompt-deactivate-hook () (change-state 'normal))

(defun vi-post-command-hook ()
 (post-command-hook (ensure-state (current-state))))

(define-condition post-command-hook (after-executing-command) ())
(defmethod handle-signal ((condition post-command-hook))
  (when (mode-active-p (current-buffer) 'vi-mode)
    (vi-post-command-hook)))

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
