(defpackage :lem-vi-mode/core
  (:use :cl
        :lem
        :lem/universal-argument)
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


(defstruct vi-state
  name
  keymap
  post-command-hook
  enable-hook
  disable-hook
  cursor-color)

(defvar *current-state* nil)

(defmacro define-vi-state (name (&key keymap post-command-hook cursor-color) &body spec)
  (let ((enable-form (rest (assoc :enable spec)))
        (disable-form (rest (assoc :disable spec))))
    `(setf (get ',name 'state)
           (make-vi-state :name ',name
                          :keymap ,keymap
                          :post-command-hook ,post-command-hook
                          :enable-hook ,(if enable-form `(lambda ,@enable-form))
                          :disable-hook ,(if disable-form `(lambda ,@disable-form))
                          :cursor-color ,cursor-color))))

(defun current-state ()
  *current-state*)

(defun ensure-state (state)
  (setf state
        (if (symbolp state)
            (get state 'state)
            state))
  (assert (vi-state-p state))
  state)

(defun change-state (name &rest args)
  (alexandria:when-let ((disable-hook (and *current-state*
                                           (vi-state-disable-hook (ensure-state *current-state*)))))
    (funcall disable-hook))
  (let ((state (ensure-state name)))
    (setf *current-state* name)
    (change-global-mode-keymap 'vi-mode (vi-state-keymap state))
    (change-element-name (format nil "[~A]" name))
    (when (vi-state-enable-hook state)
      (apply (vi-state-enable-hook state) args))
    (unless *default-cursor-color*
      (setf *default-cursor-color*
            (attribute-background (ensure-attribute 'cursor nil))))
    (set-attribute 'cursor :background (or (vi-state-cursor-color state) *default-cursor-color*))))

(defmacro with-state (state &body body)
  (alexandria:with-gensyms (old-state)
    `(let ((,old-state (current-state)))
       (change-state ,state)
       (unwind-protect (progn ,@body)
         (change-state ,old-state)))))


(defvar *command-keymap* (make-keymap :name '*command-keymap*
                                      :parent *global-keymap*))
(defvar *insert-keymap* (make-keymap :name '*insert-keymap* :parent *global-keymap*))
(defvar *inactive-keymap* (make-keymap :parent *global-keymap*))

(define-vi-state normal (:keymap *command-keymap*))

(define-vi-state insert (:keymap *insert-keymap* :cursor-color "IndianRed")
  (:enable () (message " -- INSERT --")))

(define-vi-state modeline (:keymap *inactive-keymap*))

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

(defun prompt-activate-hook () (change-state 'modeline))
(defun prompt-deactivate-hook () (change-state 'normal))

(defun vi-post-command-hook ()
  (alexandria:when-let ((it (vi-state-post-command-hook
                             (ensure-state (current-state)))))
    (funcall it)))

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
