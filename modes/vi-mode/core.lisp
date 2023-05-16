(defpackage :lem-vi-mode/core
  (:use :cl
        :lem
        :lem/universal-argument)
  (:import-from :cl-package-locks)
  (:export :*enable-hook*
           :*disable-hook*
           :vi-mode
           :define-vi-state
           :current-state
           :change-state
           :with-state
           :*command-keymap*
           :*insert-keymap*
           :*inactive-keymap*
           :post-command-hook
           :state-enable-hook
           :state-disable-hook
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
  ((tag
  :initarg :tag
  :reader tag)
  (message
  :initarg :message
  :reader msg)
  (cursor-type 
  :initarg :cursor-type
  :reader cursor-type)
  (keymap
  :initarg :keymap
  :reader state-keymap)
  (cursor-color
  :initarg :cursor-color
  :accessor cursor-color)))

(defvar *current-state* nil)

;;; vi-state methods
(defmacro define-vi-state (name (&key tag message cursor-type keymap cursor-color) &body spec)
    `(progn
      (cl-package-locks:without-package-locks
       (defclass ,name (vi-state) ()))
      (setf (get ',name 'state)
           (make-instance ',name
                          :tag ,tag
                          :message ,message
                          :cursor-type ,cursor-type
                          :keymap ,keymap
                          :cursor-color ,cursor-color))))

(defgeneric post-command-hook (state))

(defmethod post-command-hook ((state vi-state)))

(defgeneric state-enable-hook (state &rest args))

(defmethod state-enable-hook ((state vi-state) &rest args))

(defgeneric state-disable-hook (state))

(defmethod state-disable-hook ((state vi-state)))

(defun current-state ()
  *current-state*)

(defun ensure-state (state)
  (setf state
        (if (symbolp state)
            (get state 'state)
            state))
  (assert (or (subtypep (type-of state) 'vi-state) (typep 'vi-state (type-of state))))
  state)

(defun change-state (name &rest args)
  (and *current-state*
       (state-disable-hook (ensure-state *current-state*))) 
  (let ((state (ensure-state name)))
    (setf *current-state* name)
    (change-global-mode-keymap 'vi-mode (state-keymap state))
    (change-element-name (format nil "[~A]" name))
    (state-enable-hook state args)
    (unless *default-cursor-color*
      (setf *default-cursor-color*
            (attribute-background (ensure-attribute 'cursor nil))))
    (set-attribute 'cursor :background (or (cursor-color state) *default-cursor-color*))))

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

(defmethod state-enable-hook ((state insert) &rest args)
  (message " -- INSERT --"))

(define-vi-state modeline (:keymap *inactive-keymap*))

(defun prompt-activate-hook () (change-state 'modeline))
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
