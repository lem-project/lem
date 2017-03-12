(in-package :lem)

(export '(make-attribute
          ensure-attribute
          set-attribute
          set-attribute-foreground
          set-attribute-background
          set-attribute-reverse-p
          set-attribute-bold-p
          set-attribute-underline-p
          define-attribute
          region
          modeline
          modeline-inactive
          syntax-string-attribute
          syntax-comment-attribute
          syntax-keyword-attribute
          syntax-constant-attribute
          syntax-function-name-attribute
          syntax-variable-attribute
          syntax-type-attribute))

(defvar *attributes* '())

(defclass attribute ()
  ((foreground
    :initarg :foreground
    :reader attribute-foreground)
   (background
    :initarg :background
    :reader attribute-background)
   (reverse-p
    :initarg :reverse-p
    :reader attribute-reverse-p)
   (bold-p
    :initarg :bold-p
    :reader attribute-bold-p)
   (underline-p
    :initarg :underline-p
    :reader attribute-underline-p)
   (%internal-value
    :initform nil
    :accessor attribute-%internal-value)))

(defun attribute-p (x)
  (typep x 'attribute))

(defun make-attribute (&key foreground background reverse-p bold-p underline-p)
  (make-instance 'attribute
                 :foreground foreground
                 :background background
                 :reverse-p reverse-p
                 :bold-p bold-p
                 :underline-p underline-p))

(defun ensure-attribute (x raise)
  (cond ((symbolp x)
         (let ((fn (get x 'attribute)))
           (cond (fn (funcall fn))
                 (raise (error "invalid attribute: ~A" x))
                 (t nil))))
        ((attribute-p x)
         x)
        (t
         nil)))

(defun set-attribute (attribute &key
                                (foreground nil foregroundp)
                                (background nil backgroundp)
                                reverse-p bold-p underline-p)
  (let ((attribute (ensure-attribute attribute t)))
    (setf (attribute-%internal-value attribute) nil)
    (when foregroundp
      (setf (slot-value attribute 'foreground) foreground))
    (when backgroundp
      (setf (slot-value attribute 'background) background))
    (setf (slot-value attribute 'reverse-p) reverse-p)
    (setf (slot-value attribute 'bold-p) bold-p)
    (setf (slot-value attribute 'underline-p) underline-p)))

(macrolet ((def (setter slot-name)
             `(defun ,setter (attribute value)
                (let ((attribute (ensure-attribute attribute t)))
                  (setf (attribute-%internal-value attribute) nil)
                  (setf (slot-value attribute ',slot-name) value)))))
  (def set-attribute-foreground foreground)
  (def set-attribute-background background)
  (def set-attribute-reverse-p reverse-p)
  (def set-attribute-bold-p bold-p)
  (def set-attribute-underline-p underline-p))

(defun clear-all-attribute-cache ()
  (dolist (attribute *attributes*)
    (setf (get attribute '%attribute-value) nil)))

(defun display-light-p ()
  (eq :light (display-background-mode)))

(defun display-dark-p ()
  (eq :dark (display-background-mode)))

(defmacro define-attribute (name &body specs)
  (check-type name symbol)
  `(progn
     (pushnew ',name *attributes*)
     (setf (get ',name '%attribute-value) nil)
     (setf (get ',name 'attribute)
           (lambda ()
             (or (get ',name '%attribute-value)
                 (setf (get ',name '%attribute-value)
                       (cond
                         ,@(loop :for (pattern . args) :in specs
                                 :collect (if (eq pattern t)
                                              `(t (make-attribute ,@args))
                                              `((or ,@(mapcar
                                                       (lambda (p)
                                                         (cond ((eq p :light)
                                                                '(display-light-p))
                                                               ((eq p :dark)
                                                                `(display-dark-p))))
                                                       (alexandria:ensure-list pattern)))
                                                (make-attribute ,@args)))))))))))

(define-attribute region
  (:light :reverse-p t)
  (:dark :reverse-p t))

(define-attribute modeline
  (:light :reverse-p t)
  (:dark :reverse-p t))

(define-attribute modeline-inactive
  (:light :reverse-p t)
  (:dark :reverse-p t))

(define-attribute syntax-string-attribute
  (t :foreground "green"))

(define-attribute syntax-comment-attribute
  (t :foreground "red"))

(define-attribute syntax-keyword-attribute
  (t :foreground "blue"))

(define-attribute syntax-constant-attribute
  (t :foreground "magenta"))

(define-attribute syntax-function-name-attribute
  (t :foreground "cyan"))

(define-attribute syntax-variable-attribute
  (t :foreground "yellow"))

(define-attribute syntax-type-attribute
  (t :foreground "cyan"))
