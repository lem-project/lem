(in-package :lem)

(export '(make-attribute
          ensure-attribute
          merge-attribute
          set-attribute
          set-attribute-foreground
          set-attribute-background
          set-attribute-reverse-p
          set-attribute-bold-p
          set-attribute-underline-p
          attribute-foreground
          attribute-background
          attribute-reverse-p
          attribute-bold-p
          attribute-underline-p
          define-attribute
          cursor
          region
          modeline
          modeline-inactive
          truncate-attribute
          compiler-note-attribute
          syntax-warning-attribute
          syntax-string-attribute
          syntax-comment-attribute
          syntax-keyword-attribute
          syntax-constant-attribute
          syntax-function-name-attribute
          syntax-variable-attribute
          syntax-type-attribute
          syntax-builtin-attribute
          completion-attribute
          non-focus-completion-attribute
	  *attribute-destroy-%internal-value*))

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

;; 22-Dec-17 stacksmith:
;; If internal value needs to be deallocated, set this to (lambda (attribute))
(defparameter *attribute-destroy-%internal-value* nil)

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

(defun merge-attribute (under over)
  (make-attribute :foreground (or (attribute-foreground over)
                                  (attribute-foreground under))
                  :background (or (attribute-background over)
                                  (attribute-background under))
                  :bold-p (or (attribute-bold-p over)
                              (attribute-bold-p under))
                  :reverse-p (or (attribute-reverse-p over)
                                 (attribute-reverse-p under))
                  :underline-p (or (attribute-underline-p over)
                                   (attribute-underline-p under))))

(defun set-attribute (attribute &key (foreground nil foregroundp)
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
    (when *attribute-destroy-%internal-value* ; let client clean up
      (funcall *attribute-destroy-%internal-value* attribute))
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
                         ,@(if (null specs)
                               `((t (make-attribute)))
                               (loop :for (pattern . args) :in specs
                                     :collect (if (eq pattern t)
                                                  `(t (make-attribute ,@args))
                                                  `((or ,@(mapcar
                                                           (lambda (p)
                                                             (cond ((eq p :light)
                                                                    '(display-light-p))
                                                                   ((eq p :dark)
                                                                    `(display-dark-p))))
                                                           (alexandria:ensure-list pattern)))
                                                    (make-attribute ,@args))))))))))))

(define-attribute cursor
  (:light :foreground "white" :background "black")
  (:dark :foreground "black" :background "white"))

(define-attribute region
  (:light :foreground nil :background "#eedc82")
  (:dark :foreground nil :background "blue"))

(define-attribute modeline
  (t :background "#bbbbbb" :foreground "black"))

(define-attribute modeline-inactive
  (t :background "#bbbbbb" :foreground "#777777"))

(define-attribute truncate-attribute)

(define-attribute compiler-note-attribute
  (t :foreground "red" :underline-p t))

(define-attribute syntax-warning-attribute
  (t :foreground "red"))

(define-attribute syntax-string-attribute
  (:light :foreground "#8B2252")
  (:dark :foreground "light salmon"))

(define-attribute syntax-comment-attribute
  (:light :foreground "#cd0000")
  (:dark :foreground "chocolate1"))

(define-attribute syntax-keyword-attribute
  (:light :foreground "purple")
  (:dark :foreground "cyan1"))

(define-attribute syntax-constant-attribute
  (:light :foreground "#ff00ff")
  (:dark :foreground "LightSteelBlue"))

(define-attribute syntax-function-name-attribute
  (:light :foreground "#0000ff")
  (:dark :foreground "LightSkyBlue"))

(define-attribute syntax-variable-attribute
  (:light :foreground "#8D5232")
  (:dark :foreground "LightGoldenrod"))

(define-attribute syntax-type-attribute
  (:light :foreground "#00875f")
  (:dark :foreground "PaleGreen"))

(define-attribute syntax-builtin-attribute
  (t :foreground "#D030F0"))
