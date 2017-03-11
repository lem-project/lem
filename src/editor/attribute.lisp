(in-package :lem)

(export '(make-attribute
          ensure-attribute
          define-attribute
          mark-overlay-attribute
          modeline-attribute
          modeline-inactive-attribute
          syntax-string-attribute
          syntax-comment-attribute
          syntax-keyword-attribute
          syntax-constant-attribute
          syntax-function-name-attribute
          syntax-variable-attribute
          syntax-type-attribute))

(defstruct (attribute (:constructor %make-attribute))
  foreground
  background
  reverse-p
  bold-p
  underline-p
  %internal-value)

(defun make-attribute (&key foreground background reverse-p bold-p underline-p)
  (%make-attribute :foreground foreground
                   :background background
                   :reverse-p reverse-p
                   :bold-p bold-p
                   :underline-p underline-p))

(defun ensure-attribute (x)
  (cond ((symbolp x)
         (let ((fn (get x 'attribute)))
           (when fn
             (funcall fn))))
        ((attribute-p x)
         x)
        (t
         nil)))

(defun display-light-p ()
  (eq :light (display-background-mode)))

(defun display-dark-p ()
  (eq :dark (display-background-mode)))

(defmacro define-attribute (name &body specs)
  (check-type name symbol)
  `(setf (get ',name 'attribute)
         (lambda ()
           (or (get ',name '%attribute-value)
               (setf (get ',name '%attribute-value)
                     (cond ,@(loop :for (pattern . args) :in specs
                                   :collect (ecase pattern
                                              ((:light)
                                               `((display-light-p)
                                                 (make-attribute ,@args)))
                                              ((:dark)
                                               `((display-dark-p)
                                                 (make-attribute ,@args)))
                                              ((t)
                                               `(t (make-attribute ,@args)))))))))))

(define-attribute mark-overlay-attribute
  (:light :foreground "blue" :reverse-p t)
  (:dark :foreground "cyan" :reverse-p t))

(define-attribute modeline-attribute
  (:light :reverse-p t)
  (:dark :reverse-p t))

(define-attribute modeline-inactive-attribute
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
