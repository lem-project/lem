(in-package :lem)

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
   (cache
    :initform nil
    :accessor attribute-cache)))

(defmethod print-object ((attribute attribute) stream)
  (print-unreadable-object (attribute stream :type t :identity t)
    (format stream "(~A ~A)~:[~; reverse~]~:[~; bold~]~:[~; underline~]"
            (or (attribute-foreground attribute) "")
            (or (attribute-background attribute) "")
            (attribute-reverse-p attribute)
            (attribute-bold-p attribute)
            (attribute-underline-p attribute))))

(defun attribute-p (x)
  (typep x 'attribute))

(defun make-attribute (&key foreground background reverse-p bold-p underline-p)
  (make-instance 'attribute
                 :foreground foreground
                 :background background
                 :reverse-p reverse-p
                 :bold-p bold-p
                 :underline-p underline-p))

(defun ensure-attribute (x &optional (errorp t))
  (cond ((symbolp x)
         (let ((fn (get x 'attribute)))
           (cond (fn (funcall fn))
                 (errorp (error "invalid attribute: ~A" x))
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

(defun attribute-equal (attribute1 attribute2)
  (and (equal (attribute-foreground attribute1)
              (attribute-foreground attribute2))
       (equal (attribute-background attribute1)
              (attribute-background attribute2))
       (equal (attribute-reverse-p attribute1)
              (attribute-reverse-p attribute2))
       (equal (attribute-bold-p attribute1)
              (attribute-bold-p attribute2))
       (equal (attribute-underline-p attribute1)
              (attribute-underline-p attribute2))))

(defun set-attribute (attribute &key (foreground nil foregroundp)
                                     (background nil backgroundp)
                                     reverse-p bold-p underline-p)
  (let ((attribute (ensure-attribute attribute t)))
    (setf (attribute-cache attribute) nil)
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
                  (setf (attribute-cache attribute) nil)
                  (setf (slot-value attribute ',slot-name) value)))))
  (def set-attribute-foreground foreground)
  (def set-attribute-background background)
  (def set-attribute-reverse-p reverse-p)
  (def set-attribute-bold-p bold-p)
  (def set-attribute-underline-p underline-p))

(defun clear-all-attribute-cache ()
  (dolist (attribute *attributes*)
    (setf (get attribute '%attribute-value) nil)))

(defun get-attribute-cache (attribute &rest args &key background)
  (declare (ignore background))
  (if (null (attribute-cache attribute))
      nil
      (cdr (assoc args
                  (attribute-cache attribute)
                  :test #'equal))))

(defun (setf get-attribute-cache) (value attribute &rest args &key background)
  (declare (ignore background))
  (alexandria:if-let ((elt (assoc args
                                  (attribute-cache attribute)
                                  :test #'equal)))
    (setf (cdr elt) value)
    (setf (attribute-cache attribute)
          (acons args
                 value
                 (attribute-cache attribute))))
  value)

(defun display-light-p ()
  (eq :light (display-background-mode)))

(defun display-dark-p ()
  (eq :dark (display-background-mode)))

(defmacro define-attribute (name &body specs)
  (check-type name symbol)
  `(progn
     ,(when (eql *package* (symbol-package name))
        ;; for the purpose of source location.
        `(defvar ,name))
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
                                                    (make-attribute ,@args))))))))))
     ',name))

(define-attribute cursor
  (:light :foreground "white" :background "black")
  (:dark :foreground "black" :background "white"))

(define-attribute fake-cursor
  (:light :foreground "white" :background "blue")
  (:dark :foreground "black" :background "yellow"))

(define-attribute region
  (:light :foreground nil :background "#eedc82")
  (:dark :foreground nil :background "blue"))

(define-attribute modeline
  (t :bold-p t :background "#404040" :foreground "white"))

(define-attribute modeline-inactive
  (t :bold-p t :background "#212121" :foreground "#707070"))

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
  (t :foreground "#FF87FF"))
