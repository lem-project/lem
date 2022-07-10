(in-package :lem)

(defvar *global-minor-mode-list* '())
(defvar *mode-objects* '())

(defclass mode ()
  ((name :initarg :name :reader mode-name)
   (description :initarg :description :reader mode-description)
   (keymap :initarg :keymap :reader mode-keymap)))

(defclass major-mode (mode)
  ((syntax-table :initarg :syntax-table :reader mode-syntax-table)
   (hook-variable :initarg :hook-variable :reader mode-hook-variable)))

(defclass minor-mode (mode)
  ((enable-hook :initarg :enable-hook :reader mode-enable-hook)
   (disable-hook :initarg :disable-hook :reader mode-disable-hook)))

(defmethod mode-identifier-name ((mode mode))
  (type-of mode))

(defun major-mode-p (mode)
  (typep mode 'major-mode))

(defun minor-mode-p (mode)
  (typep mode 'minor-mode))

(defun get-mode-object (mode-name)
  (get mode-name 'mode-object))

(defun register-mode (name object)
  (setf *mode-objects*
        (cons object
              (remove name
                      *mode-objects*
                      :key #'mode-identifier-name
                      :test #'eq)))
  (setf (get name 'mode-object) object))

(defun find-mode (name)
  (find name
        *mode-objects*
        :key #'mode-name
        :test #'string-equal))

(defun collect-modes (test-function)
  (sort (remove-if-not test-function *mode-objects*)
        #'string<
        :key #'mode-identifier-name))

(defun major-modes ()
  (mapcar #'mode-identifier-name (collect-modes #'major-mode-p)))

(defun minor-modes ()
  (mapcar #'mode-identifier-name (collect-modes #'minor-mode-p)))

(defmethod mode-name ((mode symbol))
  (assert (not (null mode)))
  (mode-name (get-mode-object mode)))

(defmethod mode-description ((mode symbol))
  (assert (not (null mode)))
  (mode-description (get-mode-object mode)))

(defmethod mode-keymap ((mode symbol))
  (assert (not (null mode)))
  (mode-keymap (get-mode-object mode)))

(defmethod mode-syntax-table ((mode symbol))
  (assert (not (null mode)))
  (mode-syntax-table (get-mode-object mode)))

(defmethod mode-enable-hook ((mode symbol))
  (assert (not (null mode)))
  (mode-enable-hook (get-mode-object mode)))

(defmethod mode-disable-hook ((mode symbol))
  (assert (not (null mode)))
  (mode-disable-hook (get-mode-object mode)))

(defmethod mode-hook-variable ((mode symbol))
  (assert (not (null mode)))
  (mode-hook-variable (get-mode-object mode)))

(defun global-minor-mode-p (mode)
  (get mode 'global-minor-mode-p))

(defun mode-active-p (buffer mode)
  (or (eq mode (buffer-major-mode buffer))
      (find mode (buffer-minor-modes buffer))
      (find mode *global-minor-mode-list*)
      (eq mode (mode-name (current-global-mode)))))

(defun enable-minor-mode (minor-mode)
  (if (global-minor-mode-p minor-mode)
      (pushnew minor-mode *global-minor-mode-list*)
      (pushnew minor-mode (buffer-minor-modes (current-buffer))))
  (when (mode-enable-hook minor-mode)
    (funcall (mode-enable-hook minor-mode))))

(defun disable-minor-mode (minor-mode)
  (if (global-minor-mode-p minor-mode)
      (setf *global-minor-mode-list*
            (delete minor-mode *global-minor-mode-list*))
      (setf (buffer-minor-modes (current-buffer))
            (delete minor-mode (buffer-minor-modes (current-buffer)))))
  (when (mode-disable-hook minor-mode)
    (funcall (mode-disable-hook minor-mode))))

(defun toggle-minor-mode (minor-mode)
  (if (mode-active-p (current-buffer) minor-mode)
      (disable-minor-mode minor-mode)
      (enable-minor-mode minor-mode)))

(defmacro define-major-mode (major-mode
                             parent-mode
                             (&key name
                                   description
                                   keymap
                                   (syntax-table '(fundamental-syntax-table))
                                   mode-hook)
                             &body body)
  (let ((command-class-name (make-symbol (string major-mode))))
    `(progn
       ,@(when mode-hook
           `((defvar ,mode-hook '())))
       ,@(when keymap
           `((defvar ,keymap (make-keymap :name ',keymap
                                          :parent ,(when parent-mode
                                                     `(mode-keymap ',parent-mode))))))
       (define-command (,major-mode (:class-name ,command-class-name)) () ()
         (clear-editor-local-variables (current-buffer))
         ,(when parent-mode `(,parent-mode))
         (setf (buffer-major-mode (current-buffer)) ',major-mode)
         (setf (buffer-syntax-table (current-buffer)) (mode-syntax-table ',major-mode))
         ,@body
         ,(when mode-hook
            `(run-hooks ,mode-hook)))
       (defclass ,major-mode (,(or parent-mode 'major-mode))
         ()
         (:default-initargs
          :name ,name
          :description ,description
          :keymap ,keymap
          :syntax-table ,syntax-table
          :hook-variable ',mode-hook))
       (register-mode ',major-mode (make-instance ',major-mode)))))

(defmacro define-minor-mode (minor-mode
                             (&key name description (keymap nil keymapp) global enable-hook disable-hook)
                             &body body)
  (let ((command-class-name (make-symbol (string minor-mode))))
    `(progn
       ,(when global
          `(setf (get ',minor-mode 'global-minor-mode-p) t))
       ,@(when keymapp
           `((defvar ,keymap (make-keymap :name ',keymap))))
       (define-command (,minor-mode (:class-name ,command-class-name)) (&optional (arg nil arg-p)) ("p")
         (cond ((not arg-p)
                (toggle-minor-mode ',minor-mode))
               ((eq arg t)
                (enable-minor-mode ',minor-mode))
               ((eq arg nil)
                (disable-minor-mode ',minor-mode))
               ((integerp arg)
                (toggle-minor-mode ',minor-mode))
               (t
                (error "Invalid arg: ~S" arg)))
         ,@body)
       (defclass ,minor-mode (minor-mode)
         ()
         (:default-initargs
          :name ,name
          :description ,description
          :keymap ,keymap
          :enable-hook ,enable-hook
          :disable-hook ,disable-hook))
       (register-mode ',minor-mode (make-instance ',minor-mode)))))

(defun change-buffer-mode (buffer mode &rest args)
  (save-excursion
    (setf (current-buffer) buffer)
    (apply mode args))
  buffer)

(defvar *global-mode-list* '())
(defvar *current-global-mode* nil)

(defclass global-mode ()
  ((name :initarg :name :reader mode-name)
   (parent :initarg :parent :reader mode-parent)
   (keymap :initarg :keymap :reader mode-keymap :writer set-mode-keymap)
   (enable-hook :initarg :enable-hook :reader mode-enable-hook)
   (disable-hook :initarg :disable-hook :reader mode-disable-hook)))

(defun current-global-mode ()
  (if (symbolp *current-global-mode*)
      (setf *current-global-mode*
            (get *current-global-mode* 'global-mode))
      *current-global-mode*))

(defun change-global-mode-keymap (mode keymap)
  (set-mode-keymap keymap (get mode 'global-mode)))

(defun change-global-mode (mode)
  (flet ((call (fun)
           (unless (null fun)
             (alexandria:when-let ((fun (alexandria:ensure-function fun)))
               (funcall fun)))))
    (let ((global-mode (get mode 'global-mode)))
      (check-type global-mode global-mode)
      (when global-mode
        (when *current-global-mode*
          (call (mode-disable-hook *current-global-mode*)))
        (setf *current-global-mode* global-mode)
        (call (mode-enable-hook global-mode))))))

(defmacro define-global-mode (mode (&optional parent) (&key keymap enable-hook disable-hook))
  (check-type parent symbol)
  (alexandria:with-gensyms (global-mode parent-mode)
    (let ((command-class-name (make-symbol (string mode))))
      `(progn
         ,@(when keymap
             `((defvar ,keymap
                 (make-keymap :name ',keymap
                              :parent (alexandria:when-let ((,parent-mode
                                                             ,(when parent
                                                                `(get ',parent 'global-mode))))
                                        (mode-keymap ,parent-mode))))))
         (let ((,global-mode
                 (make-instance 'global-mode
                                :name ',mode
                                :parent ',parent
                                :keymap ,keymap
                                :enable-hook ,enable-hook
                                :disable-hook ,disable-hook)))
           (setf (get ',mode 'global-mode) ,global-mode)
           (pushnew ',mode *global-mode-list*)
           (when (null *current-global-mode*)
             (setf *current-global-mode* ,global-mode)))
         (define-command (,mode (:class-name ,command-class-name)) () ()
           (change-global-mode ',mode))))))
