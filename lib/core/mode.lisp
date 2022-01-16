(in-package :lem)

(export '(major-mode
          mode-name
          mode-description
          mode-keymap
          mode-syntax-table
          mode-hook
          is-major
          current-mode-keymap
          find-mode-from-name
          mode-active-p
          toggle-minor-mode
          define-major-mode
          define-minor-mode
          change-buffer-mode
          define-global-mode
          change-global-mode-keymap))

(defvar *mode-list* '())
(defvar *global-minor-mode-list* '())

(macrolet ((def (name)
             `(progn
                (defgeneric ,name (mode))
                (defgeneric (setf ,name) (new-val mode))
                (defmethod ,name (mode)
                  (get mode ',name))
                (defmethod (setf ,name) (new-val mode)
                  (setf (get mode ',name) new-val)))))
  (def mode-name)
  (def mode-description)
  (def mode-keymap)
  (def mode-syntax-table)
  (def mode-enable-hook)
  (def mode-disable-hook)
  (def mode-hook))

(defun current-mode-keymap ()
  (mode-keymap (buffer-major-mode (current-buffer))))

(defun (setf current-mode-keymap) (new-keymap)
  (setf (mode-keymap (buffer-major-mode (current-buffer))) new-keymap))

(defun find-mode-from-name (mode-name)
  (find-if #'(lambda (mode)
               (string-equal mode-name (mode-name mode)))
           *mode-list*))

(defun find-mode (mode)
  (find mode *mode-list*))

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

(defun is-major (mode)
  (get mode 'is-major))

(defmacro define-major-mode (major-mode
                             parent-mode
                             (&key name description keymap syntax-table mode-hook)
                             &body body)
  `(progn
     ,@(when mode-hook
         `((defvar ,mode-hook '())
           (setf (mode-hook ',major-mode) ',mode-hook)))
     (pushnew ',major-mode *mode-list*)
     (setf (mode-name ',major-mode) ,name)
     (setf (get ',major-mode 'is-major) t)
     ,@(when description
         `((setf (mode-description ',major-mode)
                ,description)))
     ,@(cond (keymap
              `((defvar ,keymap (make-keymap :name ',keymap
                                             :parent ,(when parent-mode
                                                        `(mode-keymap ',parent-mode))))
                (setf (mode-keymap ',major-mode) ,keymap)))
             (parent-mode
              `((setf (mode-keymap ',major-mode)
                      (mode-keymap ',parent-mode))))
             (t
              `((setf (mode-keymap ',major-mode) nil))))
     ,(cond (syntax-table
             `(setf (mode-syntax-table ',major-mode)
                    ,syntax-table))
            (parent-mode
             `(setf (mode-syntax-table ',major-mode)
                    (mode-syntax-table ',parent-mode)))
            (t
             `(setf (mode-syntax-table ',major-mode)
                    (fundamental-syntax-table))))
     (export ',major-mode)
     (define-command ,major-mode () ()
       (clear-editor-local-variables (current-buffer))
       ,(when parent-mode `(,parent-mode))
       (setf (buffer-major-mode (current-buffer)) ',major-mode)
       (setf (buffer-syntax-table (current-buffer)) (mode-syntax-table ',major-mode))
       ,@body
       ,(when mode-hook
          `(run-hooks ,mode-hook)))))

(defmacro define-minor-mode (minor-mode
                             (&key name description (keymap nil keymapp) global enable-hook disable-hook)
                             &body body)
  `(progn
     (pushnew ',minor-mode *mode-list*)
     ,(when global
        `(setf (get ',minor-mode 'global-minor-mode-p) t))
     (setf (mode-name ',minor-mode) ,name)
     ,@(when description
         `((setf (mode-description ',minor-mode)
                 ,description)))
     ,@(when keymapp
         `((defvar ,keymap (make-keymap :name ',keymap))
           (setf (mode-keymap ',minor-mode) ,keymap)))
     (setf (mode-enable-hook ',minor-mode) ,enable-hook)
     (setf (mode-disable-hook ',minor-mode) ,disable-hook)
     (export ',minor-mode)
     (define-command ,minor-mode (&optional (arg nil arg-p)) ("p")
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
       ,@body)))

(defun change-buffer-mode (buffer mode &rest args)
  (save-excursion
    (setf (current-buffer) buffer)
    (apply mode args))
  buffer)

(defvar *global-mode-list* '())
(defvar *current-global-mode* nil)

(defclass global-mode ()
  ((name :initarg :name :accessor mode-name)
   (parent :initarg :parent :accessor mode-parent)
   (keymap :initarg :keymap :accessor mode-keymap)
   (enable-hook :initarg :enable-hook :accessor mode-enable-hook)
   (disable-hook :initarg :disable-hook :accessor mode-disable-hook)))

(defun current-global-mode ()
  (if (symbolp *current-global-mode*)
      (setf *current-global-mode*
            (get *current-global-mode* 'global-mode))
      *current-global-mode*))

(defun change-global-mode-keymap (mode keymap)
  (setf (mode-keymap (get mode 'global-mode)) keymap))

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
         (export ',mode)
         (when (null *current-global-mode*)
           (setf *current-global-mode* ,global-mode)))
       (define-command ,mode () ()
         (change-global-mode ',mode)))))
