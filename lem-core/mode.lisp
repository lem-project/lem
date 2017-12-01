(in-package :lem)

(export '(major-mode
          mode-name
          mode-keymap
          mode-syntax-table
          current-mode-keymap
          find-mode-from-name
          mode-active-p
          toggle-minor-mode
          define-major-mode
          define-minor-mode
          change-buffer-mode
          define-global-mode))

(defvar *mode-list* nil)

(macrolet ((def (name)
             `(progn
                (defun ,name (mode)
                  (get mode ',name))
                (defun (setf ,name) (new-val mode)
                  (setf (get mode ',name) new-val)))))
  (def mode-name)
  (def mode-keymap)
  (def mode-syntax-table))

(defun current-mode-keymap ()
  (mode-keymap (buffer-major-mode (current-buffer))))

(defun (setf current-mode-keymap) (new-keymap)
  (setf (mode-keymap (buffer-major-mode (current-buffer))) new-keymap))

(defun find-mode-from-name (mode-name)
  (find-if #'(lambda (mode)
               (string-equal mode-name (mode-name mode)))
           *mode-list*))

(defun mode-active-p (buffer mode)
  (or (eq mode (buffer-major-mode buffer))
      (find mode (buffer-minor-modes buffer))))

(defun enable-minor-mode (minor-mode)
  (pushnew minor-mode (buffer-minor-modes (current-buffer))))

(defun disable-minor-mode (minor-mode)
  (setf (buffer-minor-modes (current-buffer))
        (delete minor-mode (buffer-minor-modes (current-buffer)))))

(defun toggle-minor-mode (minor-mode)
  (if (mode-active-p (current-buffer) minor-mode)
      (disable-minor-mode minor-mode)
      (enable-minor-mode minor-mode)))

(defmacro define-major-mode (major-mode
                             parent-mode
                             (&key name keymap syntax-table)
                             &body body)
  `(progn
     (pushnew ',major-mode *mode-list*)
     (setf (mode-name ',major-mode) ,name)
     ,@(cond (keymap
              `((defvar ,keymap (make-keymap :name ',keymap))
                (setf (mode-keymap ',major-mode) ,keymap)
                ,(when parent-mode
                   `(setf (keymap-parent ,keymap)
                          (mode-keymap ',parent-mode)))))
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
     (define-command ,major-mode () ()
       (clear-editor-local-variables (current-buffer))
       ,(when parent-mode `(,parent-mode))
       (setf (buffer-major-mode (current-buffer)) ',major-mode)
       (setf (buffer-syntax-table (current-buffer)) (mode-syntax-table ',major-mode))
       ,@body)))

(defmacro define-minor-mode (minor-mode (&key name (keymap nil keymapp)) &body body)
  `(progn
     (pushnew ',minor-mode *mode-list*)
     (setf (mode-name ',minor-mode) ,name)
     ,@(when keymapp
         `((defvar ,keymap (make-keymap :name ',keymap))
           (setf (mode-keymap ',minor-mode) ,keymap)))
     (define-command ,minor-mode (&rest args) ("P")
       (cond ((null args)
              (toggle-minor-mode ',minor-mode))
             ((consp (car args))
              (if (null (caar args))
                  (toggle-minor-mode ',minor-mode)
                  (enable-minor-mode ',minor-mode)))
             ((car args)
              (enable-minor-mode ',minor-mode))
             (t
              (disable-minor-mode ',minor-mode)))
       ,@body)))

(defun change-buffer-mode (buffer mode &rest args)
  (save-excursion
    (setf (current-buffer) buffer)
    (apply mode args))
  buffer)


(defvar *global-mode-list* '())
(defvar *current-global-mode* nil)

(defclass global-mode ()
  ((name :initarg :name :accessor global-mode-name)
   (parent :initarg :parent :accessor global-mode-parent)
   (keymap :initarg :keymap :accessor global-mode-keymap)
   (on-hook :initarg :on-hook :accessor global-mode-on-hook)
   (off-hook :initarg :off-hook :accessor global-mode-off-hook)))

(defun current-global-mode ()
  (if (symbolp *current-global-mode*)
      (setf *current-global-mode*
            (get *current-global-mode* 'global-mode))
      *current-global-mode*))

(defun change-global-mode (mode)
  (flet ((call (fun)
           (unless (null fun)
             (alexandria:when-let ((fun (alexandria:ensure-function fun)))
               (funcall fun)))))
    (let ((global-mode (get mode 'global-mode)))
      (check-type global-mode global-mode)
      (when global-mode
        (when *current-global-mode*
          (call (global-mode-off-hook *current-global-mode*)))
        (setf *current-global-mode* global-mode)
        (call (global-mode-on-hook global-mode))))))

(defmacro define-global-mode (mode parent (&key keymap on-hook off-hook))
  (alexandria:with-gensyms (parent-mode)
    `(progn
       ,@(when keymap
           `((defvar ,keymap (make-keymap :name ',keymap))
             (alexandria:when-let ((,parent-mode
                                    ,(when parent
                                       `(get ',parent 'global-mode))))
               (setf (keymap-parent ,keymap)
                     (global-mode-keymap ,parent-mode)))))
       (setf (get ',mode 'global-mode)
             (make-instance 'global-mode
                            :name ',mode
                            :parent ',parent
                            :keymap ,keymap
                            :on-hook ,on-hook
                            :off-hook ,off-hook))
       (pushnew ',mode *global-mode-list*)
       (define-command ,mode () ()
         (change-global-mode ',mode))
       (when (null *current-global-mode*)
         (setf *current-global-mode* ',mode)))))
