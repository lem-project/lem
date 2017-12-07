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
          change-buffer-mode))

(defvar *mode-list* '())
(defvar *global-minor-mode-list* '())

(macrolet ((def (name)
             `(progn
                (defun ,name (mode)
                  (get mode ',name))
                (defun (setf ,name) (new-val mode)
                  (setf (get mode ',name) new-val)))))
  (def mode-name)
  (def mode-keymap)
  (def mode-syntax-table)
  (def mode-enable-hook)
  (def mode-disable-hook))

(defun current-mode-keymap ()
  (mode-keymap (buffer-major-mode (current-buffer))))

(defun (setf current-mode-keymap) (new-keymap)
  (setf (mode-keymap (buffer-major-mode (current-buffer))) new-keymap))

(defun find-mode-from-name (mode-name)
  (find-if #'(lambda (mode)
               (string-equal mode-name (mode-name mode)))
           *mode-list*))

(defun global-minor-mode-p (mode)
  (get mode 'global-minor-mode-p))

(defun mode-active-p (buffer mode)
  (or (eq mode (buffer-major-mode buffer))
      (find mode (buffer-minor-modes buffer))
      (find mode *global-minor-mode-list*)))

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
                             (&key name keymap syntax-table)
                             &body body)
  `(progn
     (pushnew ',major-mode *mode-list*)
     (setf (mode-name ',major-mode) ,name)
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
     (define-command ,major-mode () ()
       (clear-editor-local-variables (current-buffer))
       ,(when parent-mode `(,parent-mode))
       (setf (buffer-major-mode (current-buffer)) ',major-mode)
       (setf (buffer-syntax-table (current-buffer)) (mode-syntax-table ',major-mode))
       ,@body)))

(defmacro define-minor-mode (minor-mode
                             (&key name (keymap nil keymapp) global enable-hook disable-hook)
                             &body body)
  `(progn
     (pushnew ',minor-mode *mode-list*)
     ,(when global
        `(setf (get ',minor-mode 'global-minor-mode-p) t))
     (setf (mode-name ',minor-mode) ,name)
     ,@(when keymapp
         `((defvar ,keymap (make-keymap :name ',keymap))
           (setf (mode-keymap ',minor-mode) ,keymap)))
     (setf (mode-enable-hook ',minor-mode) ,enable-hook)
     (setf (mode-disable-hook ',minor-mode) ,disable-hook)
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
