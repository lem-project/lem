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
