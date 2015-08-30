(in-package :lem)

(export '(major-mode
          mode-name
          mode-keymap
          mode-syntax-table
          current-mode-keymap
          current-syntax
          define-major-mode
          define-minor-mode))

(defun major-mode ()
  (or (buffer-major-mode (window-buffer))
      'fundamental-mode))

(defun (setf major-mode) (new-val)
  (setf (buffer-major-mode (window-buffer)) new-val))

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
  (mode-keymap (major-mode)))

(defun (setf current-mode-keymap) (new-keymap)
  (setf (mode-keymap (major-mode)) new-keymap))

(defun current-syntax ()
  (mode-syntax-table (major-mode)))

(defun mode-find-keybind (key)
  (dolist (mode (buffer-minor-modes))
    (let ((result (keymap-find-command (mode-keymap mode) key)))
      (when result
        (return-from mode-find-keybind result))))
  (keymap-find-command (current-mode-keymap) key))

(defun toggle-minor-mode (minor-mode)
  (if (member minor-mode (buffer-minor-modes))
      (setf (buffer-minor-modes)
            (delete minor-mode (buffer-minor-modes)))
      (push minor-mode (buffer-minor-modes))))

(defmacro define-major-mode (major-mode &key name keymap syntax-table)
  `(progn
     (setf (mode-name ',major-mode) ,name)
     (setf (mode-keymap ',major-mode) ,keymap)
     (setf (mode-syntax-table ',major-mode) ,syntax-table)
     (define-command ,major-mode () ()
       (setf (major-mode) ',major-mode)
       (buffer-empty-plist (current-buffer))
       (syntax-scan-buffer (current-buffer)))))

(defmacro define-minor-mode (minor-mode &key name keymap)
  `(progn
     (setf (mode-name ',minor-mode) ,name)
     (setf (mode-keymap ',minor-mode) ,keymap)
     (define-command ,minor-mode () ()
       (toggle-minor-mode ',minor-mode))))

(define-major-mode fundamental-mode
  :name "fundamental-mode"
  :keymap *global-keymap*
  :syntax-table (make-syntax-table))
