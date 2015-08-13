(in-package :lem)

(export '(major-mode
          mode-name
          define-major-mode
          fundamental-mode
          current-syntax))

(defun major-mode ()
  (or (buffer-major-mode (window-buffer))
      'fundamental-mode))

(defun mode-name (mode)
  (get mode 'mode-name))

(defun current-mode-keymap ()
  (get (major-mode) 'keymap))

(defun set-current-mode-keymap (keymap)
  (setf (get (major-mode) 'keymap) keymap))

(defun mode-find-keybind (key)
  (dolist (mode (buffer-minor-modes))
    (let ((result (keymap-find-command (get mode 'keymap) key)))
      (when result
        (return-from mode-find-keybind result))))
  (keymap-find-command (current-mode-keymap) key))

(defun set-major-mode (major-mode)
  (setf (buffer-major-mode (window-buffer)) major-mode))

(defun toggle-minor-mode (minor-mode)
  (if (member minor-mode (buffer-minor-modes))
    (setf (buffer-minor-modes)
          (delete minor-mode (buffer-minor-modes)))
    (push minor-mode (buffer-minor-modes))))

(defmacro define-major-mode (major-mode &key name keymap syntax-table)
  `(progn
    (setf (get ',major-mode 'mode-name) ,name)
    (setf (get ',major-mode 'keymap) ,keymap)
    (setf (get ',major-mode 'syntax-table) ,syntax-table)
    (define-command ,major-mode () ()
      (set-major-mode ',major-mode))))

(defmacro define-minor-mode (minor-mode &key name keymap)
  `(progn
     (setf (get ',minor-mode 'mode-name) ,name)
     (setf (get ',minor-mode 'keymap) ,keymap)
     (define-command ,minor-mode () ()
       (toggle-minor-mode ',minor-mode))))

(define-major-mode fundamental-mode
  :name "fundamental-mode"
  :keymap *global-keymap*
  :syntax-table (make-syntax-table))

(defun current-syntax ()
  (get (major-mode) 'syntax-table))
