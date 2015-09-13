(in-package :lem)

(export '(major-mode
          mode-name
          mode-keymap
          mode-syntax-table
          current-mode-keymap
          current-syntax
          mode-find-keybind
          toggle-minor-mode
          define-major-mode
          define-minor-mode
          fundamental-mode))

(defun major-mode ()
  (buffer-major-mode (window-buffer)))

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

(defun mode-find-keybind (mode key)
  (keymap-find-keybind (mode-keymap mode) key))

(defun toggle-minor-mode (minor-mode)
  (if (member minor-mode (buffer-minor-modes))
      (setf (buffer-minor-modes)
            (delete minor-mode (buffer-minor-modes)))
      (push minor-mode (buffer-minor-modes))))

(defmacro define-major-mode (major-mode
                             parent-mode
                             (&key name keymap syntax-table)
                             &body body)
  `(progn
     (setf (mode-name ',major-mode) ,name)
     (setf (mode-keymap ',major-mode)
           ,(or keymap
                (when parent-mode
                  (mode-keymap parent-mode))))
     (setf (mode-syntax-table ',major-mode)
           ,(or syntax-table
                (when parent-mode
                  (mode-syntax-table parent-mode))))
     (define-command ,major-mode () ()
       ,(when parent-mode `(,parent-mode))
       (setf (major-mode) ',major-mode)
       (buffer-empty-plist (current-buffer))
       (syntax-scan-buffer (current-buffer))
       ,@body)))

(defmacro define-minor-mode (minor-mode &key name keymap)
  `(progn
     (setf (mode-name ',minor-mode) ,name)
     (setf (mode-keymap ',minor-mode) ,keymap)
     (define-command ,minor-mode (&rest args) ("P")
       (cond ((null args)
              (toggle-minor-mode ',minor-mode))
             ((car args)
              (unless (member ',minor-mode (buffer-minor-modes))
                (push ',minor-mode (buffer-minor-modes))))
             (t
              (setf (buffer-minor-modes)
                    (delete ',minor-mode (buffer-minor-modes))))))))

(define-major-mode fundamental-mode nil
  (:name "fundamental-mode"
   :keymap *global-keymap*
   :syntax-table (make-syntax-table)))
