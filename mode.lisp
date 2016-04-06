;; -*- Mode: LISP; Package: LEM -*-

(in-package :lem)

(export '(major-mode
          mode-name
          mode-keymap
          mode-syntax-table
          current-mode-keymap
          current-syntax
          mode-find-keybind
          find-mode-from-name
          toggle-minor-mode
          define-major-mode
          define-minor-mode
          fundamental-mode))

(defvar *mode-list* nil)

(defun major-mode ()
  (buffer-major-mode (current-buffer)))

(defun (setf major-mode) (new-val)
  (setf (buffer-major-mode (current-buffer)) new-val))

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

(defun find-mode-from-name (mode-name)
  (find-if #'(lambda (mode)
               (equal (string-downcase mode-name)
                      (string-downcase (mode-name mode))))
           *mode-list*))

(defun toggle-minor-mode (minor-mode)
  (if (member minor-mode (buffer-minor-modes))
      (setf (buffer-minor-modes)
            (delete minor-mode (buffer-minor-modes)))
      (push minor-mode (buffer-minor-modes))))

(defmacro define-major-mode (major-mode
                             parent-mode
                             (&key name keymap-var syntax-table)
                             &body body)
  `(progn
     (push ',major-mode *mode-list*)
     (setf (mode-name ',major-mode) ,name)
     (export '(,(symb major-mode "-HOOK")))
     ,@(cond (keymap-var
              `((defvar ,keymap-var (make-keymap))
                (setf (mode-keymap ',major-mode) ,keymap-var)
                ,(when parent-mode
                   `(setf (keymap-parent ,keymap-var)
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
                    (make-syntax-table))))
     (define-command ,major-mode () ()
       (clear-buffer-variables)
       ,(when parent-mode `(,parent-mode))
       (setf (major-mode) ',major-mode)
       (run-hooks ',(symb major-mode "-HOOK"))
       (prog1 (progn ,@body)
         (syntax-scan-buffer (current-buffer))))))

(defmacro define-minor-mode (minor-mode (&key name keymap-var) &body body)
  `(progn
     (push ',minor-mode *mode-list*)
     (setf (mode-name ',minor-mode) ,name)
     (defvar ,keymap-var (make-keymap))
     (setf (mode-keymap ',minor-mode) ,keymap-var)
     (define-command ,minor-mode (&rest args) ("P")
       (cond ((null args)
              (toggle-minor-mode ',minor-mode))
             ((car args)
              (unless (member ',minor-mode (buffer-minor-modes))
                (push ',minor-mode (buffer-minor-modes))))
             (t
              (setf (buffer-minor-modes)
                    (delete ',minor-mode (buffer-minor-modes)))))
       ,@body)))

(define-major-mode fundamental-mode nil
  (:name "fundamental"
   :keymap-var *global-keymap*))
