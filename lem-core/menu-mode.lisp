(defpackage :lem.menu-mode
  (:use :cl :lem)
  (:export :menu
           :menu-item
           :append-menu
           :append-menu-item
           :display-menu))
(in-package :lem.menu-mode)

(define-attribute head-line-attribute
  (:light :background "gray85")
  (:dark :background "gray20"))

(define-major-mode menu-mode nil
    (:name "Menu"
     :keymap *menu-mode-keymap*))

(define-key *menu-mode-keymap* "q" 'quit-window)
(define-key *menu-mode-keymap* "C-m" 'menu-select)

(defclass menu ()
  ((buffer-name
    :initarg :buffer-name
    :reader menu-buffer-name)
   (columns
    :initarg :columns
    :accessor menu-columns)
   (items
    :initform nil
    :accessor menu-items)))

(defclass menu-item ()
  ((elements
    :initform nil
    :accessor menu-item-elements)
   (select-function
    :initarg :select-function
    :initform nil
    :reader menu-item-select-function)))

(defun append-menu (menu item)
  (setf (menu-items menu)
        (nconc (menu-items menu) (list item))))

(defun append-menu-item (item x &optional attribute)
  (setf (menu-item-elements item)
        (nconc (menu-item-elements item)
               (list (cons (princ-to-string x) attribute)))))

(defun compute-columns (menu)
  (let ((width-vector (make-array (length (menu-columns menu))
                                  :initial-contents (mapcar #'string-width
                                                            (menu-columns menu)))))
    (dolist (item (menu-items menu))
      (loop :for i :from 0
            :for (string . _) :in (menu-item-elements item)
            :do (setf (aref width-vector i)
                      (max (aref width-vector i)
                           (string-width string)))))
    (loop :for width :across width-vector
          :for column := (+ width 1) :then (+ column width 1)
          :collect column)))

(defun display-menu (menu)
  (let* ((columns (compute-columns menu))
         (buffer (get-buffer-create (menu-buffer-name menu)))
         (p (buffer-point buffer)))
    (change-buffer-mode buffer 'menu-mode)
    (setf (variable-value 'truncate-lines :buffer buffer) nil)
    (setf (buffer-read-only-p buffer) t)
    (let ((window (display-buffer buffer)))
      (with-current-window window
        (with-buffer-read-only buffer nil
          (erase-buffer buffer)
          (with-point ((start p))
            (loop :for str :in (menu-columns menu)
                  :for column :in columns
                  :do
                  (insert-string p str)
                  (move-to-column p column t))
            (move-to-column p (1- (window-width window)) t)
            (put-text-property start p :attribute 'head-line-attribute))
          (dolist (item (menu-items menu))
            (insert-character p #\newline)
            (with-point ((start p))
              (loop :for (string . attribute) :in (menu-item-elements item)
                    :for column :in columns
                    :do
                    (insert-string p string :attribute attribute)
                    (move-to-column p column t))
              (put-text-property start p 'function (menu-item-select-function item))))
          (move-to-line (buffer-point buffer) 2))))))

(define-command menu-select () ()
  (let ((fn (text-property-at (current-point) 'function)))
    (when fn (funcall fn))))
