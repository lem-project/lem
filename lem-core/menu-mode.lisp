(defpackage :lem.menu-mode
  (:use :cl :lem)
  (:export :menu-mode
           :menu
           :menu-item
           :append-menu
           :append-menu-item
           :display-menu
           :menu-property-at))
(in-package :lem.menu-mode)

(define-attribute head-line-attribute
  (:light :background "gray85")
  (:dark :foreground "black" :background "gray85"))

(define-major-mode menu-mode nil
    (:name "Menu"
     :keymap *menu-mode-keymap*))

(define-key *menu-mode-keymap* "q" 'quit-window)
(define-key *menu-mode-keymap* "C-m" 'menu-select-this-window)
(define-key *menu-mode-keymap* "C-o" 'menu-select-other-window)
(define-key *menu-mode-keymap* "o" 'menu-select-switch-other-window)
(define-key *menu-mode-keymap* "n" 'menu-next-line)
(define-key *menu-mode-keymap* "p" 'menu-previous-line)

(defclass menu ()
  ((buffer-name
    :initarg :buffer-name
    :reader menu-buffer-name)
   (columns
    :initarg :columns
    :accessor menu-columns)
   (items
    :initform nil
    :accessor menu-items)
   (use-headline-p
    :initform t
    :reader menu-use-headline-p)))

(defclass menu-item ()
  ((elements
    :initform nil
    :accessor menu-item-elements)
   (select-function
    :initarg :select-function
    :initform nil
    :reader menu-item-select-function)
   (plist
    :initarg :plist
    :initform nil
    :reader menu-item-plist)))

(defun append-menu (menu item)
  (setf (menu-items menu)
        (nconc (menu-items menu) (list item))))

(defun append-menu-item (item x &optional attribute)
  (setf (menu-item-elements item)
        (nconc (menu-item-elements item)
               (list (cons (princ-to-string x) attribute)))))

(defun compute-columns (menu)
  (let ((width-vector (make-array (length (menu-columns menu))
                                  :initial-contents (mapcar #'length
                                                            (menu-columns menu)))))
    (dolist (item (menu-items menu))
      (loop :for i :from 0
            :for (string . _) :in (menu-item-elements item)
            :do (setf (aref width-vector i)
                      (max (aref width-vector i)
                           (length string)))))
    (loop :for width :across width-vector
          :for column := (+ width 1) :then (+ column width 1)
          :collect column)))

(defun display-menu (menu &optional (mode 'menu-mode))
  (let* ((columns (compute-columns menu))
         (buffer (make-buffer (menu-buffer-name menu)))
         (p (buffer-point buffer)))
    (setf (buffer-value buffer '%menu) menu)
    (change-buffer-mode buffer mode)
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
              (put-text-property start p 'function (menu-item-select-function item))
              (put-text-property start p 'plist (menu-item-plist item))))
          (move-to-line (buffer-point buffer) 2))))))

(defun menu-property-at (point indicator)
  (with-point ((p point))
    (getf (text-property-at (line-start p) 'plist) indicator)))

(defun menu-select-1 (set-buffer-fn)
  (alexandria:when-let ((fn (text-property-at (current-point) 'function)))
    (funcall fn set-buffer-fn)))

(define-command menu-select-this-window () ()
  (menu-select-1 #'switch-to-buffer))

(define-command menu-select-other-window () ()
  (menu-select-1 #'pop-to-buffer))

(define-command menu-select-switch-other-window () ()
  (menu-select-1 (lambda (buffer)
                   (setf (current-window)
                         (pop-to-buffer buffer)))))

(define-command menu-next-line (n) ("p")
  (line-offset (current-point) n))

(define-command menu-previous-line (n) ("p")
  (let ((menu (buffer-value (current-buffer) '%menu)))
    (dotimes (_ n)
      (when (and (menu-use-headline-p menu)
                 (>= 2 (line-number-at-point (current-point))))
        (return))
      (line-offset (current-point) -1))))
