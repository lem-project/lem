(defpackage :lem.menu-mode
  (:use :cl :lem)
  (:export :menu-mode
           :menu
           :menu-item
           :append-menu
           :append-menu-item
           :display-menu
           :menu-property-at
           :marked-menu-items))
(in-package :lem.menu-mode)

(define-attribute head-line-attribute
  (:light :background "gray85")
  (:dark :foreground "black" :background "gray85"))

(define-attribute mark-attribute
  (t :background "blue" :foreground "white" :underline-p t))

(define-major-mode menu-mode nil
    (:name "Menu"
     :keymap *menu-mode-keymap*))

(define-key *menu-mode-keymap* "q" 'quit-window)
(define-key *menu-mode-keymap* "C-m" 'menu-select-this-window)
(define-key *menu-mode-keymap* "C-o" 'menu-select-other-window)
(define-key *menu-mode-keymap* "o" 'menu-select-switch-other-window)
(define-key *menu-mode-keymap* "n" 'menu-next-line)
(define-key *menu-mode-keymap* "p" 'menu-previous-line)
(define-key *menu-mode-keymap* "m" 'menu-mark-and-next-line)
(define-key *menu-mode-keymap* "u" 'menu-unmark-and-next-line)
(define-key *menu-mode-keymap* "U" 'menu-unmark-and-previous-line)

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
   (select-callback
    :initarg :select-callback
    :initform nil
    :reader menu-item-select-callback)
   (plist
    :initform nil
    :accessor menu-item-plist)))

(defmethod initialize-instance :after ((menu-item menu-item) &rest initargs &key &allow-other-keys)
  (let ((plist '()))
    (loop :for (k v) :on initargs :by #'cddr
          :do (unless (member k '(:select-callback))
                (push v plist)
                (push k plist)))
    (setf (menu-item-plist menu-item) plist)))

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
    (mapc #'delete-overlay (buffer-value buffer 'mark-overlays))
    (setf (buffer-value buffer 'mark-overlays) nil)
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
                  :do (insert-string p str)
                      (move-to-column p column t))
            (move-to-column p (1- (window-width window)) t)
            (put-text-property start p :attribute 'head-line-attribute))
          (dolist (item (menu-items menu))
            (insert-character p #\newline)
            (with-point ((start p))
              (loop :for (string . attribute) :in (menu-item-elements item)
                    :for column :in columns
                    :do (insert-string p string :attribute attribute)
                        (move-to-column p column t))
              (put-text-property start p 'select-callback (menu-item-select-callback item))
              (put-text-property start p 'plist (menu-item-plist item))))
          (move-to-line (buffer-point buffer) 2))))))

(defun menu-property-at (point indicator)
  (with-point ((p point))
    (getf (text-property-at (line-start p) 'plist) indicator)))

(defun menu-select-1 (set-buffer-fn)
  (alexandria:when-let ((fn (text-property-at (current-point) 'select-callback)))
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
  (alexandria:when-let ((menu (buffer-value (current-buffer) '%menu)))
    (dotimes (_ n t)
      (when (and (menu-use-headline-p menu)
                 (>= 2 (line-number-at-point (current-point))))
        (return))
      (line-offset (current-point) -1))))

(defun find-overlay-marked-line (p)
  (dolist (ov (buffer-value p 'mark-overlays))
    (when (and (point<= (overlay-start ov) p)
               (point<= p (overlay-end ov)))
      (return ov))))

(defun marked-line-p (p)
  (not (null (find-overlay-marked-line p))))

(defun mark-line (p)
  (unless (marked-line-p p)
    (with-point ((start p)
                 (end p))
      (line-start start)
      (line-end end)
      (push (make-overlay start end 'mark-attribute)
            (buffer-value p 'mark-overlays)))))

(defun unmark-line (p)
  (alexandria:when-let ((ov (find-overlay-marked-line p)))
    (alexandria:deletef (buffer-value p 'mark-overlays) ov)
    (delete-overlay ov)))

(define-command menu-mark-and-next-line (n) ("p")
  (dotimes (_ n)
    (mark-line (current-point))
    (unless (menu-next-line 1)
      (return))))

(define-command menu-unmark-and-next-line (n) ("p")
  (dotimes (_ n)
    (unmark-line (current-point))
    (unless (menu-next-line 1)
      (return))))

(define-command menu-unmark-and-previous-line (n) ("p")
  (dotimes (_ n)
    (unless (menu-previous-line 1)
      (return))
    (unmark-line (current-point))))

(defun marked-menu-items (point indicator)
  (or (loop :for ov :in (buffer-value point 'mark-overlays)
            :collect (menu-property-at (overlay-start ov) indicator))
      (list (menu-property-at point indicator))))
