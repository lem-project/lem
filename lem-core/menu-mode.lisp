(defpackage :lem.menu-mode
  (:use :cl :lem)
  (:export :menu
           :display-menu
           :update-menu
           :menu-change-buffer
           :menu-delete-buffer))
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
(define-key *menu-mode-keymap* "d" 'menu-delete)
(define-key *menu-mode-keymap* "C-m" 'menu-select-this-window)
(define-key *menu-mode-keymap* "C-o" 'menu-select-other-window)
(define-key *menu-mode-keymap* "o" 'menu-select-switch-other-window)
(define-key *menu-mode-keymap* "n" 'menu-next-line)
(define-key *menu-mode-keymap* "p" 'menu-previous-line)
(define-key *menu-mode-keymap* "m" 'menu-mark-and-next-line)
(define-key *menu-mode-keymap* "u" 'menu-unmark-and-next-line)
(define-key *menu-mode-keymap* "U" 'menu-unmark-and-previous-line)
(define-key *menu-mode-keymap* "g" 'menu-update)

(defclass menu ()
  ((name
    :initform nil
    :accessor menu-name)
   (columns
    :initarg :columns
    :accessor menu-columns)
   (column-function
    :initarg :column-function
    :initform nil
    :reader menu-column-function)
   (items
    :initarg :items
    :initform nil
    :accessor menu-items)
   (origin-items
    :accessor menu-origin-items)
   (update-items-function
    :initarg :update-items-function
    :reader menu-update-items-function)
   (callback
    :initarg :callback
    :initform nil
    :reader menu-callback)
   ))

(defmethod initialize-instance :around ((menu menu) &rest initargs &key items)
  (setf (menu-origin-items menu) items)
  (apply #'call-next-method menu initargs))

(defun update-items (menu)
  (let* ((columns (menu-columns menu))
         (ncolumns (length columns))
         (items (menu-origin-items menu))
         (column-function (menu-column-function menu)))
    (when column-function
      (setf items (mapcar column-function items)))
    (setf items
          (mapcar (lambda (item)
                    (setf item (uiop:ensure-list item))
                    (let ((n (length item)))
                      (if (< n ncolumns)
                          (append item (make-list (- ncolumns n) :initial-element nil))
                          item)))
                  items))
    (setf (menu-items menu) items)))

(defun compute-columns (menu)
  (let ((width-vector (make-array (length (menu-columns menu))
                                  :initial-contents (mapcar #'length
                                                            (menu-columns menu)))))
    (dolist (item (menu-items menu))
      (loop :for i :from 0
            :for object :in item
            :do (setf (aref width-vector i)
                      (max (aref width-vector i)
                           (length (princ-to-string object))))))
    (loop :for width :across width-vector
          :for column := (+ width 1) :then (+ column width 1)
          :collect column)))

(defun update-menu (menu items)
  (lem-if:update-menu (implementation) menu items))

(defun display-menu (menu &key name)
  (lem-if:display-menu (implementation) menu name))

(defmethod lem-if:display-menu (implementation menu name)
  (setf (menu-name menu) name)
  (update-items menu)
  (let* ((columns (compute-columns menu))
         (buffer (make-buffer (format nil "*~A*" name)))
         (p (buffer-point buffer)))
    (mapc #'delete-overlay (buffer-value buffer 'mark-overlays))
    (setf (buffer-value buffer 'mark-overlays) nil)
    (setf (buffer-value buffer 'menu) menu)
    (change-buffer-mode buffer 'menu-mode)
    (setf (variable-value 'truncate-lines :buffer buffer) nil)
    (setf (buffer-read-only-p buffer) t)
    (let ((window (display-buffer buffer)))
      (with-current-window window
        (with-buffer-read-only buffer nil
          (erase-buffer buffer)
          (remove-text-property (buffer-start-point buffer) (buffer-end-point buffer) :item)
          (with-point ((start p))
            (loop :for str :in (menu-columns menu)
                  :for column :in columns
                  :do (insert-string p str)
                      (move-to-column p column t))
            (move-to-column p (1- (window-width window)) t)
            (put-text-property start p :attribute 'head-line-attribute))
          (loop :for item :in (menu-items menu)
                :for origin-item :in (menu-origin-items menu)
                :do (insert-character p #\newline)
                    (with-point ((start p))
                      (loop :for object :in item
                            :for column :in columns
                            :do (insert-string p (princ-to-string object))
                                (move-to-column p column t))
                      (put-text-property start p :item origin-item)))
          (move-to-line (buffer-point buffer) 2))))))

(defmethod lem-if:update-menu (implementation menu items)
  (setf (menu-origin-items menu) items)
  (display-menu menu :name (menu-name menu)))

(defun menu-change-buffer (menu buffer)
  (declare (ignore menu))
   buffer)

(defun menu-delete-buffer (menu buffer)
  (declare (ignore menu))
  (kill-buffer buffer)
  :redraw)

(defun menu-select-1 (&key (set-buffer #'switch-to-buffer)
                           (callback
                            #'(lambda (x) (getf (menu-callback x) :select)))
                           marked)
  (alexandria:when-let* ((menu (buffer-value (current-buffer) 'menu))
                         (fn (funcall callback menu))
                         (items (menu-current-items :marked marked)))
    (loop :with cb := (current-buffer)
          :with cw := (current-window)
          :with redraw
          :with close
          :with buffer
          :for item :in items
          :for result := (funcall fn menu item)
          :do (cond ((bufferp result) (setf buffer result))
                    ((eql result :redraw) (setf redraw t))
                    ((eql result :close)  (setf close t)))
          :finally
          (progn
            (when buffer
              (funcall set-buffer buffer))
            (when redraw
              (let ((items (funcall (menu-update-items-function menu))))
                (update-menu menu items)))
            (when (and close
                       (equal (current-buffer) cb)
                       (equal (current-window) cw))
              (quit-window))))))

(define-command menu-select-this-window () ()
  (menu-select-1))

(define-command menu-select-other-window () ()
  (menu-select-1 :set-buffer #'pop-to-buffer))

(define-command menu-select-switch-other-window () ()
  (menu-select-1 :set-buffer (lambda (buffer)
                               (setf (current-window)
                                     (pop-to-buffer buffer)))))

(define-command menu-delete () ()
  (menu-select-1 :callback #'(lambda (x) (getf (menu-callback x) :delete)) :marked t))

(define-command menu-next-line (n) ("p")
  (line-offset (current-point) n))

(define-command menu-previous-line (n) ("p")
  (alexandria:when-let ((menu (buffer-value (current-buffer) 'menu)))
    (dotimes (_ n t)
      (when (>= 2 (line-number-at-point (current-point)))
        (return))
      (line-offset (current-point) -1))))

(defun find-overlay-marked-line (p)
  (dolist (ov (buffer-value p 'mark-overlays))
    (when (and (point<= (overlay-start ov) p)
               (point<= p (overlay-end ov)))
      (return ov))))

(defun marked-line-p (p)
  (not (null (find-overlay-marked-line p))))

(defun menu-current-items (&key marked)
  (or (and marked
           (mapcar (lambda (x) (text-property-at (overlay-start x) :item))
                   (buffer-value (current-buffer) 'mark-overlays)))
      (let ((item (text-property-at (current-point) :item)))
        (and item
             (list item)))))

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

(define-command menu-update () ()
  (alexandria:when-let* ((menu (buffer-value (current-buffer) 'menu))
                         (fn (menu-update-items-function menu)))
    (let ((items (funcall fn)))
      (update-menu menu items))))
