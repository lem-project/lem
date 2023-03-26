(defpackage :lem/multi-column-list
  (:use :cl :lem)
  (:export :multi-column-list
           :multi-column-list-item
           :select-item
           :delete-item
           :map-columns
           :display
           :update
           :quit))
(in-package :lem/multi-column-list)

(defvar *multi-column-list-mode-keymap*
  (make-keymap :undef-hook 'multi-column-list/default))

(define-minor-mode multi-column-list-mode
    (:name "multi-column-list"
     :keymap *multi-column-list-mode-keymap*))

(define-key *multi-column-list-mode-keymap* 'keyboard-quit 'multi-column-list/quit)
(define-key *multi-column-list-mode-keymap* 'escape 'multi-column-list/quit)
(define-key *multi-column-list-mode-keymap* 'next-line 'multi-column-list/down)
(define-key *multi-column-list-mode-keymap* 'previous-line 'multi-column-list/up)
(define-key *multi-column-list-mode-keymap* 'move-to-end-of-buffer 'multi-column-list/last)
(define-key *multi-column-list-mode-keymap* 'move-to-beginning-of-buffer 'multi-column-list/first)
(define-key *multi-column-list-mode-keymap* "Return" 'multi-column-list/select)
(define-key *multi-column-list-mode-keymap* "Space" 'multi-column-list/mark-and-down)
(define-key *multi-column-list-mode-keymap* "M-Space" 'multi-column-list/up-and-mark)
(define-key *multi-column-list-mode-keymap* "C-k" 'multi-column-list/delete-items)

(define-command multi-column-list/default () ()
  )

(define-command multi-column-list/quit () ()
  (quit (current-multi-column-list)))

(define-command multi-column-list/down () ()
  (popup-menu-down (multi-column-list-popup-menu (current-multi-column-list))))

(define-command multi-column-list/up () ()
  (popup-menu-up (multi-column-list-popup-menu (current-multi-column-list))))

(define-command multi-column-list/first () ()
  (popup-menu-first (multi-column-list-popup-menu (current-multi-column-list))))

(define-command multi-column-list/last () ()
  (popup-menu-last (multi-column-list-popup-menu (current-multi-column-list))))

(define-command multi-column-list/select () ()
  (popup-menu-select (multi-column-list-popup-menu (current-multi-column-list))))

(define-command multi-column-list/mark-and-down () ()
  (mark-current-item (current-multi-column-list))
  (multi-column-list/down))

(define-command multi-column-list/up-and-mark () ()
  (multi-column-list/up)
  (mark-current-item (current-multi-column-list)))

(define-command multi-column-list/delete-items () ()
  (delete-marked-items (current-multi-column-list)))

;;
(defgeneric select-item (component item))
(defgeneric delete-item (component item))
(defgeneric map-columns (component item))

(defclass multi-column-list-item ()
  ((mark :initform nil
         :accessor multi-column-list-item-mark-p)))

(defclass default-multi-column-list-item (multi-column-list-item)
  ((value :initarg :value
          :reader default-multi-column-list-item-value)))

(defun wrap (value)
  (if (typep value 'multi-column-list-item)
      value
      (make-instance 'default-multi-column-list-item :value value)))

(defun unwrap (value)
  (if (typep value 'default-multi-column-list-item)
      (default-multi-column-list-item-value value)
      value))

(defmethod select-item :around (component (item default-multi-column-list-item))
  (call-next-method component (unwrap item)))

(defmethod delete-item :around (component (item default-multi-column-list-item))
  (call-next-method component (unwrap item)))

(defmethod map-columns :around (component (item default-multi-column-list-item))
  (call-next-method component (unwrap item)))

(defclass multi-column-list ()
  ((columns :initarg :columns
            :initform nil
            :reader multi-column-list-columns)
   (items :initarg :items
          :accessor multi-column-list-items)
   (select-callback :initarg :select-callback
                    :initform nil
                    :reader multi-column-list-select-callback)
   (delete-callback :initarg :delete-callback
                    :initform nil
                    :accessor multi-column-list-delete-callback)
   (column-function :initarg :column-function
                    :initform nil
                    :accessor multi-column-list-column-function)
   (print-spec :accessor multi-column-list-print-spec)
   (popup-menu :accessor multi-column-list-popup-menu)
   (use-mark :initform nil
             :initarg :use-mark
             :reader multi-column-list-use-mark-p)))

(defmethod initialize-instance ((instance multi-column-list) &rest initargs &key items &allow-other-keys)
  (apply #'call-next-method
         instance
         :items (mapcar #'wrap items)
         initargs))

(defmethod map-columns :around ((component multi-column-list) item)
  (append (if (multi-column-list-use-mark-p component)
              (list (if (multi-column-list-item-mark-p item)
                        "✔ "
                        "  "))
              nil)
          (mapcar #'princ-to-string (call-next-method))))

(defmethod select-item ((component multi-column-list) item)
  (when (multi-column-list-select-callback component)
    (funcall (multi-column-list-select-callback component) component item)))

(defmethod delete-item ((component multi-column-list) item)
  (when (multi-column-list-delete-callback component)
    (funcall (multi-column-list-delete-callback component) component item)))

(defmethod map-columns ((component multi-column-list) item)
  (when (multi-column-list-column-function component)
    (funcall (multi-column-list-column-function component) component item)))

(defmethod multi-column-list-columns :around ((multi-column-list multi-column-list))
  (append (if (multi-column-list-use-mark-p multi-column-list)
              (list "")
              nil)
          (call-next-method)))

(defclass print-spec ()
  ((multi-column-list :initarg :multi-column-list
                      :reader print-spec-multi-column-list)
   (column-width-list :initarg :column-width-list
                      :reader print-spec-column-width-list)))

(defmethod lem/popup-menu:write-header ((print-spec print-spec) point)
  (let ((columns (multi-column-list-columns
                  (print-spec-multi-column-list print-spec))))
    (when columns
      (with-point ((start point))
        (loop :for width :in (print-spec-column-width-list print-spec)
              :for column-header :in columns
              :do (insert-string point " ")
                  (let ((column (point-column point)))
                    (insert-string point column-header)
                    (move-to-column point (+ column width) t)))
        (insert-string point " ")
        (put-text-property start point :attribute (make-attribute :underline-p t))))))

(defmethod lem/popup-menu:apply-print-spec ((print-spec print-spec) point item)
  (check-type item multi-column-list-item)
  (loop :for value :in (map-columns (print-spec-multi-column-list print-spec) item)
        :for width :in (print-spec-column-width-list print-spec)
        :do (insert-string point " ")
            (let ((column (point-column point)))
              (insert-string point value)
              (move-to-column point (+ column width) t)))
  (insert-string point " "))

(defun compute-column-width-list (multi-column-list)
  (let ((width-matrix
          (loop :for row :in (append (alexandria:when-let (columns (multi-column-list-columns multi-column-list))
                                       (list columns))
                                     (mapcar (lambda (item) (map-columns multi-column-list item))
                                             (multi-column-list-items multi-column-list)))
                :collect (loop :for value :in row
                               :collect (string-width value)))))
    (loop :repeat (length (first width-matrix))
          :for i :from 0
          :collect (loop :for width-list :in width-matrix :maximize (elt width-list i)))))

(defun window-multi-column-list (window)
  (window-parameter window 'multi-column-list))

(defun (setf window-multi-column-list) (value window)
  (setf (window-parameter window 'multi-column-list) value))

(defun current-multi-column-list ()
  (window-multi-column-list (current-window)))

(defmethod display ((component multi-column-list))
  (let ((print-spec (make-instance
                     'print-spec
                     :multi-column-list component
                     :column-width-list (compute-column-width-list component))))
    (setf (multi-column-list-print-spec component) print-spec)
    (let ((popup-menu
            (display-popup-menu (multi-column-list-items component)
                                :print-spec print-spec
                                :action-callback (lambda (item)
                                                   (select-item component item))
                                :style '(:gravity :center)
                                :max-display-items 100)))
      (setf (multi-column-list-popup-menu component) popup-menu)
      (setf (current-window)
            (lem/popup-menu::popup-menu-window popup-menu))
      (setf (window-multi-column-list (current-window)) component)
      (multi-column-list-mode t)
      (popup-menu-first popup-menu))))

(defmethod quit ((component multi-column-list))
  (let* ((popup-menu (multi-column-list-popup-menu component))
         (popup-window (lem/popup-menu::popup-menu-window popup-menu)))
    (when (eq (current-window) popup-window)
      (setf (current-window) (window-parent popup-window)))
    (popup-menu-quit popup-menu)))

(defmethod update ((component multi-column-list))
  (popup-menu-update (multi-column-list-popup-menu component)
                     (multi-column-list-items component)
                     :print-spec (multi-column-list-print-spec component)
                     :max-display-items 100
                     :keep-focus t))

(defun mark-current-item (multi-column-list)
  (when (multi-column-list-use-mark-p multi-column-list)
    (let ((item (lem/popup-menu:get-focus-item
                 (multi-column-list-popup-menu multi-column-list))))
      (setf (multi-column-list-item-mark-p item)
            (not (multi-column-list-item-mark-p item))))
    (update multi-column-list)))

(defun mark-items (multi-column-list)
  (remove-if-not #'multi-column-list-item-mark-p
                 (multi-column-list-items multi-column-list)))

(defun delete-marked-items (multi-column-list)
  (let ((whole-items (multi-column-list-items multi-column-list)))
    (dolist (item (mark-items multi-column-list))
      (delete-item multi-column-list item)
      (setf whole-items
            (delete item whole-items)))
    (setf (multi-column-list-items multi-column-list) whole-items)
    (update multi-column-list)))
