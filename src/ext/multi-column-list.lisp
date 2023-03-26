(defpackage :lem/multi-column-list
  (:use :cl :lem)
  (:export :multi-column-list
           :multi-column-list-item
           :select-item
           :delete-item
           :row-values
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
  (quit-multi-column-list))

(define-command multi-column-list/down () ()
  (popup-menu-down))

(define-command multi-column-list/up () ()
  (popup-menu-up))

(define-command multi-column-list/first () ()
  (popup-menu-first))

(define-command multi-column-list/last () ()
  (popup-menu-last))

(define-command multi-column-list/select () ()
  (popup-menu-select))

(define-command multi-column-list/mark-and-down () ()
  (mark-current-item)
  (multi-column-list/down))

(define-command multi-column-list/up-and-mark () ()
  (multi-column-list/up)
  (mark-current-item))

(define-command multi-column-list/delete-items () ()
  (delete-marked-items))

;;
(defgeneric select-item (component item)
  (:method (component item)))

(defgeneric delete-item (component item)
  (:method (component item)))

(defgeneric row-values (item)
  (:method :around (item)
    (append (mapcar #'princ-to-string (call-next-method))
            (list (if (multi-column-list-item-mark-p item)
                      "✔ "
                      "  ")))))

(defclass multi-column-list ()
  ((columns :initarg :columns
            :reader multi-column-list-columns)
   (items :initarg :items
          :accessor multi-column-list-items)
   (print-spec :accessor multi-column-list-print-spec)
   (use-mark :initform nil
             :initarg :use-mark
             :reader multi-column-list-use-mark-p)))

(defmethod multi-column-list-columns :around ((multi-column-list multi-column-list))
  (append (call-next-method) (list "")))

(defclass multi-column-list-item ()
  ((mark :initform nil
          :accessor multi-column-list-item-mark-p)))

(defclass print-spec ()
  ((multi-column-list :initarg :multi-column-list
                      :reader print-spec-multi-column-list)
   (column-width-list :initarg :column-width-list
                      :reader print-spec-column-width-list)))

(defmethod lem/popup-window:write-header ((print-spec print-spec) point)
  (with-point ((start point))
    (loop :for width :in (print-spec-column-width-list print-spec)
          :for column-header :in (multi-column-list-columns
                                  (print-spec-multi-column-list print-spec))
          :do (insert-string point " ")
              (let ((column (point-column point)))
                (insert-string point column-header)
                (move-to-column point (+ column width) t)))
    (put-text-property start point :attribute (make-attribute :underline-p t))))

(defmethod lem/popup-window:apply-print-spec ((print-spec print-spec) point item)
  (check-type item multi-column-list-item)
  (loop :for value :in (row-values item)
        :for width :in (print-spec-column-width-list print-spec)
        :do (insert-string point " ")
            (let ((column (point-column point)))
              (insert-string point value)
              (move-to-column point (+ column width) t))))

(defun compute-column-width-list (multi-column-list)
  (let ((width-matrix
          (loop :for row :in (cons (multi-column-list-columns multi-column-list)
                                   (mapcar #'row-values
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
    (lem:display-popup-menu (multi-column-list-items component)
                            :print-spec print-spec
                            :action-callback (lambda (item)
                                               (select-item component item))
                            :style '(:gravity :center)
                            :max-display-items 100)
    (setf (current-window)
          (lem/popup-window::popup-menu-window
           (lem/popup-window:find-popup-menu :parent-window (current-window))))
    (setf (window-multi-column-list (current-window)) component)
    (multi-column-list-mode t)
    (popup-menu-first)))

(defmethod quit ((component multi-column-list))
  (quit-multi-column-list))

(defun quit-multi-column-list ()
  (when (lem/popup-window:find-popup-menu :current-window (current-window))
    (multi-column-list-mode nil)
    (setf (current-window) (window-parent (current-window)))
    (popup-menu-quit)))

(defun update (multi-column-list)
  (popup-menu-update (multi-column-list-items multi-column-list)
                     :print-spec (multi-column-list-print-spec multi-column-list)
                     :max-display-items 100
                     :keep-focus t))

(defun mark-current-item ()
  (let ((multi-column-list (current-multi-column-list)))
    (when (multi-column-list-use-mark-p multi-column-list)
      (let ((item (lem/popup-window:get-focus-item
                   (lem/popup-window:find-popup-menu :current-window (current-window)))))
        (setf (multi-column-list-item-mark-p item)
              (not (multi-column-list-item-mark-p item))))
      (update multi-column-list))))

(defun mark-items ()
  (remove-if-not #'multi-column-list-item-mark-p
                 (multi-column-list-items (current-multi-column-list))))

(defun delete-marked-items ()
  (let* ((multi-column-list (current-multi-column-list))
         (whole-items (multi-column-list-items multi-column-list)))
    (dolist (item (mark-items))
      (delete-item multi-column-list item)
      (setf whole-items
            (delete item whole-items)))
    (setf (multi-column-list-items multi-column-list) whole-items)
    (update multi-column-list)))
