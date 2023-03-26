(defpackage :lem/multi-column-list
  (:use :cl :lem)
  (:export :multi-column-list
           :multi-column-list-item
           :select-item
           :delete-item
           :row-values
           :display
           :quit))
(in-package :lem/multi-column-list)

(defvar *multi-column-list-mode-keymap*
  (make-keymap :undef-hook 'multi-column-list/default))

(define-minor-mode multi-column-list-mode
    (:name "multi-column-list"
     :keymap *multi-column-list-mode-keymap*))

(define-key *multi-column-list-mode-keymap* 'keyboard-quit 'multi-column-list/quit)
(define-key *multi-column-list-mode-keymap* 'lem::escape 'multi-column-list/quit)
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
(defgeneric select-item (component item))
(defgeneric delete-item (component item))
(defgeneric row-values (item))

(defmethod row-values :around (item)
  (append (mapcar #'princ-to-string (call-next-method))
          (list (if (multi-column-list-item-check-p item)
                    "✔ "
                    "  "))))

(defclass multi-column-list ()
  ((columns :initarg :columns
            :reader multi-column-list-columns)
   (items :initarg :items
          :accessor multi-column-list-items)
   (print-spec :accessor multi-column-list-print-spec)))

(defmethod multi-column-list-columns :around ((multi-column-list multi-column-list))
  (append (call-next-method) (list "")))

(defclass multi-column-list-item ()
  ((check :initform nil
          :accessor multi-column-list-item-check-p)))

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

(defun current-multi-column-list ()
  (let* ((popup-menu (lem/popup-window:find-popup-menu :parent-window (current-window)))
         (buffer (lem/popup-window::popup-menu-buffer popup-menu)))
    (buffer-value buffer 'multi-column-list)))

(defun (setf current-multi-column-list) (multi-column-list)
  (let* ((popup-menu (lem/popup-window:find-popup-menu :parent-window (current-window)))
         (buffer (lem/popup-window::popup-menu-buffer popup-menu)))
    (setf (buffer-value buffer 'multi-column-list)
          multi-column-list)))

(defmethod display ((component multi-column-list))
  (let ((print-spec (make-instance
                     'print-spec
                     :multi-column-list component
                     :column-width-list (compute-column-width-list component))))
    (lem:display-popup-menu (multi-column-list-items component)
                            :print-spec print-spec
                            :action-callback (lambda (item)
                                               (select-item component item))
                            :style '(:gravity :center)
                            :max-display-items 100)
    (setf (multi-column-list-print-spec component) print-spec)
    (setf (current-multi-column-list) component)
    (multi-column-list-mode t)))

(defmethod quit ((component multi-column-list))
  (quit-multi-column-list))

(defun quit-multi-column-list ()
  (multi-column-list-mode nil)
  (popup-menu-quit))

(defun update (multi-column-list)
  (popup-menu-update (multi-column-list-items multi-column-list)
                     :print-spec (multi-column-list-print-spec multi-column-list)
                     :max-display-items 100
                     :keep-focus t))

(defun mark-current-item ()
  (let ((item (lem/popup-window:get-focus-item
               (lem/popup-window:find-popup-menu
                :parent-window (current-window)))))
    (setf (multi-column-list-item-check-p item)
          (not (multi-column-list-item-check-p item))))
  (let ((multi-column-list (current-multi-column-list)))
    (update multi-column-list)))

(defun mark-items ()
  (remove-if-not #'multi-column-list-item-check-p
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
