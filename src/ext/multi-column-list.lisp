(defpackage :lem/multi-column-list
  (:use :cl :lem)
  (:export :multi-column-list
           :multi-column-list-item
           :select-item
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

;;
(defgeneric select-item (window item))
(defgeneric row-values (item))

(defclass multi-column-list ()
  ((columns :initarg :columns
            :reader multi-column-list-window-columns)
   (items :initarg :items
          :reader multi-column-list-window-items)))

(defclass multi-column-list-item ()
  ())

(defclass print-spec ()
  ((multi-column-list :initarg :multi-column-list
                      :reader print-spec-multi-column-list)
   (column-width-list :initarg :column-width-list
                      :reader print-spec-column-width-list)))

(defmethod lem/popup-window:write-header ((print-spec print-spec) point)
  (with-point ((start point))
    (loop :for width :in (print-spec-column-width-list print-spec)
          :for column-header :in (multi-column-list-window-columns
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
          (loop :for row :in (cons (multi-column-list-window-columns multi-column-list)
                                   (mapcar #'row-values (multi-column-list-window-items multi-column-list)))
                :collect (loop :for value :in row
                               :collect (string-width value)))))
    (loop :repeat (length (first width-matrix))
          :for i :from 0
          :collect (loop :for width-list :in width-matrix :maximize (elt width-list i)))))

(defmethod display ((window multi-column-list))
  (lem:display-popup-menu (multi-column-list-window-items window)
                          :print-spec (make-instance
                                       'print-spec
                                       :multi-column-list window
                                       :column-width-list (compute-column-width-list window))
                          :action-callback (lambda (item)
                                             (select-item window item))
                          :style '(:gravity :center)
                          :max-display-items 100)
  (multi-column-list-mode t))

(defmethod quit ((window multi-column-list))
  (quit-multi-column-list))

(defun quit-multi-column-list ()
  (multi-column-list-mode nil)
  (popup-menu-quit))
