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
(define-key *multi-column-list-mode-keymap* "Return" 'multi-column-list/select)

(define-command multi-column-list/default () ()
  )

(define-command multi-column-list/quit () ()
  (quit-multi-column-list))

(define-command multi-column-list/down () ()
  (popup-menu-down))

(define-command multi-column-list/up () ()
  (popup-menu-up))

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
  ((column-width-list :initarg :column-width-list
                      :reader print-spec-column-width-list)))

(defmethod lem/popup-window:apply-print-spec ((print-spec print-spec) point item)
  (check-type item multi-column-list-item)
  (loop :for value :in (row-values item)
        :for width :in (print-spec-column-width-list print-spec)
        :do (insert-string point " ")
            (let ((column (point-column point)))
              (insert-string point value)
              (move-to-column point (+ column width) t))
            (insert-string point " ")))

(defun compute-column-width-list (multi-column-list)
  (let ((width-matrix
          (loop :for item :in (multi-column-list-window-items multi-column-list)
                :collect (loop :for value :in (row-values item)
                               :collect (string-width value)))))
    (loop :repeat (length (first width-matrix))
          :for i :from 0
          :collect (loop :for width-list :in width-matrix :maximize (elt width-list i)))))

(defmethod display ((window multi-column-list))
  (lem:display-popup-menu (multi-column-list-window-items window)
                          :print-spec (make-instance
                                       'print-spec
                                       :column-width-list (compute-column-width-list window))
                          :action-callback (lambda (item)
                                             (select-item window item))
                          :style '(:gravity :center))
  (multi-column-list-mode t))

(defmethod quit ((window multi-column-list))
  (quit-multi-column-list))

(defun quit-multi-column-list ()
  (multi-column-list-mode nil)
  (popup-menu-quit))
