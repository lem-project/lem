(defpackage :lem/buffer-menu
  (:use :cl
        :lem)
  (:import-from :lem/multi-column-list
                :multi-column-list
                :multi-column-list-item
                :select-item
                :row-values
                :display
                :quit))
(in-package :lem/buffer-menu)

(defclass buffer-menu (multi-column-list)
  ())

(defmethod select-item ((window buffer-menu) item)
  (quit window)
  (switch-to-buffer (item-buffer item)))

(defclass item (multi-column-list-item)
  ((buffer :initarg :buffer
           :reader item-buffer)))

(defmethod row-values ((item item))
  (let ((buffer (item-buffer item)))
    (list (string-trim " " (lem::buffer-attributes buffer))
          (buffer-name buffer)
          (buffer-filename buffer))))

(defun make-item (buffer)
  (make-instance 'item :buffer buffer))

(define-command list-buffer-menu () ()
  (display
   (make-instance 'buffer-menu
                  :columns '("" "Buffer" "File")
                  :items (mapcar #'make-item (buffer-list)))))

;(define-key *global-keymap* "C-x C-b" 'list-buffer-menu)
