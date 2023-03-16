(defpackage :lem/list-buffers
  (:use :cl :lem :lem/menu-mode)
  (:export :list-buffers)
  #+sbcl
  (:lock t))
(in-package :lem/list-buffers)

(define-key *global-keymap* "C-x C-b" 'list-buffers)

(defun buffer-menu-items ()
  ;; copy-list is necessary to detect buffer list changes
  (copy-list (buffer-list)))

(defun buffer-menu-columns (buffer)
  (list (format nil "~:[-~;%~]~:[-~;%~]"
                (buffer-modified-p buffer)
                (buffer-read-only-p buffer))
        (buffer-name buffer)
        (buffer-filename buffer)))

(defun buffer-menu-select (menu buffer)
  (declare (ignore menu))
  buffer)

(defun buffer-menu-delete (menu buffer)
  (declare (ignore menu))
  (kill-buffer buffer)
  :redraw)

(defun buffer-menu-check-consistency (menu origin-items)
  (let ((items (buffer-menu-items)))
    (cond
      ((set-exclusive-or items origin-items :test 'equal)
       (update-menu menu items)
       (message "Buffer list has been changed. Please select again.")
       nil)
      (t t))))

(define-command list-buffers () ()
  (let ((menu
         (make-instance 'menu
                        :columns '("Attributes" "Buffer" "File")
                        :items (buffer-menu-items)
                        :column-function #'buffer-menu-columns
                        :select-callback #'buffer-menu-select
                        :delete-callback #'buffer-menu-delete
                        :update-items-function #'buffer-menu-items
                        :check-consistency-function #'buffer-menu-check-consistency)))
    (display-menu menu :name "Buffer Menu")
    ;; update is necessary to add the buffer menu itself
    (update-menu menu (buffer-menu-items))))
