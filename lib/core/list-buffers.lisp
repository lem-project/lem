(defpackage :lem.list-buffers
  (:use :cl :lem :lem.menu-mode)
  (:export :list-buffers)
  #+sbcl
  (:lock t))
(in-package :lem.list-buffers)

(define-key *global-keymap* "C-x C-b" 'list-buffers)

(defun buffer-list-changed-p (menu &optional (msg-on nil))
  (cond
    ((set-exclusive-or (buffer-list)
                       (lem.menu-mode::menu-origin-items menu)
                       :test 'equal)
     (let ((items (funcall (lem.menu-mode::menu-update-items-function menu))))
       (update-menu menu items))
     (when msg-on
       (message "Buffer list has been changed. Please select again."))
     t)
    (t nil)))

(defun menu-change-buffer (menu buffer)
  (unless (buffer-list-changed-p menu t)
    buffer))

(defun menu-delete-buffer (menu buffer)
  (unless (buffer-list-changed-p menu t)
    (kill-buffer buffer)
    :redraw))

(define-command list-buffers () ()
  ;; copy-list is necessary to detect buffer list changes
  (let ((menu
         (make-instance 'menu
                        :columns '("Attributes" "Buffer" "File")
                        :items (copy-list (buffer-list))
                        :column-function (lambda (buffer)
                                           (list (format nil "~:[-~;%~]~:[-~;%~]"
                                                         (buffer-modified-p buffer)
                                                         (buffer-read-only-p buffer))
                                                 (buffer-name buffer)
                                                 (buffer-filename buffer)))
                        :select-callback #'menu-change-buffer
                        :delete-callback #'menu-delete-buffer
                        :update-items-function (lambda () (copy-list (buffer-list))))))
    (display-menu menu :name "Buffer Menu")
    ;; update is necessary to add the buffer menu itself
    (let ((items (funcall (lem.menu-mode::menu-update-items-function menu))))
      (update-menu menu items))))
