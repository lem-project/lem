(defpackage :lem.list-buffers
  (:use :cl :lem :lem.menu-mode)
  #+sbcl
  (:lock t))
(in-package :lem.list-buffers)

(define-key *global-keymap* "C-x C-b" 'list-buffers)

(define-command list-buffers () ()
  (display-menu
   (make-instance 'menu
                  :columns '("Attributes" "Buffer" "File")
                  :items (buffer-list)
                  :column-function (lambda (buffer)
                                     (list (format nil "~:[-~;%~]~:[-~;%~]"
                                                   (buffer-modified-p buffer)
                                                   (buffer-read-only-p buffer))
                                           (buffer-name buffer)
                                           (buffer-filename buffer)))
                  :select-callback #'menu-change-buffer
                  :delete-callback #'menu-delete-buffer
                  :update-items-function (lambda () (buffer-list)))
   :name "Buffer Menu"))
