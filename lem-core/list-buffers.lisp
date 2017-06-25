(defpackage :lem.list-buffers
  (:use :cl :lem :lem.menu-mode)
  (:export :list-buffers))
(in-package :lem.list-buffers)

(define-key *global-keymap* "C-x C-b" 'list-buffers)

(define-command list-buffers () ()
  (let ((menu (make-instance 'menu
                             :buffer-name "*Buffer menu*"
                             :columns '("MOD" "ROL" "Buffer" "File"))))
    (dolist (buffer (buffer-list))
      (let ((item (make-instance
                   'menu-item
                   :select-function (let ((buffer-name (buffer-name buffer)))
                                      (lambda (set-buffer-fn)
                                        (let ((buffer (get-buffer buffer-name)))
                                          (when buffer
                                            (funcall set-buffer-fn buffer))))))))
        (append-menu-item item (if (buffer-modified-p buffer) " *" " "))
        (append-menu-item item (if (buffer-read-only-p buffer) " *" " "))
        (append-menu-item item (buffer-name buffer))
        (append-menu-item item (or (buffer-filename buffer) ""))
        (append-menu menu item)))
    (display-menu menu)))
