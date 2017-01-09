(in-package :cl-user)
(defpackage :lem.list-buffers
  (:use :cl :lem)
  (:export :list-buffers))
(in-package :lem.list-buffers)

(define-major-mode list-buffers-mode nil
    (:name "List Buffers"
	   :keymap *list-buffers-keymap*))

(define-key *list-buffers-keymap* (kbd "q") 'quit-window)

(define-key *global-keymap* (kbd "C-x C-b") 'list-buffers)
(define-command list-buffers () ()
  (let ((buffer (get-buffer-create "*Buffers*")))
    (setf (buffer-truncate-lines buffer) nil)
    (change-buffer-mode buffer 'list-buffers-mode)
    (with-pop-up-typeout-window (out buffer :erase t)
      (let* ((max-name-len
              (+ 3 (apply 'max
                          (mapcar #'(lambda (b)
                                      (length (buffer-name b)))
                                  (buffer-list)))))
             (max-filename-len
              (apply 'max
                     (mapcar #'(lambda (b)
                                 (length (buffer-filename b)))
                             (buffer-list)))))
        (format out
                (format nil "MOD ROL Buffer~~~dTFile~~%"
                        (+ 8 max-name-len)))
        (princ (make-string (+ max-name-len max-filename-len 8)
                            :initial-element #\-)
               out)
        (terpri out)
        (dolist (b (buffer-list))
          (format out
                  (format nil " ~a   ~a  ~a~~~dT~a~~%"
                          (if (buffer-modified-p b) "*" " ")
                          (if (buffer-read-only-p b) "*" " ")
                          (buffer-name b)
                          (+ 8 max-name-len)
                          (or (buffer-filename b) ""))))))))
