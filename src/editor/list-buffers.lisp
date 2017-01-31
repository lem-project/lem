(in-package :cl-user)
(defpackage :lem.list-buffers
  (:use :cl :lem)
  (:export :list-buffers))
(in-package :lem.list-buffers)

(define-major-mode list-buffers-mode nil
    (:name "List Buffers"
	   :keymap *list-buffers-keymap*))

(define-key *list-buffers-keymap* "q" 'quit-window)
(define-key *list-buffers-keymap* "C-m" 'list-buffers-select)

(define-key *global-keymap* (kbd "C-x C-b") 'list-buffers)
(define-command list-buffers () ()
  (let ((buffer (get-buffer-create "*Buffers*")))
    (change-buffer-mode buffer 'list-buffers-mode)
    (display-buffer buffer)
    (erase-buffer buffer)
    (setf (value 'truncate-lines :buffer buffer) nil)
    (let ((point (buffer-point buffer)))
      (buffer-start point)
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
        (insert-string point
                       (format nil "MOD ROL Buffer~vTFile~%"
                               (+ 8 max-name-len)))
        (insert-character point #\- (+ max-name-len max-filename-len 8))
        (insert-character point #\newline)
        (dolist (b (buffer-list))
          (insert-string point
                         (format nil " ~a   ~a  ~a~vT~a~%"
                                 (if (buffer-modified-p b) "*" " ")
                                 (if (buffer-read-only-p b) "*" " ")
                                 (buffer-name b)
                                 (+ 8 max-name-len)
                                 (or (buffer-filename b) ""))
                         :buffer
                         (buffer-name b)))))))

(define-command list-buffers-select () ()
  (let* ((buffer-name
          (text-property-at (line-start (copy-point (current-point) :temporary))
                            :buffer))
         (buffer (and buffer-name (get-buffer buffer-name))))
    (message "~A ~A" buffer-name buffer)
    (when buffer
      (switch-to-buffer buffer))))
