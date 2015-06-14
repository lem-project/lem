(in-package :lem)

(defvar *buffer-list* nil)

(defun add-buffer (buffer)
  (push buffer *buffer-list*))

(defun any-modified-buffer-p ()
  (find-if 'buffer-modified-p *buffer-list*))

(defun get-buffer (name)
  (find-if (lambda (buffer)
             (string= name (buffer-name buffer)))
    *buffer-list*))

(defun get-buffer-create (name)
  (or (get-buffer name)
    (make-buffer name nil)))

(defun uniq-buffer-name (name)
  (if (null (get-buffer name))
    name
    (do ((n 1 (1+ n))) (nil)
      (let ((name (format nil "~a<~d>" name n)))
        (unless (get-buffer name)
          (return name))))))

(add-command 'select-buffer 'select-buffer "C-xb")
(defun select-buffer (arg)
  (let* ((name (mb-readline "Use buffer: "))
         (buf (or (get-buffer name)
                (make-buffer name nil))))
    (set-buffer buf)
    t))

(add-command 'kill-buffer 'kill-buffer "C-xk")
(defun kill-buffer (arg)
  (let* ((name (mb-readline "Kill buffer: "))
         (buf (get-buffer name)))
    (when (cdr *buffer-list*)
      (dolist (b *window-list*)
        (when (eq buf (window-buffer b))
          (next-buffer nil)))
      (setq *buffer-list* (delete buf *buffer-list*))))
  t)

(add-command 'next-buffer 'next-buffer "C-xx")
(defun next-buffer (arg)
  (arg-repeat (arg t)
    (let ((buf (cadr (member (window-buffer) *buffer-list*))))
      (set-buffer (or buf (car *buffer-list*))))))

(add-command 'list-buffers 'list-buffers "C-xC-b")
(defun list-buffers (arg)
  (let* ((buf (get-buffer-create "*Buffers*"))
         (max-name-len
          (+ 3 (apply 'max
                 (mapcar (lambda (b)
                           (length (buffer-name b)))
                   *buffer-list*))))
         (max-filename-len
          (apply 'max
            (mapcar (lambda (b)
                      (length (buffer-filename b)))
              *buffer-list*))))
    (pop-to-buffer buf)
    (buffer-erase buf)
    (buffer-append-line buf
      (format nil
        (format nil "Buffer~~~dTFile"
          max-name-len)))
    (buffer-append-line buf
      (make-string (+ max-name-len max-filename-len) :initial-element #\-))
    (dolist (b *buffer-list*)
      (buffer-append-line buf
        (format nil
          (format nil "~a~~~dT~a"
            (buffer-name b)
            max-name-len
            (or (buffer-filename b) "")))))))
