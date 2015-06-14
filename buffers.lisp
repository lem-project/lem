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

(add-command 'select-buffer 'select-buffer "C-xb")
(defun select-buffer (arg)
  (let* ((name (mb-readline "Use buffer: "))
         (buf (get-buffer name)))
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
