(in-package :lem)

(defvar *buffer-list* nil)

(defun add-buffer (textbuf)
  (push textbuf *buffer-list*))

(defun any-modified-buffer-p ()
  (find-if 'textbuf-modified-p *buffer-list*))

(defun get-buffer (name)
  (find-if (lambda (textbuf)
             (string= name (textbuf-name textbuf)))
    *buffer-list*))

(add-command 'select-buffer 'select-buffer "C-xb")
(defun select-buffer (arg)
  (let* ((name (mb-readline "Use buffer: "))
         (tb (get-buffer name)))
    (set-buffer tb)
    t))

(add-command 'kill-buffer 'kill-buffer "C-xk")
(defun kill-buffer (arg)
  (let* ((name (mb-readline "Kill buffer: "))
         (tb (get-buffer name)))
    (when (cdr *buffer-list*)
      (dolist (b *window-list*)
        (when (eq tb (window-textbuf b))
          (next-buffer nil)))
      (setq *buffer-list* (delete tb *buffer-list*))))
  t)

(add-command 'next-buffer 'next-buffer "C-xx")
(defun next-buffer (arg)
  (arg-repeat (arg t)
    (let ((tb (cadr (member (window-textbuf) *buffer-list*))))
      (set-buffer (or tb (car *buffer-list*))))))
