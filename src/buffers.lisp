(in-package :lem)

(export '(buffer-list
          ghost-buffer-p
          special-buffer-p
          filter-special-buffers
          any-modified-buffer-p
          get-buffer
          get-buffer-create
          uniq-buffer-name
          other-buffer
          update-prev-buffer
          bury-buffer
          get-next-buffer
          get-buffer-windows))

(defvar *buffer-list* nil)

(defun add-buffer (buffer)
  (check-type buffer buffer)
  (unless (ghost-buffer-p buffer)
    (assert (not (get-buffer (buffer-name buffer))))
    (push buffer *buffer-list*)))

(defun buffer-list () *buffer-list*)

(defun ghost-buffer-p (buffer)
  (let ((name (buffer-name buffer)))
    (and (<= 3 (length name))
         (char= #\space (aref name 0))
         (char= #\* (aref name 1))
         (char= #\* (aref name (1- (length name)))))))

(defun special-buffer-p (buffer)
  (or (ghost-buffer-p buffer)
      (let ((name (buffer-name buffer)))
        (and (char= #\* (aref name 0))
             (char= #\* (aref name (1- (length name))))))))

(defun filter-special-buffers ()
  (remove-if #'special-buffer-p (buffer-list)))

(defun any-modified-buffer-p ()
  (find-if #'(lambda (buffer)
               (and (buffer-have-file-p buffer)
                    (buffer-modified-p buffer)))
           (filter-special-buffers)))

(defun get-buffer (buffer-or-name)
  (if (buffer-p buffer-or-name)
      buffer-or-name
      (find-if #'(lambda (buffer)
                   (string= buffer-or-name
                            (buffer-name buffer)))
               (buffer-list))))

(defun get-buffer-create (name)
  (or (get-buffer name)
      (make-buffer name)))

(defun uniq-buffer-name (name)
  (if (null (get-buffer name))
      name
      (do ((n 1 (1+ n))) (nil)
        (let ((name (format nil "~a<~d>" name n)))
          (unless (get-buffer name)
            (return name))))))

(defun other-buffer ()
  (let ((buffer-list (buffer-list)))
    (dolist (win (window-list))
      (setq buffer-list
            (remove (window-buffer win)
                    buffer-list)))
    (if (null buffer-list)
        (car (buffer-list))
        (car buffer-list))))

(defun update-prev-buffer (buffer)
  (check-type buffer buffer)
  (setq *buffer-list*
        (cons buffer
              (delete buffer (buffer-list)))))

(defun delete-buffer (buffer)
  (check-type buffer buffer)
  (call-buffer-delete-hooks buffer)
  (setf *buffer-list* (delete buffer (buffer-list))))

(defun get-next-buffer (buffer)
  (check-type buffer buffer)
  (let* ((buffer-list (reverse (buffer-list)))
         (res (member buffer buffer-list)))
    (if (cdr res)
        (cadr res)
        (car buffer-list))))

(defun bury-buffer (buffer)
  (check-type buffer buffer)
  (setf *buffer-list*
        (append (delete buffer (buffer-list))
                (list buffer)))
  (car (buffer-list)))
