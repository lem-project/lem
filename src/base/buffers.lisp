(in-package :lem-base)

(annot:enable-annot-syntax)

(export '(kill-buffer-hook
          buffer-list
          ghost-buffer-p
          special-buffer-p
          filter-special-buffers
          any-modified-buffer-p
          get-buffer
          get-buffer-create
          uniq-buffer-name
          update-prev-buffer
          bury-buffer
          get-next-buffer
          delete-buffer))

(define-editor-variable kill-buffer-hook '())

(defvar *buffer-list* '())

(defun add-buffer (buffer)
  (check-type buffer buffer)
  (unless (ghost-buffer-p buffer)
    (assert (not (get-buffer (buffer-name buffer))))
    (push buffer *buffer-list*)))

(defun buffer-list ()
  @lang(:jp "`buffer`のリストを返します。")
  *buffer-list*)

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
               (and (buffer-filename buffer)
                    (buffer-modified-p buffer)))
           (filter-special-buffers)))

(defun get-buffer (buffer-or-name)
  (if (bufferp buffer-or-name)
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

(defun update-prev-buffer (buffer)
  (check-type buffer buffer)
  (setq *buffer-list*
        (cons buffer
              (remove buffer (buffer-list)))))

(defun delete-buffer (buffer)
  (check-type buffer buffer)
  (alexandria:when-let ((hooks (variable-value 'kill-buffer-hook :buffer buffer)))
    (run-hooks hooks buffer))
  (alexandria:when-let ((hooks (variable-value 'kill-buffer-hook :global)))
    (run-hooks hooks buffer))
  (buffer-free buffer)
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
        (append (remove buffer (buffer-list))
                (list buffer)))
  (car (buffer-list)))
