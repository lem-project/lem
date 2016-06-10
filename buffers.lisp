(in-package :lem)

(export '(current-buffer
          buffer-list
          ghost-buffer-p
          special-buffer-p
          filter-special-buffers
          any-modified-buffer-p
          get-buffer
          get-buffer-create
          uniq-buffer-name
          other-buffer
          update-prev-buffer
          set-buffer
          bury-buffer
          get-next-buffer
          get-buffer-windows))

(defvar *buffer-list* nil)

(defun add-buffer (buffer)
  (unless (ghost-buffer-p buffer)
    (assert (not (get-buffer (buffer-name buffer))))
    (push buffer *buffer-list*)))

(defun buffer-list () *buffer-list*)

(defun current-buffer ()
  (window-buffer (current-window)))

(defun (setf current-buffer) (new-buffer)
  (setf (window-buffer (current-window)) new-buffer))

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
  (setq *buffer-list*
        (cons buffer
              (delete buffer (buffer-list)))))

(defun set-buffer (buffer &optional (update-prev-buffer-p t))
  (check-switch-minibuffer-window)
  (unless (eq (current-buffer) buffer)
    (when update-prev-buffer-p
      (setf (window-parameter (current-window) :split-p) nil)
      (let ((old-buf (current-buffer)))
        (update-prev-buffer old-buf)
        (setf (buffer-keep-binfo old-buf)
              (list (window-view-linum (current-window))
                    (window-current-linum (current-window))
                    (window-current-charpos (current-window))))))
    (setf (current-buffer) buffer)
    (let ((view-linum 1)
          (cur-linum 1)
          (cur-pos 0))
      (when (buffer-keep-binfo buffer)
        (multiple-value-setq
            (view-linum cur-linum cur-pos)
          (apply 'values (buffer-keep-binfo buffer))))
      (delete-marker (window-point-marker (current-window)))
      (setf (window-point-marker (current-window)) (make-marker-current-point))
      (let ((buffer-nlines (buffer-nlines)))
        (setf (window-view-marker (current-window))
              (make-marker buffer
                           (make-point (min view-linum buffer-nlines)
                                       0)))
        (setf (window-current-linum (current-window))
              (min cur-linum buffer-nlines)))
      (setf (window-current-charpos (current-window))
            (min cur-pos
                 (buffer-line-length (current-buffer)
                                     (window-current-linum (current-window)))))
      (assert (<= 0 (window-current-charpos (current-window)))))))

(defun delete-buffer (buffer)
  (setf *buffer-list* (delete buffer (buffer-list))))

(defun get-next-buffer (buffer)
  (let* ((buffer-list (reverse (buffer-list)))
         (res (member buffer buffer-list)))
    (if (cdr res)
        (cadr res)
        (car buffer-list))))

(defun bury-buffer (buffer)
  (setf *buffer-list*
        (append (delete buffer (buffer-list))
                (list buffer)))
  (car (buffer-list)))
