;; -*- Mode: LISP; Package: LEM -*-

(in-package :lem)

(export '(current-buffer
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
          select-buffer
          get-next-buffer
          kill-buffer
          next-buffer
          list-buffers
          get-buffer-window))

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
               (and (buffer-filename buffer)
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
              (list (window-vtop-linum (current-window))
                    (window-current-linum (current-window))
                    (window-current-charpos (current-window))))))
    (setf (current-buffer) buffer)
    (let ((vtop-linum 1)
          (cur-linum 1)
          (cur-pos 0))
      (when (buffer-keep-binfo buffer)
        (multiple-value-setq
            (vtop-linum cur-linum cur-pos)
          (apply 'values (buffer-keep-binfo buffer))))
      (delete-marker (window-point-marker (current-window)))
      (setf (window-point-marker (current-window)) (make-marker-current-point))
      (let ((buffer-nlines (buffer-nlines)))
        (setf (window-vtop-linum (current-window))
              (min vtop-linum buffer-nlines))
        (setf (window-current-linum (current-window))
              (min cur-linum buffer-nlines)))
      (setf (window-vtop-charpos (current-window)) 0)
      (setf (window-current-charpos (current-window))
            (min cur-pos
                 (buffer-line-length (current-buffer)
                                     (window-current-linum (current-window)))))
      (assert (<= 0 (window-current-charpos (current-window)))))))

(defun bury-buffer (buffer)
  (check-switch-minibuffer-window)
  (setq *buffer-list*
        (append (delete buffer (buffer-list))
                (list buffer)))
  (set-buffer (car (buffer-list)) nil))

(define-key *global-keymap* (kbd "C-x b") 'select-buffer)
(define-command select-buffer (name) ("BUse Buffer: ")
  (check-switch-minibuffer-window)
  (set-buffer (get-buffer-create name))
  t)

(defun get-next-buffer (buffer)
  (let* ((buffer-list (reverse (buffer-list)))
         (res (member buffer buffer-list)))
    (if (cdr res)
        (cadr res)
        (car buffer-list))))

(define-key *global-keymap* (kbd "C-x k") 'kill-buffer)
(define-command kill-buffer (buffer-or-name) ("bKill buffer: ")
  (let ((buffer (get-buffer buffer-or-name)))
    (when (cdr (buffer-list))
      (dolist (win (window-list))
        (when (eq buffer (window-buffer win))
          (with-current-window win
            (next-buffer))))
      (setq *buffer-list* (delete buffer (buffer-list)))))
  t)

(define-key *global-keymap* (kbd "C-x x") 'next-buffer)
(define-command next-buffer (&optional (n 1)) ("p")
  (check-switch-minibuffer-window)
  (dotimes (_ n t)
    (set-buffer (get-next-buffer (current-buffer)))))

(define-major-mode list-buffers-mode nil
  (:name "List Buffers"
   :keymap *list-buffers-keymap*))

(define-key *list-buffers-keymap* (kbd "q") 'quit-window)

(define-key *global-keymap* (kbd "C-x C-b") 'list-buffers)
(define-command list-buffers () ()
  (info-popup (get-buffer-create "*Buffers*")
              #'(lambda (out)
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
                                      (or (buffer-filename b) ""))))))
              nil
              'list-buffers-mode))

(defun get-buffer-window (buffer)
  (find buffer (window-list) :key #'window-buffer))
