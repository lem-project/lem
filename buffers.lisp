(in-package :lem)

(defun special-buffer-p (buffer)
  (let ((name (buffer-name buffer)))
    (and
     (char= #\* (aref name 0))
     (char= #\* (aref name (1- (length name)))))))

(defun filter-special-buffers ()
  (remove-if #'special-buffer-p *buffer-list*))

(defun any-modified-buffer-p ()
  (find-if 'buffer-modified-p
    (filter-special-buffers)))

(defun get-buffer (name)
  (find-if (lambda (buffer)
             (string= name (buffer-name buffer)))
    *buffer-list*))

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

(define-key *global-keymap* "C-xb" 'select-buffer)
(define-command select-buffer (name) ("BUse Buffer: ")
  (let ((buf (or (get-buffer name)
               (make-buffer name))))
    (set-buffer buf)
    t))

(defun get-next-buffer (buffer)
  (let ((res (member buffer *buffer-list*)))
    (if (cdr res)
      (cadr res)
      (car *buffer-list*))))

(define-key *global-keymap* "C-xk" 'kill-buffer)
(define-command kill-buffer (name) ("bKill buffer: ")
  (let ((buf (get-buffer name)))
    (when (cdr *buffer-list*)
      (dolist (win *window-list*)
        (when (eq buf (window-buffer win))
          (let ((*current-window* win))
            (next-buffer)))
        (when (eq buf *prev-buffer*)
          (setq *prev-buffer* (get-next-buffer *prev-buffer*))))
      (setq *buffer-list* (delete buf *buffer-list*))))
  t)

(define-key *global-keymap* "C-xx" 'next-buffer)
(define-command next-buffer (&optional (n 1)) ("p")
  (dotimes (_ n t)
    (set-buffer (get-next-buffer (window-buffer)))))

(define-key *global-keymap* "C-xC-b" 'list-buffers)
(define-command list-buffers () ()
  (let* ((max-name-len
          (+ 3 (apply 'max
                      (mapcar (lambda (b)
                                (length (buffer-name b)))
                              *buffer-list*))))
         (max-filename-len
          (apply 'max
                 (mapcar (lambda (b)
                           (length (buffer-filename b)))
                         *buffer-list*))))
    (let* ((buffer (get-buffer-create "*Buffers*"))
           (out (make-buffer-output-stream buffer)))
      (popup buffer
             (lambda ()
               (format out
                       (format nil "MOD ROL Buffer~~~dTFile~~%"
                               (+ 8 max-name-len)))
               (format out "~a~%"
                       (make-string (+ max-name-len max-filename-len 8)
                                    :initial-element #\-))
               (dolist (b *buffer-list*)
                 (format out
                         (format nil " ~a   ~a  ~a~~~dT~a~~%"
                                 (if (buffer-modified-p b) "*" " ")
                                 (if (buffer-read-only-p b) "*" " ")
                                 (buffer-name b)
                                 (+ 8 max-name-len)
                                 (or (buffer-filename b) "")))))))))
