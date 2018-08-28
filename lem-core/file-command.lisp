(in-package :lem)

(export '(find-file
          read-file
          save-buffer
          changefile-name
          write-file
          write-region-file
          insert-file
          save-some-buffers
          revert-buffer
          change-directory))

(defun expand-files* (filename)
  (directory-files (expand-file-name filename (buffer-directory))))

(define-key *global-keymap* "C-x C-f" 'find-file)
(define-command find-file (filename) ("p")
  (check-switch-minibuffer-window)
  (let ((*default-external-format* *default-external-format*))
    (cond ((and (numberp filename) (= 1 filename))
           (setf filename (prompt-for-file
                           "Find File: "
                           (buffer-directory)
                           nil
                           nil)))
          ((numberp filename)
           (setf *default-external-format*
                 (prompt-for-encodings
                  "Encodings: "
                  'mh-read-file-encodings)
                 filename (prompt-for-file
                           "Find File: "
                           (buffer-directory)
                           nil
                           nil))
           (message "::~S" *default-external-format*)
           )
          ((pathnamep filename)
           (setf filename (namestring filename))))
    (dolist (pathname (expand-files* filename))
      (switch-to-buffer (find-file-buffer (namestring (probe-file pathname))) t nil))
    t))

(define-key *global-keymap* "C-x C-r" 'read-file)
(define-command read-file (filename) ("FRead File: ")
  (check-switch-minibuffer-window)
  (when (pathnamep filename)
    (setf filename (namestring filename)))
  (dolist (pathname (expand-files* filename))
    (let ((buffer (find-file-buffer (namestring pathname))))
      (setf (buffer-read-only-p buffer) t)
      (switch-to-buffer buffer t nil)))
  t)

(define-key *global-keymap* "C-x C-s" 'save-buffer)
(define-command save-buffer (&optional arg) ("P")
  (let ((buffer (current-buffer)))
    (cond
      ((and (or arg (buffer-modified-p buffer))
            (buffer-filename buffer))
       (write-to-file buffer (buffer-filename buffer))
       (buffer-unmark buffer)
       (message "Wrote ~A" (buffer-filename)))
      ((not (buffer-filename buffer))
       (message "No file name")))))

(define-key *global-keymap* "C-x C-w" 'write-file)
(define-command write-file (filename) ("FWrite File: ")
  (let* ((old (buffer-name))
         (new (file-namestring filename))
         (expand-file-name (expand-file-name filename)))
    (unless (and (find expand-file-name (mapcar #'buffer-filename
                                                (buffer-list))
                       :test #'equal)
                 (not (prompt-for-y-or-n-p (format nil
                                                   "~a is opend, overwrite it?"
                                                   expand-file-name))))
      (unless (string= old new)
        (setf (buffer-name)
              (if (get-buffer new)
                  (uniq-buffer-name new)
                  new)))
      (setf (buffer-filename) expand-file-name)
      (save-buffer t))))

(define-command write-region-file (start end filename)
    ((progn
       (check-marked)
       (list (region-beginning)
             (region-end)
             (prompt-for-file "Write Region To File: " (buffer-directory) nil nil))))
  (setf filename (expand-file-name filename))
  (write-region-to-file start end filename)
  (message "Wrote ~A" filename))

(define-key *global-keymap* "C-x Tab" 'insert-file)
(define-command insert-file (filename) ("fInsert file: ")
  (insert-file-contents (current-point)
                        (expand-file-name filename))
  t)

(define-key *global-keymap* "C-x s" 'save-some-buffers)
(define-command save-some-buffers (&optional save-silently-p) ("P")
  (check-switch-minibuffer-window)
  (let ((prev-buffer (current-buffer)))
    (dolist (buffer (buffer-list))
      (when (and (buffer-modified-p buffer)
                 (buffer-filename buffer))
        (switch-to-buffer buffer nil)
        (when (or save-silently-p
                  (prompt-for-y-or-n-p (format nil "Save file ~A" (buffer-filename buffer))))
          (save-buffer))))
    (switch-to-buffer prev-buffer nil)))

(define-command revert-buffer (does-not-ask-p) ("P")
  (cond ((buffer-value (current-buffer) 'revert-buffer-function)
         (funcall (buffer-value (current-buffer) 'revert-buffer-function) does-not-ask-p))
        ((and (or (buffer-modified-p (current-buffer))
                  (changed-disk-p (current-buffer)))
              (or does-not-ask-p
                  (prompt-for-y-or-n-p (format nil "Revert buffer from file ~A" (buffer-filename)))))
         (with-buffer-read-only (current-buffer) nil
           (erase-buffer)
           (insert-file-contents (current-point)
                                 (buffer-filename))
           (buffer-unmark (current-buffer))
           (update-changed-disk-date (current-buffer))
           t))))

(define-command change-directory (directory)
    ((list (prompt-for-directory "change directory: " (buffer-directory))))
  (let ((directory (expand-file-name directory (buffer-directory))))
    (setf (buffer-directory) directory)
    (uiop:chdir directory)
    (setf *default-pathname-defaults* (uiop:getcwd)))
  t)
