(in-package :lem)

(export '(*auto-mode-alist*
          find-file
          read-file
          save-file
          changefile-name
          write-file
          insert-file
          save-some-buffers
          revert-buffer))

(defvar *auto-mode-alist* nil)

(defun prepare-auto-mode ()
  (let* ((filename (file-namestring (buffer-filename)))
         (elt (find-if #'(lambda (elt)
                           (ppcre:scan (car elt) filename))
                       *auto-mode-alist*)))
    (when elt
      (funcall (cdr elt)))))

(defun scan-line-property-list (str)
  (ppcre:do-register-groups (var val)
    ("([a-zA-Z0-9-_]+)\\s*:\\s*([^ ;]+);?" str)
    (cond ((string= (string-downcase var) "mode")
           (let ((mode (find-mode-from-name val)))
             (when mode
               (funcall mode))))
          (t
           (setf (get-bvar :file-property-list)
                 (cons (cons (string-downcase var) val)
                       (get-bvar :file-property-list)))))))

(defun scan-file-property-list ()
  (save-excursion
    (beginning-of-buffer)
    (when (looking-at-line "#!")
      (forward-line 1))
    (loop :until (eobp) :do
      (multiple-value-bind (result group-strings)
          (ppcre:scan-to-strings "-\\*-(.*)-\\*-" (current-line-string))
        (when result
          (scan-line-property-list (aref group-strings 0))
          (return)))
      (if (blank-line-p)
          (forward-line 1)
          (return)))))

(defun find-file-1 (buffer new-buffer-p)
  (set-buffer-with-window buffer)
  (when (and new-buffer-p
             (uiop:file-pathname-p (buffer-filename buffer)))
    (prepare-auto-mode)
    (scan-file-property-list)
    (run-hooks 'find-file-hook)))

(defun expand-files* (filename)
  (setf filename (expand-file-name filename))
  (or (directory filename)
      (list filename)))

(define-key *global-keymap* (kbd "C-x C-f") 'find-file)
(define-command find-file (filename) ("FFind File: ")
  (check-switch-minibuffer-window)
  (dolist (pathname (expand-files* filename))
    (multiple-value-bind (buffer new-buffer-p)
        (get-file-buffer (namestring pathname))
      (find-file-1 buffer new-buffer-p)))
  t)

(define-key *global-keymap* (kbd "C-x C-r") 'read-file)
(define-command read-file (filename) ("FRead File: ")
  (check-switch-minibuffer-window)
  (dolist (pathname (expand-files* filename))
    (multiple-value-bind (buffer new-buffer-p)
        (get-file-buffer (namestring pathname))
      (find-file-1 buffer new-buffer-p)
      (setf (buffer-read-only-p buffer) t)))
  t)

(defun save-file-1 ()
  (scan-file-property-list)
  (run-hooks 'before-save-hook)
  (write-to-file (current-buffer) (buffer-filename))
  (message "Wrote ~A" (buffer-filename))
  (run-hooks 'after-save-hook)
  t)

(define-key *global-keymap* (kbd "C-x C-s") 'save-file)
(define-command save-file () ()
  (let ((buffer (current-buffer)))
    (cond
     ((null (buffer-modified-p buffer))
      nil)
     ((not (buffer-have-file-p buffer))
      (message "No file name")
      nil)
     (t
      (save-file-1)))))

(define-command change-file-name (filename) ("sChange file name: ")
  (setf (buffer-filename (current-buffer)) filename)
  t)

(define-key *global-keymap* (kbd "C-x C-w") 'write-file)
(define-command write-file (filename) ("FWrite File: ")
  (change-file-name filename)
  (save-file-1))

(define-key *global-keymap* (kbd "C-x C-i") 'insert-file)
(define-command insert-file (filename) ("fInsert file: ")
  (point-set (insert-file-contents (current-buffer)
                                   (current-point)
                                   filename))
  t)

(define-key *global-keymap* (kbd "C-x s") 'save-some-buffers)
(define-command save-some-buffers (&optional save-silently-p) ("P")
  (check-switch-minibuffer-window)
  (save-excursion
    (dolist (buffer (buffer-list))
      (when (and (buffer-modified-p buffer)
                 (buffer-have-file-p buffer))
        (set-buffer-with-window buffer nil)
        (when (or save-silently-p
                  (progn
                    (redraw-display)
                    (minibuf-y-or-n-p "Save file")))
          (save-file))))))

(define-command revert-buffer (does-not-ask-p) ("P")
  (when (and (or (buffer-modified-p (current-buffer))
                 (changed-disk-p (current-buffer)))
             (or does-not-ask-p
                 (minibuf-y-or-n-p (format nil "Revert buffer from file ~A" (buffer-filename)))))
    (with-buffer-read-only (current-buffer) nil
      (buffer-erase)
      (insert-file-contents (current-buffer)
                            (current-point)
                            (buffer-filename))
      (buffer-unmark (current-buffer))
      (update-changed-disk-date (current-buffer))
      t)))
