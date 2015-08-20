(in-package :lem)

(export '(file-name-directory
          file-name-nondirectory
          file-name-as-directory
          file-directory-p
          file-exist-p
          temp-file-name
          expand-file-name
          file-completion
          file-open
          find-file
          read-file
          write-to-file
          save-file-internal
          save-file
          change-file-name
          write-file
          insert-file
          save-some-buffers))

(defun file-name-directory (filename)
  (let ((pos (position #\/ filename :from-end t)))
    (when pos
      (subseq filename 0 (1+ pos)))))

(defun file-name-nondirectory (filename)
  (let ((pos (position #\/ filename :from-end t)))
    (if pos
        (subseq filename (1+ pos))
        filename)))

(defun file-name-as-directory (filename)
  (if (char/= #\/ (aref filename (1- (length filename))))
      (concatenate 'string filename "/")
      filename))

(defun file-directory-p (filename)
  (string= "" (file-name-nondirectory filename)))

(defun file-exist-p (file-name)
  (if (probe-file file-name)
      t
      nil))

(defun temp-file-name-1 ()
  (concatenate 'string
               "/tmp/"
               *program-name*
               "-"
               (coerce (loop repeat 8
                         collect (code-char
                                  (random-range
                                   (char-code #\a)
                                   (char-code #\z))))
                       'string)))

(defun temp-file-name ()
  (loop
    for name = (temp-file-name-1)
    while (file-exist-p name)
    finally (return name)))

(defun parse-pathname (pathname)
  (let ((path))
    (loop
      (let ((pos (position #\/ pathname)))
        (when (null pos)
          (push pathname path)
          (return))
        (let ((str (subseq pathname 0 pos)))
          (setq pathname (subseq pathname (1+ pos)))
          (cond ((string= str "."))
                ((string= str "..")
                 (pop path))
                ((string= str "~")
                 (setq path
                       (nreverse
                        (parse-pathname
                         (string-right-trim
                          '(#\/)
                          (namestring
                           (user-homedir-pathname)))))))
                ((string= str "")
                 (setq path nil))
                (t
                 (push str path))))))
    (nreverse path)))

(defun expand-file-name (pathname &optional directory)
  (concatenate 'string
               "/"
               (join "/"
                     (parse-pathname
                      (if (and (plusp (length pathname))
                               (char/= #\/ (aref pathname 0)))
                          (format nil "~a~a"
                                  (or directory (buffer-directory))
                                  pathname)
                          pathname)))))

(defun file-completion (str)
  (setq str (expand-file-name str))
  (let ((dirname (file-name-directory str)))
    (completion str (files dirname))))

(defun file-open (path)
  (let ((name (file-name-nondirectory path))
        (absolute-path (expand-file-name path)))
    (when (and (string/= "" name)
               (not (cl-fad:directory-exists-p absolute-path)))
      (let ((buffer (make-buffer name :filename absolute-path)))
        (with-open-file (in (buffer-filename buffer)
                            :if-does-not-exist nil)
          (when in
            (loop
              (multiple-value-bind (str eof-p) (read-line in nil)
                (if (not eof-p)
                    (buffer-append-line buffer str)
                    (progn
                      (buffer-append-line buffer (or str ""))
                      (return)))))))
        (set-buffer buffer)
        (unmark-buffer)
        t))))

(define-key *global-keymap* (kbd "C-xC-f") 'find-file)
(define-command find-file (filename) ("FFind File: ")
  (let ((buf (get-buffer (file-name-nondirectory filename))))
    (cond
     ((null buf)
      (file-open filename))
     ((and (buffer-filename buf)
           (string/= (expand-file-name filename) (buffer-filename buf)))
      (let ((uniq-name
             (uniq-buffer-name
              (file-name-nondirectory filename))))
        (file-open filename)
        (rename-buffer uniq-name)))
     (t
      (set-buffer buf)))
    (run-hooks 'find-file-hooks)))

(define-key *global-keymap* (kbd "C-xC-r") 'read-file)
(define-command read-file (filename) ("FRead File: ")
  (find-file filename)
  (setf (buffer-read-only-p (window-buffer)) t)
  t)

(defun write-to-file (buffer filename)
  (with-open-file (out filename
                       :direction :output
                       :if-exists :supersede
                       :if-does-not-exist :create)
    (map-buffer-lines (lambda (line eof-p linum)
                        (declare (ignore linum))
                        (princ line out)
                        (unless eof-p
                          (terpri out)))
                      buffer))
  (buffer-save-node buffer))

(defun save-file-internal (buffer)
  (run-hooks 'before-save-hook)
  (write-to-file buffer (buffer-filename buffer))
  (unmark-buffer)
  (minibuf-print "Wrote")
  (run-hooks 'after-save-hook)
  t)

(define-key *global-keymap* (kbd "C-xC-s") 'save-file)
(define-command save-file () ()
  (let ((buffer (window-buffer)))
    (cond
     ((null (buffer-modified-p buffer))
      nil)
     ((null (buffer-filename buffer))
      (minibuf-print "No file name")
      nil)
     (t
      (save-file-internal buffer)))))

(define-command change-file-name (filename) ("sChange file name: ")
  (setf (buffer-filename (window-buffer)) filename)
  t)

(define-key *global-keymap* (kbd "C-xC-w") 'write-file)
(define-command write-file (filename) ("FWrite File: ")
  (change-file-name filename)
  (save-file-internal (window-buffer)))

(define-key *global-keymap* (kbd "C-xC-i") 'insert-file)
(define-command insert-file (filename) ("fInsert file: ")
  (with-open-file (in filename)
    (do ((str #1=(read-line in nil) #1#))
        ((null str))
      (insert-string str)
      (insert-newline 1))
    t))

(define-key *global-keymap* (kbd "C-xs") 'save-some-buffers)
(define-command save-some-buffers (&optional save-silently-p) ("P")
  (let ((buffer (current-buffer)))
    (dolist (buffer *buffer-list*)
      (when (and (buffer-modified-p buffer)
                 (buffer-filename buffer))
        (set-buffer buffer nil)
        (when (or save-silently-p
                  (progn
                    (window-update-all)
                    (minibuf-y-or-n-p "Save file")))
          (save-file))))
    (set-buffer buffer)))
