;; -*- Mode: LISP; Package: LEM -*-

(in-package :lem)

(export '(expand-file-name
          file-completion
          insert-file-contents
          find-file
          read-file
          write-to-file
          save-file
          change-file-name
          write-file
          insert-file
          save-some-buffers))

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
  (let ((dirname (directory-namestring str)))
    (completion str
                (mapcar #'namestring
                        (cl-fad:list-directory dirname)))))

(defun detect-external-format-from-file (pathname)
  (let ((external-format)
        (end-of-line :lf))
    (with-open-file (in pathname
                        :element-type '(unsigned-byte 8))
      (let ((inquisitor:*detecting-buffer-size* (file-length in)))
        (setq external-format (inquisitor:detect-external-format in :jp))))
    #+sbcl
    (with-open-file (in pathname
                        :element-type '(unsigned-byte 8))
      (let ((result (inquisitor:detect-end-of-line in)))
        (when result
          (setq end-of-line result))))
    (values external-format
            end-of-line)))

(defun insert-file-contents (buffer point filename)
  (multiple-value-bind (external-format end-of-line)
      (detect-external-format-from-file filename)
    (let ((output (make-buffer-output-stream buffer point nil)))
      (with-open-file (in filename :external-format external-format)
        (loop
          (multiple-value-bind (str eof-p)
              (read-line in nil)
            (cond
              (eof-p
               (when str
                 (write-string str output))
               (return))
              (t
               (let ((end nil))
                 #+sbcl
                 (when (and (eq end-of-line :crlf)
                            (< 0 (length str)))
                   (setf end (1- (length str))))
                 (write-line str output :end end)))))))
      (setf (buffer-external-format buffer)
            (cons external-format end-of-line))
      (buffer-output-stream-point output))))

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
  (let ((buffer (current-buffer))
        (start-linum 1))
    (let ((str (buffer-line-string buffer 1)))
      (when (and (< 2 (length str))
                 (string= str "#!" :end1 2))
        (incf start-linum)))
    (do ((linum start-linum (1+ linum))
         (nlines (buffer-nlines buffer)))
        ((< nlines linum))
      (let ((str (buffer-line-string buffer linum)))
        (ppcre:register-groups-bind
         (match-str)
         ("-\\*-(.*)-\\*-" str)
         (scan-line-property-list match-str)
         (return-from scan-file-property-list))
        (when (not (ppcre:scan "^\\s*$" str))
          (return-from scan-file-property-list))))))

(defun file-open-create-buffer (buffer-name filename)
  (setf filename (expand-file-name filename))
  (let ((buffer (make-buffer buffer-name
                             :filename filename
                             :enable-undo-p nil)))
    (when (probe-file filename)
      (insert-file-contents buffer
                            (point-min buffer)
                            filename)
      (buffer-unmark buffer))
    (buffer-enable-undo buffer)
    buffer))

(define-key *global-keymap* (kbd "C-x C-f") 'find-file)
(define-command find-file (filename) ("FFind File: ")
  (check-switch-minibuffer-window)
  (setf filename (expand-file-name filename))
  (cond
    ((cl-fad:directory-exists-p filename)
     (dired filename))
    ((dolist (buffer (buffer-list))
       (when (equal filename (buffer-filename buffer))
         (set-buffer buffer)
         (return t))))
    (t
     (let ((name (file-namestring filename)))
       (set-buffer
        (file-open-create-buffer
         (if (get-buffer name)
             (uniq-buffer-name name)
             name)
         filename)))
     (prepare-auto-mode)
     (scan-file-property-list)
     (run-hooks 'find-file-hook)
     t)))

(define-key *global-keymap* (kbd "C-x C-r") 'read-file)
(define-command read-file (filename) ("FRead File: ")
  (find-file filename)
  (setf (buffer-read-only-p (current-buffer)) t)
  t)

(defun write-to-file (buffer filename)
  (flet ((f (out end-of-line)
            (map-buffer-lines #'(lambda (line eof-p linum)
                                  (declare (ignore linum))
                                  (princ line out)
                                  (unless eof-p
                                    #+sbcl
                                    (ecase end-of-line
                                      ((:crlf)
                                       (princ #\return out)
                                       (princ #\newline out))
                                      ((:lf)
                                       (princ #\newline out))
                                      ((:cr)
                                       (princ #\return out)))
                                    #-sbcl
                                    (princ #\newline out)))
                              buffer)))
    (cond
     ((buffer-external-format buffer)
      (with-open-file (out filename
                           :direction :output
                           :if-exists :supersede
                           :if-does-not-exist :create
                           :external-format (car (buffer-external-format
                                                  buffer)))
        (f out
           (cdr (buffer-external-format
                 buffer)))))
     (t
      (with-open-file (out filename
                           :direction :output
                           :if-exists :supersede
                           :if-does-not-exist :create)
        (f out :lf)))))
  (buffer-unmark (current-buffer)))

(defun save-file-internal (buffer)
  (scan-file-property-list)
  (run-hooks 'before-save-hook)
  (write-to-file buffer (buffer-filename buffer))
  (minibuf-print "Wrote")
  (run-hooks 'after-save-hook)
  t)

(define-key *global-keymap* (kbd "C-x C-s") 'save-file)
(define-command save-file () ()
  (let ((buffer (current-buffer)))
    (cond
     ((null (buffer-modified-p buffer))
      nil)
     ((null (buffer-filename buffer))
      (minibuf-print "No file name")
      nil)
     (t
      (save-file-internal buffer)))))

(define-command change-file-name (filename) ("sChange file name: ")
  (setf (buffer-filename (current-buffer)) filename)
  t)

(define-key *global-keymap* (kbd "C-x C-w") 'write-file)
(define-command write-file (filename) ("FWrite File: ")
  (change-file-name filename)
  (save-file-internal (current-buffer)))

(define-key *global-keymap* (kbd "C-x C-i") 'insert-file)
(define-command insert-file (filename) ("fInsert file: ")
  (point-set (insert-file-contents (current-buffer)
                                   (current-point)
                                   filename))
  t)

(define-key *global-keymap* (kbd "C-x s") 'save-some-buffers)
(define-command save-some-buffers (&optional save-silently-p) ("P")
  (check-switch-minibuffer-window)
  (let ((curbuf (current-buffer)))
    (dolist (buffer (buffer-list))
      (when (and (buffer-modified-p buffer)
                 (buffer-filename buffer))
        (set-buffer buffer nil)
        (when (or save-silently-p
                  (progn
                    (window-update-all)
                    (minibuf-y-or-n-p "Save file")))
          (save-file))))
    (set-buffer curbuf)))
