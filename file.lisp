(in-package :lem)

(export '(file-directory-p
          temp-file-name
          expand-file-name
          file-completion
          insert-file-contents
          file-open
          find-file
          read-file
          write-to-file
          save-file
          change-file-name
          write-file
          insert-file
          save-some-buffers
          find-file-hook
          before-save-hook
          after-save-hook))

(defun file-directory-p (filename)
  (string= "" (file-namestring filename)))

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
    while (cl-fad:file-exists-p name)
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
    ;; #+sbcl
    ;; (with-open-file (in pathname
    ;;                     :element-type '(unsigned-byte 8))
    ;;   (setq end-of-line (inquisitor:detect-end-of-line in)))
    (values external-format
            end-of-line)))

(defun insert-file-contents (filename)
  (let ((buffer (window-buffer))
        (point (point)))
    (multiple-value-bind (external-format end-of-line)
        (detect-external-format-from-file filename)
      (with-open-file (in filename :external-format external-format)
        (loop
          (multiple-value-bind (str eof-p)
              (read-line in nil)
            (cond (eof-p
                   (when str
                     (insert-lines (list str)))
                   (return))
                  (t
                   #+sbcl
                   (when (eq end-of-line :crlf)
                     (setq str (subseq str 0 (1- (length str)))))
                   (insert-lines (list str))
                   (insert-newline))))))
      (setf (buffer-external-format buffer)
            (cons external-format end-of-line)))
    (point-set point)
    t))

(defun prepare-auto-mode ()
  (let* ((filename (file-namestring (buffer-filename)))
         (elt (find-if #'(lambda (elt)
                           (ppcre:scan (car elt) filename))
                       *auto-mode-alist*)))
    (when elt
      (funcall (cdr elt)))))

(defun scan-line-property-list (str)
  (ppcre:do-register-groups (var val)
    ("([a-zA-Z0-9-_]+)\\s*:\\s*([a-zA-Z0-9-_]+);?" str)
    (cond ((string= (string-downcase var) "mode")
           (let ((mode (find-mode-from-name val)))
             (when mode
               (funcall mode))))
          (t
           (buffer-put (window-buffer)
                       :file-property-list
                       (cons (cons (string-downcase var) val)
                             (buffer-get (window-buffer)
                                         :file-property-list)))))))

(defun scan-file-property-list ()
  (let ((buffer (window-buffer))
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

(defun file-open (path)
  (let ((name (file-namestring path))
        (absolute-path (expand-file-name path)))
    (when (and (string/= "" name)
               (not (cl-fad:directory-exists-p absolute-path)))
      (let* ((buffer (make-buffer name
                                  :filename absolute-path
                                  :enable-undo-p nil))
             (filename (probe-file (buffer-filename buffer))))
        (set-buffer buffer)
        (when filename
          (insert-file-contents filename)
          (unmark-buffer))
        (buffer-enable-undo buffer))))
  (prepare-auto-mode)
  (scan-file-property-list)
  (run-hooks 'find-file-hook)
  t)

(define-key *global-keymap* (kbd "C-x C-f") 'find-file)
(define-command find-file (filename) ("FFind File: ")
  (let ((buf (get-buffer (file-namestring filename))))
    (cond
     ((null buf)
      (file-open filename))
     ((and (buffer-filename buf)
           (string/= (expand-file-name filename) (buffer-filename buf)))
      (let ((uniq-name
             (uniq-buffer-name
              (file-namestring filename))))
        (file-open filename)
        (rename-buffer uniq-name)))
     (t
      (set-buffer buf)))))

(define-key *global-keymap* (kbd "C-x C-r") 'read-file)
(define-command read-file (filename) ("FRead File: ")
  (find-file filename)
  (setf (buffer-read-only-p (window-buffer)) t)
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
  (buffer-save-node buffer))

(defun save-file-internal (buffer)
  (scan-file-property-list)
  (run-hooks 'before-save-hook)
  (write-to-file buffer (buffer-filename buffer))
  (unmark-buffer)
  (minibuf-print "Wrote")
  (run-hooks 'after-save-hook)
  t)

(define-key *global-keymap* (kbd "C-x C-s") 'save-file)
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

(define-key *global-keymap* (kbd "C-x C-w") 'write-file)
(define-command write-file (filename) ("FWrite File: ")
  (change-file-name filename)
  (save-file-internal (window-buffer)))

(define-key *global-keymap* (kbd "C-x C-i") 'insert-file)
(define-command insert-file (filename) ("fInsert file: ")
  (insert-file-contents filename))

(define-key *global-keymap* (kbd "C-x s") 'save-some-buffers)
(define-command save-some-buffers (&optional save-silently-p) ("P")
  (let ((curbuf (window-buffer)))
    (dolist (buffer *buffer-list*)
      (when (and (buffer-modified-p buffer)
                 (buffer-filename buffer))
        (set-buffer buffer nil)
        (when (or save-silently-p
                  (progn
                    (window-update-all)
                    (minibuf-y-or-n-p "Save file")))
          (save-file))))
    (set-buffer curbuf)))
