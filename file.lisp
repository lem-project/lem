(in-package :lem)

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

(defun current-directory ()
  (file-name-as-directory (pwd)))

(defun file-exist-p (file-name)
  (if (probe-file file-name)
    t
    nil))

(defun expand-file-name (filename &optional directory)
  (when (char/= (aref filename 0) #\/)
    (setq filename
      (concatenate 'string
        (or (and directory (file-name-as-directory directory))
            (current-directory))
        filename)))
  (let ((path))
    (dolist (name (split-string filename #\/))
      (cond
       ((string= ".." name)
        (pop path))
       ((string/= "." name)
        (push name path))))
    (let ((str ""))
      (dolist (p (nreverse path))
        (setq str
          (concatenate 'string
            str "/" p)))
      (subseq str 1))))

(defun file-completion (str)
  (setq str (expand-file-name str))
  (let ((dirname (file-name-directory str)))
    (completion str (files dirname))))

(defun file-open (filename)
  (setq filename (file-name-nondirectory filename))
  (unless (string= "" filename)
    (let ((buffer (make-buffer
                   filename
                   (expand-file-name filename))))
      (with-open-file (in filename :if-does-not-exist nil)
        (when in
          (do () (nil)
            (multiple-value-bind (str eof-p) (read-line in nil)
              (if (not eof-p)
                (buffer-append-line buffer str)
                (progn
                 (buffer-append-line buffer (or str ""))
                 (return)))))))
      (set-buffer buffer)
      (unmark-buffer)
      t)))

(define-key *global-keymap* "C-xC-f" 'find-file)
(defcommand find-file (filename) ("FFind File: ")
  (let ((buf (get-buffer filename)))
    (cond
     ((null buf)
      (file-open filename))
     ((or
       (not (buffer-filename buf))
       (string/= filename (buffer-filename buf)))
      (let ((name (uniq-buffer-name filename)))
        (set-buffer (make-buffer (file-name-nondirectory name) filename))))
     (t
      (set-buffer buf)))))

(define-key *global-keymap* "C-xC-s" 'save-file)
(defcommand save-file () ()
  (let ((buffer (window-buffer)))
    (cond
     ((null (buffer-modified-p buffer))
      nil)
     ((null (buffer-filename buffer))
      (mb-write "No file name")
      nil)
     (t
      (with-open-file (out (buffer-filename buffer)
                        :direction :output
                        :if-exists :supersede
                        :if-does-not-exist :create)
        (do ((lines
              (buffer-take-lines buffer 1 (buffer-nlines buffer))
              (cdr lines)))
            ((null lines))
          (princ (car lines) out)
          (when (cdr lines)
            (terpri out))))
      (unmark-buffer)
      (mb-write "Wrote")
      t))))
