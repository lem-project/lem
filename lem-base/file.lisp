(in-package :lem-base)

(export '(*find-file-hook*
          *before-save-hook*
          *after-save-hook*
          *external-format-function*
          *find-directory-function*
          expand-file-name
          insert-file-contents
          find-file-buffer
          write-to-file
          update-changed-disk-date
          changed-disk-p))

(defvar *find-file-hook* '())
(defvar *before-save-hook* '())
(defvar *after-save-hook* '())

(defvar *external-format-function* nil)
(defvar *find-directory-function* nil)

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

(defun expand-file-name (pathname &optional (directory (uiop:getcwd)))
  (format nil "/~{~A~^/~}"
          (parse-pathname
           (if (and (plusp (length pathname))
                    (char/= #\/ (aref pathname 0)))
               (namestring (merge-pathnames pathname directory))
               pathname))))

(defun insert-file-contents (point filename)
  (let ((external-format :utf-8)
        (end-of-line :lf))
    (when *external-format-function*
      (multiple-value-setq (external-format end-of-line)
                           (funcall *external-format-function* filename)))
    (with-point ((point point :left-inserting))
      (with-open-file (in filename :external-format external-format)
        (loop
          (multiple-value-bind (str eof-p)
              (read-line in nil)
            (cond
              (eof-p
               (when str
                 (insert-string point str))
               (return))
              (t
               (let ((end nil))
                 #+sbcl
                 (when (and (eq end-of-line :crlf)
                            (< 0 (length str)))
                   (setf end (1- (length str))))
                 (insert-string point
                                (if end
                                    (subseq str 0 end)
                                    str))
                 (insert-character point #\newline))))))))))

(defun find-file-buffer (filename)
  (when (pathnamep filename)
    (setf filename (namestring filename)))
  (setf filename (expand-file-name filename))
  (cond ((uiop:directory-pathname-p filename)
         (if *find-directory-function*
             (funcall *find-directory-function* filename)
             (editor-error "~A is a directory" filename)))
        ((find filename (buffer-list) :key #'buffer-filename :test #'equal))
        (t
         (let* ((name (file-namestring filename))
                (buffer (make-buffer (if (get-buffer name)
                                         (uniq-buffer-name name)
                                         name)
                                     :filename filename
                                     :enable-undo-p nil)))
           (when (probe-file filename)
             (let ((*inhibit-modification-hooks* t))
               (insert-file-contents (buffer-start-point buffer)
                                     filename))
             (buffer-unmark buffer))
           (buffer-start (buffer-point buffer))
           (buffer-enable-undo buffer)
           (update-changed-disk-date buffer)
           (run-hooks *find-file-hook* buffer)
           (values buffer t)))))

(defun write-to-file-1 (buffer filename)
  (flet ((f (out end-of-line)
           (with-point ((point (buffer-start-point buffer)))
             (loop :for eof-p := (end-buffer-p point)
                   :for str := (line-string point)
                   :do
                   (princ str out)
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
                     (princ #\newline out)
                     )
                   (unless (line-offset point 1)
                     (return))))))
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
         (f out :lf))))))

(defun write-to-file (buffer filename)
  (run-hooks *before-save-hook* buffer)
  (write-to-file-1 buffer filename)
  (buffer-unmark buffer)
  (update-changed-disk-date buffer)
  (run-hooks *after-save-hook* buffer))

(defun file-write-date* (buffer)
  (if (probe-file (buffer-filename buffer))
      (file-write-date (buffer-filename buffer))))

(defun update-changed-disk-date (buffer)
  (setf (buffer-last-write-date buffer)
        (file-write-date* buffer)))

(defun changed-disk-p (buffer)
  (and (buffer-filename buffer)
       (uiop:file-exists-p (buffer-filename buffer))
       (not (eql (buffer-last-write-date buffer)
                 (file-write-date* buffer)))))
