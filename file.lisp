(in-package :lem)

(export '(*find-directory-function*
          expand-file-name
          insert-file-contents
          get-file-buffer
          write-to-file
          changed-disk-p))

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
    (update-changed-disk-date buffer)
    buffer))

(defun get-file-buffer (filename)
  (setf filename (expand-file-name filename))
  (cond ((uiop:directory-pathname-p filename)
         (if *find-directory-function*
             (funcall *find-directory-function* filename)
             (editor-error "~A is a directory" filename)))
        ((find filename (buffer-list) :key #'buffer-filename :test #'equal))
        (t
         (let* ((name (file-namestring filename))
                (buffer (file-open-create-buffer (if (get-buffer name)
                                                     (uniq-buffer-name name)
                                                     name)
                                                 filename)))
           (values buffer t)))))

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
  (buffer-unmark buffer)
  (update-changed-disk-date buffer))

(defun file-write-date* (buffer)
  (if (probe-file (buffer-filename buffer))
      (file-write-date (buffer-filename buffer))))

(defun update-changed-disk-date (buffer)
  (setf (buffer-last-write-date buffer)
        (file-write-date* buffer)))

(defun changed-disk-p (buffer)
  (and (buffer-filename buffer)
       (not (eql (buffer-last-write-date buffer)
                 (file-write-date* buffer)))))
